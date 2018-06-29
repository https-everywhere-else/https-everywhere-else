{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Data.HTTPSEverywhere.Rules.Internal.Types (
  RuleSet(..),
  Target(..),
  Rule(..),
  Exclusion(..),
  CookieRule(..),
  fromSerialisable
) where

import Prelude hiding (take, drop)
import Control.Monad (join)
import Control.Monad.State (State, evalState, modify', gets)
import Data.Bool (bool)
import Data.Bifunctor (bimap)
import Data.Maybe (catMaybes)
import Data.Text (Text, take, takeEnd, drop, dropEnd, dropWhileEnd, isSuffixOf, pack, unpack)
import Data.Text.Encoding (decodeUtf8)
import Network.HTTP.Client (Cookie(..))
import Network.URI (URI, uriRegName, uriAuthority, parseURI)

import qualified Data.HTTPSEverywhere.Rules.Internal.Types.Serialisable
  as Serialisable
import Data.Text.ICU.Extras (match, findAndReplace)

newtype Rule       = Rule       { getRule :: URI -> Maybe URI     }
newtype Target     = Target     { getTarget :: URI  -> Bool       }
newtype Exclusion  = Exclusion  { getExclusion :: URI -> Bool     }
newtype CookieRule = CookieRule { getCookieRule :: Cookie -> Bool }

data RuleSet = RuleSet
  { ruleSetName        :: Text
  , ruleSetTargets     :: [Target]
  , ruleSetRules       :: [Rule]
  , ruleSetExclusions  :: [Exclusion]
  , ruleSetCookieRules :: [CookieRule]
  }

instance Show RuleSet where
  show r = "<RuleSet " ++ unpack (ruleSetName r) ++ ">"

parseTarget' :: State (Text, Text) Bool
parseTarget' = do
  gets ((== ".*") . takeEnd 2 . fst) >>= bool (return ())
    (modify' (bimap (dropEnd 2) (dropEnd 1 . dropWhileEnd (/= '.'))))
  gets (take 2 . fst) >>= \case
    "*." -> do
      suffix <- gets (drop 1 . fst) -- Don't drop the dot, we expect
                                    -- some prefix.
      isSuffixOf suffix <$> gets snd
    _ -> gets (uncurry (==))

parseTarget :: Text -> Target
parseTarget target = Target $ \uri -> do
  case uriRegName <$> uriAuthority uri of
    Just domain -> evalState parseTarget' (target, pack domain)
    Nothing -> False

parseExclusion :: Text -> Maybe Exclusion
parseExclusion = fmap (Exclusion . (. pack . show)) `fmap` match

fromSerialisable :: Serialisable.RuleSet -> RuleSet
fromSerialisable Serialisable.RuleSet{..} = RuleSet
  { ruleSetName        = ruleSetName
  , ruleSetTargets     =
      map (parseTarget . Serialisable.targetHost) ruleSetTargets
  , ruleSetRules = catMaybes . flip map ruleSetRules $
      \Serialisable.Rule{..} -> do
        substitute <- findAndReplace ruleFrom ruleTo
        return . Rule $
          join . fmap (parseURI . unpack) . substitute . pack . show
  , ruleSetExclusions  = catMaybes $ map
      (parseExclusion . Serialisable.exclusionPattern)
      ruleSetExclusions
  , ruleSetCookieRules = catMaybes . flip map ruleSetCookieRules $
      \Serialisable.CookieRule{..} -> do
        hostMatches <- match cookieRuleHost
        nameMatches <- match cookieRuleName
        return . CookieRule $ \Cookie{..} ->
          nameMatches (decodeUtf8 cookie_name) &&
          hostMatches (decodeUtf8 cookie_domain)
  }
