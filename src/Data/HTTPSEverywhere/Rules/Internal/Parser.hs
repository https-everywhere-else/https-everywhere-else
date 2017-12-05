{-# LANGUAGE CPP                 #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE LambdaCase          #-}

module Data.HTTPSEverywhere.Rules.Internal.Parser (
  parseRuleSets,
#ifdef TEST
  parseTarget
#endif
) where

import Prelude hiding (take, drop)
import Control.Applicative ((<$>))
import Control.Lens (toListOf, only, to, (^..), (^.), (&), (<&>), _Just)
import Control.Monad (join)
import Control.Monad.State (State, evalState, modify', gets)
import Data.Bifunctor (bimap)
import Data.Bool (bool)
import Data.Functor.Infix ((<$$>))
import Data.Maybe (catMaybes)
import Data.String.Conversions (cs)
import qualified Data.Text as Strict (Text)
import Data.Text (dropEnd, dropWhileEnd, drop, isSuffixOf, takeEnd, take)
import qualified Data.Text.Lazy as Lazy (Text)
import Network.HTTP.Client (Cookie(..))
import Network.URI (URI(uriAuthority), URIAuth(uriRegName), parseURI)
import Text.Taggy.Lens (html, allNamed, attr, Element)

import Data.HTTPSEverywhere.Rules.Internal.Types (RuleSet(..), Target(..), Rule(..), Exclusion(..), CookieRule(..))
import Data.Text.ICU.Extras (match, findAndReplace)

parseRuleSets :: Lazy.Text -> [RuleSet]
parseRuleSets = catMaybes <$$> toListOf $ html . allNamed (only "ruleset") . to parseRuleSet

parseRuleSet :: Element -> Maybe RuleSet
parseRuleSet xml = xml ^. attr "name" <&> \ruleSetName -> do
  let ruleSetTargets     = xml ^.. allNamed (only "target") . attr "host" . _Just . to parseTarget
      ruleSetRules       = xml ^.. allNamed (only "rule") . to parseRule & catMaybes
      ruleSetExclusions  = xml ^.. allNamed (only "exclusion") . attr "pattern" . _Just . to parseExclusion & catMaybes
      ruleSetCookieRules = xml ^.. allNamed (only "securecookie") . to parseCookieRule & catMaybes
  RuleSet ruleSetName ruleSetTargets ruleSetRules ruleSetExclusions ruleSetCookieRules

parseTarget' :: State (Strict.Text, Strict.Text) Bool
parseTarget' = do
  gets ((== ".*") . takeEnd 2 . fst) >>= bool (return ())
    (modify' (bimap (dropEnd 2) (dropEnd 1 . dropWhileEnd (/= '.'))))
  gets (take 2 . fst) >>= \case
    "*." -> do
      suffix <- gets (drop 1 . fst) -- Don't drop the dot, we expect some prefix.
      isSuffixOf suffix <$> gets snd
    _ -> gets (uncurry (==))

parseTarget :: Strict.Text -> Target
parseTarget target = Target $ \uri -> do
  case uriRegName <$> uriAuthority uri of
    Just domain -> evalState parseTarget' (target, cs domain)
    Nothing -> False

parseRule :: Element -> Maybe Rule
parseRule element = do
  pattern     <- element ^. attr "from"
  replacement <- element ^. attr "to"
  substitute  <- findAndReplace pattern replacement 
  return . Rule $ join . fmap (parseURI . cs) . substitute . cs . show

parseExclusion :: Strict.Text -> Maybe Exclusion
parseExclusion = Exclusion . (. cs . show) <$$> match

parseCookieRule :: Element -> Maybe CookieRule
parseCookieRule element = CookieRule <$> do
  hostMatches <- element ^. attr "host" . to (>>= match)
  nameMatches <- element ^. attr "name" . to (>>= match)
  return $ \Cookie{..} -> nameMatches (cs cookie_name) && hostMatches (cs cookie_domain)
