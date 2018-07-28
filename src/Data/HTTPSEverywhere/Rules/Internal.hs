{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RecordWildCards #-}
 
module Data.HTTPSEverywhere.Rules.Internal (
  RuleTrie,
  getRulesetsMatching,
  havingRulesThatTrigger,
  havingCookieRulesThatTrigger,
  setSecureFlag
) where

import Control.Monad (MonadPlus(..))
import Data.Text (pack)
import Data.Maybe (maybeToList)
import Data.Monoid (First(..))
import Network.HTTP.Client (Cookie(..))
import Network.URI (URI, uriAuthority, uriRegName)

import Data.HTTPSEverywhere.Rules.Internal.Trie (RuleTrie)
import qualified Data.HTTPSEverywhere.Rules.Internal.Trie as Trie
import Data.HTTPSEverywhere.Rules.Internal.Types
  (RuleSet(..), getRule, getExclusion, getCookieRule)
import qualified Data.HTTPSEverywhere.Rules.Internal.Trie.Supercompilation
  as Supercompilation (trie)

-- trie :: RuleTrie
-- trie = $(Supercompilation.trie)

unless :: MonadPlus m => Bool -> m a -> m a
unless True action = action
unless False _ = mzero

getRulesetsMatching :: URI -> [RuleSet]
getRulesetsMatching uri = do
  let hostname = fmap (pack . uriRegName) (uriAuthority uri)
  r@RuleSet{..} <- maybeToList hostname >>= (const [])
  unless (any (\e -> getExclusion e uri) ruleSetExclusions) $
    return r

havingRulesThatTrigger :: URI -> RuleSet -> Maybe URI
havingRulesThatTrigger uri RuleSet{..} =
  getFirst . mconcat $ map (First . ($ uri) . getRule) ruleSetRules

havingCookieRulesThatTrigger :: Cookie -> RuleSet -> Bool
havingCookieRulesThatTrigger cookie RuleSet{..} =
  any (($ cookie) . getCookieRule) ruleSetCookieRules

setSecureFlag :: Cookie -> Cookie
setSecureFlag cookie = cookie { cookie_secure_only = True }
