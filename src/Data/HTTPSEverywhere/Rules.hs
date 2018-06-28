module Data.HTTPSEverywhere.Rules (
  RuleSet,
  getRulesets,
  rewriteURL,
  rewriteCookie
) where

import Prelude hiding (null, head)
import Data.Bool (bool)
import Network.HTTP.Client (Cookie)
import Network.URI (URI)
import Pipes ((>->))
import Pipes.Prelude (head, null)
import Control.Monad.Identity (Identity(runIdentity))
import Data.HTTPSEverywhere.Rules.Internal (getRulesets, getRulesetsMatching, havingRulesThatTrigger, havingCookieRulesThatTrigger, setSecureFlag, RuleSet)

rewriteURL' :: Monad m => [RuleSet] -> URI -> m (Maybe URI)
rewriteURL' rs url = head $ getRulesetsMatching rs url >-> havingRulesThatTrigger url

rewriteURL :: [RuleSet] -> (URI -> Maybe URI)
rewriteURL = fmap runIdentity <$> rewriteURL'

rewriteCookie' :: Monad m => [RuleSet] -> URI -> Cookie -> m (Maybe Cookie)
rewriteCookie' rs url cookie = Just (setSecureFlag cookie) `bool` Nothing <$> null producer
  where producer = getRulesetsMatching rs url >-> havingCookieRulesThatTrigger cookie

rewriteCookie :: [RuleSet] -> URI -> (Cookie -> Maybe Cookie)
rewriteCookie = fmap (fmap runIdentity) <$> rewriteCookie'
