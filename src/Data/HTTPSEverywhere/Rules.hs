module Data.HTTPSEverywhere.Rules (
  RuleSet,
  getRulesets,
  rewriteURL,
  rewriteCookie
) where

import Prelude hiding (null, head)
import Control.Lens ((<&>),(&))
import Data.Bool (bool)
import Data.Functor.Infix ((<$$>), (<$$$>))
import Data.Maybe (fromMaybe)
import Network.HTTP.Client (Cookie)
import Network.URI (URI)
import Pipes ((>->))
import Pipes.Prelude (head, null)
import Control.Monad (join)
import Control.Monad.Identity (Identity(runIdentity))
import Data.HTTPSEverywhere.Rules.Internal (getRulesets, getRulesetsMatching, havingRulesThatTrigger, havingCookieRulesThatTrigger, setSecureFlag, RuleSet)

rewriteURL' :: Monad m => [RuleSet] -> URI -> m URI
rewriteURL' rs url = getRulesetsMatching rs url >-> havingRulesThatTrigger url & head <&> fromMaybe url . join

rewriteURL :: [RuleSet] -> URI -> URI
rewriteURL = runIdentity <$$> rewriteURL'

rewriteCookie' :: Monad m => [RuleSet] -> URI -> Cookie -> m Cookie
rewriteCookie' rs url cookie = null producer <&> setSecureFlag cookie `bool` cookie
  where producer = getRulesetsMatching rs url >-> havingCookieRulesThatTrigger cookie

rewriteCookie :: [RuleSet] -> URI -> Cookie -> Cookie
rewriteCookie = runIdentity <$$$> rewriteCookie'
