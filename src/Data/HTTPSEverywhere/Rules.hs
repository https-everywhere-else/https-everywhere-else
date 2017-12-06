module Data.HTTPSEverywhere.Rules (
  RuleSet,
  getRulesets,
  rewriteURL,
  rewriteCookie
) where

import Prelude hiding (null, head)
import Control.Lens ((<&>),(&))
import Data.Bool (bool)
import Data.Maybe (fromMaybe)
import Network.HTTP.Client (Cookie)
import Network.URI (URI)
import Pipes ((>->))
import Pipes.Prelude (head, null)
import Control.Monad (join)
import Data.HTTPSEverywhere.Rules.Internal (getRulesets, getRulesetsMatching, havingRulesThatTrigger, havingCookieRulesThatTrigger, setSecureFlag, RuleSet)

rewriteURL :: [RuleSet] -> URI -> IO URI
rewriteURL rs url = getRulesetsMatching rs url >-> havingRulesThatTrigger url & head <&> fromMaybe url . join

rewriteCookie :: [RuleSet] -> URI -> Cookie -> IO Cookie
rewriteCookie rs url cookie = null producer <&> setSecureFlag cookie `bool` cookie
  where producer = getRulesetsMatching rs url >-> havingCookieRulesThatTrigger cookie
