module Data.HTTPSEverywhere.Rules (
  RuleTrie,
  rewriteURL,
  rewriteCookie
) where

import Network.URI (URI)
import Data.Monoid (First(..), Any(..))
import Data.Bool (bool)
import Network.HTTP.Client (Cookie(..))

import Data.HTTPSEverywhere.Rules.Internal

rewriteURL :: URI -> Maybe URI
rewriteURL uri = getFirst . mconcat $
  map (First . havingRulesThatTrigger uri) (getRulesetsMatching uri)

rewriteCookie :: URI -> (Cookie -> Maybe Cookie)
rewriteCookie uri cookie =
  bool Nothing (Just $ setSecureFlag cookie) . getAny . mconcat $
    map (Any . havingCookieRulesThatTrigger cookie)
      (getRulesetsMatching uri)
