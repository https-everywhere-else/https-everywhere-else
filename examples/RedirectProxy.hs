{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PackageImports    #-}
{-# LANGUAGE RecordWildCards   #-}

module Main (main) where

import "base" Data.Monoid ((<>))
import "bytestring" Data.ByteString.Char8 (pack, unpack)
import "http-proxy" Network.HTTP.Proxy (runProxySettings, defaultProxySettings, Settings(..), Request(..))
import "http-types" Network.HTTP.Types (status302)
import "https-everywhere-rules" Data.HTTPSEverywhere.Rules (getRulesets, rewriteURL, RuleSet)
import "network-uri" Network.URI (URI, parseURI)
import "wai" Network.Wai (Response, responseLBS)

(<&>) :: Functor f => f a -> (a -> b) -> f b
(<&>) = flip fmap; infixl 1 <&>

httpRedirect :: URI -> Response
httpRedirect to = responseLBS status302 [("Location", pack . show $ to)] ""

tryHTTPS :: [RuleSet] -> Request -> IO (Either Response Request)
tryHTTPS rs Request{..} = case parseURI . unpack $ requestPath <> queryString of
  Just uri -> rewriteURL rs uri <&> \rewrite -> if rewrite == uri
    then return Request{..}
    else Left $ httpRedirect rewrite
  Nothing -> return $ Right Request{..}

main :: IO ()
main = do
  rulesets <- getRulesets
  runProxySettings $ defaultProxySettings { proxyRequestModifier = tryHTTPS rulesets }
