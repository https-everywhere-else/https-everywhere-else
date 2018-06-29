{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PackageImports    #-}
{-# LANGUAGE RecordWildCards   #-}

module Main (main) where

import "base" Data.Maybe (maybe)
import "base" Data.Monoid ((<>))
import "bytestring" Data.ByteString.Char8 (pack, unpack)
import "http-proxy" Network.HTTP.Proxy
  (runProxySettings, defaultProxySettings, Settings(..), Request(..))
import "http-types" Network.HTTP.Types (status302)
import "https-everywhere-rules" Data.HTTPSEverywhere.Rules (rewriteURL)
import "network-uri" Network.URI (URI, parseURI)
import "wai" Network.Wai (Response, responseLBS)

httpRedirect :: URI -> Response
httpRedirect to =
  responseLBS status302 [("Location", pack . show $ to)] ""

tryHTTPS :: Request -> Either Response Request
tryHTTPS Request{..} =
  case parseURI . unpack $ requestPath <> queryString of
    Just uri -> maybe
      (return Request{..})
      (Left . httpRedirect)
      (rewriteURL uri)
    Nothing -> return Request{..}

main :: IO ()
main = runProxySettings $ defaultProxySettings
  { proxyRequestModifier = return . tryHTTPS }
