{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import Lib

import qualified Network.HTTP.Client     as H
import qualified Network.HTTP.Client.TLS as H
import Network.HTTP.Client.OpenSSL
import OpenSSL.Session (context)
import Network.Wreq
import Control.Lens


main :: IO ()
main = do
  let opts = defaults & manager .~ Left (opensslManagerSettings context)
                      & header "Authorisation" .~ ["AUTH_INFO_STRING"] 
  r <- withOpenSSL $
    getWith opts "https://httpbin.org/get"
  print r
  print "kek"

