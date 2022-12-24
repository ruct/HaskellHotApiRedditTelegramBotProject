{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import Lib

import OpenSSL.Session (context)
import Network.HTTP.Client.OpenSSL
-- import Network.HTTP.Client.Types
import Network.HTTP.Client (defaultManagerSettings, managerResponseTimeout)
import Network.Wreq
import Control.Lens



main :: IO ()
main = do
  let opts = defaults & manager .~ Left (opensslManagerSettings context)
  print "kek"
  let opts = defaults & manager .~ Left (opensslManagerSettings context)
  withOpenSSL $ do
    r <- getWith opts "https://httpbin.org/get"
    print r
