{-# LANGUAGE OverloadedStrings #-}
module Lib
    ( redditHotApiRequest
    ) where

import Network.HTTP.Client.OpenSSL
import OpenSSL.Session (context)
-- import Data.ByteString.UTF8 as BSU
import Data.ByteString.Lazy.UTF8 as BLU
import Data.ByteString as DB
import Network.Wreq
import Control.Lens

redditHotApiRequest :: DB.ByteString -> IO BLU.ByteString
redditHotApiRequest token = do
  let opts = defaults & manager .~ Left (opensslManagerSettings context)
                      & header "Authorization" .~ [DB.concat ["bearer", token]]
                      & header "User-Agent" .~ ["APP-NAME by REDDIT-USERNAME"]
  r <- withOpenSSL $
    getWith opts "https://oauth.reddit.com/hot"
  return $ r ^. responseBody
