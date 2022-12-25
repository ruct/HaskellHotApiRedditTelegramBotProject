{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import Lib
import System.Environment
import Data.ByteString.UTF8 as BSU

main :: IO ()
main = do
  token <- getEnv "REDDIT_ACCESS_TOKEN"
  kek <- redditHotApiRequest (BSU.fromString token)
  print kek

