{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import Lib

import qualified Network.HTTP.Client     as H
import qualified Network.HTTP.Client.TLS as H
import OpenSSL
import qualified Network.Http.Client    as H2
import qualified System.IO.Streams as Streams
import qualified Data.ByteString as S

main :: IO ()
main = withOpenSSL $ do
  x <- H2.withConnection (H2.openConnection "s3.example.com" 80) $ (\c -> do
    let q = H2.buildRequest1 $ do
              H2.http H2.POST "/api/v1/messages"
              H2.setContentType "application/json"
              H2.setHostname "clue.example.com" 80
              H2.setAccept "text/html"
              H2.setHeader "X-WhoDoneIt" "The Butler"
    H2.sendRequest c q H2.emptyBody

    H2.receiveResponse c (\p i -> do
        xm <- Streams.read i
        case xm of
            Just x    -> S.putStr x
            Nothing   -> "")

    return "blah")
  print x
