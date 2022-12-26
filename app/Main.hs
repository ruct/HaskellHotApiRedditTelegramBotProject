module Main (main) where

import BotCore
import qualified Data.Text as Text
import Telegram.Bot.API

main :: IO ()
main = do
  let tokenPath = "token.txt"
  token <- Token . Text.pack <$> readFile tokenPath
  putStrLn "running bot..."
  runBot token