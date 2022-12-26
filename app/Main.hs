module Main (main) where

import BotCore
import qualified Data.Text as Text
import Telegram.Bot.API

main :: IO ()
main = do
  let tokensPath = "token.txt"
  contents <- readFile tokensPath
  let (botTokenStr:redditToken:_) = lines contents
  let botToken = Token (Text.pack (botTokenStr))
  putStrLn "running bot..."
  runBot botToken redditToken
