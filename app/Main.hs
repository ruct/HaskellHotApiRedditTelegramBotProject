module Main (main) where

import BotCore
import qualified Data.Text as Text
import Telegram.Bot.API (Token (Token))

main :: IO ()
main = do
  let tokensPath = "token.txt"
  contents <- readFile tokensPath
  let ls = lines contents
  let botTokenStr = head ls
  let redditToken = head $ tail ls
  let botToken = Token (Text.pack botTokenStr)
  putStrLn "running bot..."
  runBot botToken redditToken
