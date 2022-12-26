{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# LANGUAGE PackageImports #-}

module BotCore
  ( runBot,
  )
where

import Common.FlipCoin
import Control.Applicative
import Control.Monad.IO.Class

import Data.Text as TS
import Data.Text.Lazy.Encoding as TLE
import Data.Text.Lazy as TL

import Telegram.Bot.API
import Telegram.Bot.Simple
import Telegram.Bot.Simple.UpdateParser

import Data.IORef
import System.IO.Unsafe (unsafePerformIO)

import "hot-reddit" Lib (redditHotApiRequest)

redditTokenRef :: IORef String
redditTokenRef =
  unsafePerformIO (newIORef "")

runBot :: Token -> String -> IO ()
runBot botToken redditToken = do
  modifyIORef' redditTokenRef (\_ -> redditToken)
  env <- defaultTelegramClientEnv botToken
  startBot_ redditBot env

type Model = ()

data Action
  = Start
  | Coin
  | Friends
  deriving (Show, Read)

redditBot :: BotApp Model Action
redditBot =
  BotApp
    { botInitialModel = (),
      botAction = flip updateToAction,
      botHandler = handleAction,
      botJobs = []
    }

updateToAction :: Model -> Update -> Maybe Action
updateToAction _ =
  parseUpdate $
    Start <$ command "start"
      <|> Coin <$ command "coin"
      <|> Friends <$ command "friends"
      <|> callbackQueryDataRead

startMessage :: TS.Text
startMessage =
  TS.unlines
    [ "Was wollen wir trinken? - Sieben Tage lang",
      "Was wollen wir trinken? - So ein Durst",
      "Was wollen wir trinken? - Sieben Tage lang",
      "Was wollen wir trinken? - So ein Durst",
      "",
      "Es wird genug für alle sein",
      "Wir trinken zusammen - Roll das Fass mal rein",
      "Wir trinken zusammen - Nicht allein",
      "",
      "Es wird genug für alle sein",
      "Wir trinken zusammen - Roll das Fass mal rein",
      "Wir trinken zusammen - Nicht allein",
      "",
      "/start -- show start message",
      "/coin -- flip a coin (0/1)"
    ]

handleAction :: Action -> Model -> Eff Action Model
handleAction action model = case action of
  Start ->
    model <# do
      replyText startMessage
  Coin ->
    model <# do
      resp <- liftIO $ TS.pack . show <$> flipCoin
      replyText resp
  Friends ->
    model <# do
      redditToken <- liftIO $ readIORef redditTokenRef
      res <- liftIO $ redditHotApiRequest redditToken
      replyText (TL.toStrict (TLE.decodeUtf8 res))
