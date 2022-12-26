{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

module BotCore
  ( runBot,
  )
where

import Common.FlipCoin
import Control.Applicative
import Control.Monad.IO.Class
import Data.Text
import Telegram.Bot.API
import Telegram.Bot.Simple
import Telegram.Bot.Simple.UpdateParser

runBot :: Token -> IO ()
runBot token = do
  env <- defaultTelegramClientEnv token
  startBot_ redditBot env

type Model = ()

data Action
  = Start
  | Coin
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
      <|> callbackQueryDataRead

startMessage :: Text
startMessage =
  Data.Text.unlines
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
      resp <- liftIO $ pack . show <$> flipCoin
      replyText resp