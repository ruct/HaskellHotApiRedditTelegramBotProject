{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PackageImports #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

module BotCore
  ( runBot,
  )
where

import Common.FlipCoin
import Control.Applicative
import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.IORef
import Data.Text as TS
import RedditJson
import RequestLib
import System.IO.Unsafe (unsafePerformIO)
import Telegram.Bot.API
import Telegram.Bot.Simple
import Telegram.Bot.Simple.UpdateParser

redditTokenRef :: IORef String
redditTokenRef =
  unsafePerformIO (newIORef "")

runBot :: Token -> String -> IO ()
runBot botToken redditToken = do
  modifyIORef' redditTokenRef (const redditToken)
  env <- defaultTelegramClientEnv botToken
  startBot_ redditBot env

type Model = ()

data Action
  = Start
  | Coin
  | Friends
  deriving (Show, Read)

redditBot :: BotApp BotCore.Model Action
redditBot =
  BotApp
    { botInitialModel = (),
      botAction = flip updateToAction,
      botHandler = handleAction,
      botJobs = []
    }

updateToAction :: BotCore.Model -> Update -> Maybe Action
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
      "/coin -- flip a coin (0/1)",
      "/friends -- show your reddit friends"
    ]

handleAction :: Action -> BotCore.Model -> Eff Action BotCore.Model
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
      let (Just resp) = parseRedditJson res
      let ch = dataChildren (modelData resp)
      replyText $ intercalate "\n" $ formatChildren ch

formatChildren :: [Children] -> [TS.Text]
formatChildren chs = [ TS.concat [(TS.pack "- "), (childrenName ch)] | ch <- chs]
