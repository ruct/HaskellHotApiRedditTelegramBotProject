{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module RedditJson
  ( parseRedditJson,
    Model,
    modelData,
    Data,
    dataChildren,
    Children,
    childrenName,
  )
where

import Data.Aeson (FromJSON (..), ToJSON (..), Value (..), decode, object, (.:), (.=))
import Data.Aeson.Types (prependFailure, typeMismatch)
import Data.ByteString.Lazy.UTF8 as BLU
import Data.Text (Text)

data Children = Children
  { childrenDate :: Int,
    childrenName :: Text,
    childrenId :: Text,
    childrenRelId :: Text
  }
  deriving (Show, Eq, Ord)

data Data = Data
  { dataChildren :: [Children]
  }
  deriving (Show, Eq, Ord)

data Model = Model
  { modelKind :: Text,
    modelData :: Data
  }
  deriving (Show, Eq, Ord)

instance ToJSON Children where
  toJSON Children {..} =
    object
      [ "date" .= childrenDate,
        "name" .= childrenName,
        "id" .= childrenId,
        "rel_id" .= childrenRelId
      ]

instance ToJSON Data where
  toJSON Data {..} =
    object
      [ "children" .= dataChildren
      ]

instance ToJSON Model where
  toJSON Model {..} =
    object
      [ "kind" .= modelKind,
        "data" .= modelData
      ]

instance FromJSON Children where
  parseJSON (Object v) = do
    childrenDate <- v .: "date"
    childrenName <- v .: "name"
    childrenId <- v .: "id"
    childrenRelId <- v .: "rel_id"
    pure $ Children {..}
  parseJSON invalid = do
    prependFailure
      "parsing Children failed, "
      (typeMismatch "Object" invalid)

instance FromJSON Data where
  parseJSON (Object v) = do
    dataChildren <- v .: "children"
    pure $ Data {..}
  parseJSON invalid = do
    prependFailure
      "parsing Data failed, "
      (typeMismatch "Object" invalid)

instance FromJSON Model where
  parseJSON (Object v) = do
    modelKind <- v .: "kind"
    modelData <- v .: "data"
    pure $ Model {..}
  parseJSON invalid = do
    prependFailure
      "parsing Model failed, "
      (typeMismatch "Object" invalid)

parseRedditJson :: BLU.ByteString -> Maybe Model
parseRedditJson str = Data.Aeson.decode str :: Maybe Model
