{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Application.DTO.TodoDTO
  ( TodoDTO(..)
  , TodoEventDTO(..)
  , TodoStatisticsDTO(..)
  ) where

import Data.Aeson
import Data.Text (Text)
import Data.Time
import GHC.Generics

-- DTOs for external communication
data TodoDTO = TodoDTO
  { todoDtoId :: String
  , todoDtoText :: String
  , todoDtoCompleted :: Bool
  } deriving (Show, Eq, Generic)

instance ToJSON TodoDTO where
  toJSON dto = object
    [ "todoId" .= todoDtoId dto
    , "text" .= todoDtoText dto
    , "completed" .= todoDtoCompleted dto
    ]

instance FromJSON TodoDTO where
  parseJSON = withObject "TodoDTO" $ \v -> TodoDTO
    <$> v .: "todoId"
    <*> v .: "text"
    <*> v .: "completed"

-- Event DTOs
data TodoEventDTO
  = TodoCreatedDTO String String UTCTime
  | TodoCompletedDTO String UTCTime
  | TodoUncompletedDTO String UTCTime
  | TodoDeletedDTO String UTCTime
  deriving (Show, Eq, Generic)

instance ToJSON TodoEventDTO where
  toJSON (TodoCreatedDTO id' txt ts) =
    object
      [ "type" .= ("TodoCreated" :: Text)
      , "id" .= id'
      , "text" .= txt
      , "timestamp" .= ts
      ]
  toJSON (TodoCompletedDTO id' ts) =
    object
      [ "type" .= ("TodoCompleted" :: Text)
      , "id" .= id'
      , "timestamp" .= ts
      ]
  toJSON (TodoUncompletedDTO id' ts) =
    object
      [ "type" .= ("TodoUnCompleted" :: Text)
      , "id" .= id'
      , "timestamp" .= ts
      ]
  toJSON (TodoDeletedDTO id' ts) =
    object
      [ "type" .= ("TodoDeleted" :: Text)
      , "id" .= id'
      , "timestamp" .= ts
      ]

-- StatisticsDTO
data TodoStatisticsDTO = TodoStatisticsDTO
  { totalCount :: Int
  , activeCount :: Int
  , completedCount :: Int
  } deriving (Show, Eq, Generic)

instance ToJSON TodoStatisticsDTO
