{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Application.DTO.TaskDTO
  ( TaskDTO(..)
  , TodoEventDTO(..)
  , TodoStatisticsDTO(..)
  ) where

import Data.Aeson
import Data.Text (Text)
import Data.Time
import GHC.Generics

-- DTOs for external communication
data TaskDTO = TaskDTO
  { taskDtoId :: String
  , taskDtoDesc :: String
  , taskDtoIsCompleted :: Bool
  } deriving (Show, Eq, Generic)

instance ToJSON TaskDTO where
  toJSON dto = object
    [ "todoId" .= taskDtoId dto
    , "text" .= taskDtoDesc dto
    , "completed" .= taskDtoIsCompleted dto
    ]

instance FromJSON TaskDTO where
  parseJSON = withObject "TaskDTO" $ \v -> TaskDTO
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
      [ "type" .= ("TodoUncompleted" :: Text)
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
