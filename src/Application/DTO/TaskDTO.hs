{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Application.DTO.TaskDTO
  ( TaskDTO(..)
  , TodoEventDTO(..)
  , TasksStatisticsDTO(..)
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
    [ "taskId" .= taskDtoId dto
    , "desc" .= taskDtoDesc dto
    , "isCompleted" .= taskDtoIsCompleted dto
    ]

instance FromJSON TaskDTO where
  parseJSON = withObject "TaskDTO" $ \v -> TaskDTO
    <$> v .: "taskId"
    <*> v .: "desc"
    <*> v .: "isCompleted"

-- Event DTOs
data TodoEventDTO
  = TaskInitiatedDTO String String UTCTime
  | TaskCompletedDTO String UTCTime
  | TaskReopenedDTO String UTCTime
  | TaskDeletedDTO String UTCTime
  deriving (Show, Eq, Generic)

instance ToJSON TodoEventDTO where
  toJSON (TaskInitiatedDTO id' desc ts) =
    object
      [ "type" .= ("TaskInitiated" :: Text)
      , "id" .= id'
      , "desc" .= desc
      , "timestamp" .= ts
      ]
  toJSON (TaskCompletedDTO id' ts) =
    object
      [ "type" .= ("TaskCompleted" :: Text)
      , "id" .= id'
      , "timestamp" .= ts
      ]
  toJSON (TaskReopenedDTO id' ts) =
    object
      [ "type" .= ("TaskReopened" :: Text)
      , "id" .= id'
      , "timestamp" .= ts
      ]
  toJSON (TaskDeletedDTO id' ts) =
    object
      [ "type" .= ("TaskDeleted" :: Text)
      , "id" .= id'
      , "timestamp" .= ts
      ]

-- StatisticsDTO
data TasksStatisticsDTO = TasksStatisticsDTO
  { totalCount :: Int
  , activeCount :: Int
  , isCompletedCount :: Int
  } deriving (Show, Eq, Generic)

instance ToJSON TasksStatisticsDTO
