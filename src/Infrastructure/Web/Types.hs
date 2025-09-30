{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Infrastructure.Web.Types
  ( InitiateTaskRequest(..)
  , InitiateTaskResponse(..)
  , TasksResponse(..)
  , ToggleRequest(..)
  , DeleteRequest(..)
  , TodoAPI
  ) where

import Application.DTO.TaskDTO

import Data.Aeson
import GHC.Generics
import Servant

newtype InitiateTaskRequest = InitiateTaskRequest
  { requestText :: String
  } deriving (Generic)

instance FromJSON InitiateTaskRequest

newtype InitiateTaskResponse = InitiateTaskResponse
  { createId :: String
  } deriving (Generic)

instance ToJSON InitiateTaskResponse

data TasksResponse = TasksResponse
  { tasks :: [TaskDTO]
  , statistics :: TasksStatisticsDTO
  } deriving (Generic)

instance ToJSON TasksResponse

newtype ToggleRequest = ToggleRequest
  { toggleId :: String
  } deriving (Generic)

instance FromJSON ToggleRequest

newtype DeleteRequest = DeleteRequest
  { deleteId :: String
  } deriving (Generic)

instance FromJSON DeleteRequest

type TodoAPI =
  "api" :> "tasks" :> Get '[JSON] TasksResponse
  :<|> "api" :> "tasks" :> ReqBody '[JSON] InitiateTaskRequest :> Post '[JSON] InitiateTaskResponse
  :<|> "api" :> "tasks" :> "toggle" :> ReqBody '[JSON] ToggleRequest :> Post '[JSON] NoContent
  :<|> "api" :> "tasks" :> "delete" :> ReqBody '[JSON] DeleteRequest :> Post '[JSON] NoContent
  :<|> "api" :> "events" :> Get '[JSON] [TodoEventDTO]


