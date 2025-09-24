{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Infrastructure.Web.Types
  ( CreateTodoRequest(..)
  , CreateTodoResponse(..)
  , TodosResponse(..)
  , ToggleRequest(..)
  , DeleteRequest(..)
  , TodoAPI
  ) where

import Application.DTO.TodoDTO

import Data.Aeson
import GHC.Generics
import Servant

newtype CreateTodoRequest = CreateTodoRequest
  { requestText :: String
  } deriving (Generic)

instance FromJSON CreateTodoRequest

newtype CreateTodoResponse = CreateTodoResponse
  { createId :: String
  } deriving (Generic)

instance ToJSON CreateTodoResponse

data TodosResponse = TodosResponse
  { todos :: [TodoDTO]
  , statistics :: TodoStatisticsDTO
  } deriving (Generic)

instance ToJSON TodosResponse

newtype ToggleRequest = ToggleRequest
  { toggleId :: String
  } deriving (Generic)

instance FromJSON ToggleRequest

newtype DeleteRequest = DeleteRequest
  { deleteId :: String
  } deriving (Generic)

instance FromJSON DeleteRequest

type TodoAPI =
  "api" :> "todos" :> Get '[JSON] TodosResponse
  :<|> "api" :> "todos" :> ReqBody '[JSON] CreateTodoRequest :> Post '[JSON] CreateTodoResponse
  :<|> "api" :> "todos" :> "toggle" :> ReqBody '[JSON] ToggleRequest :> Post '[JSON] NoContent
  :<|> "api" :> "todos" :> "delete" :> ReqBody '[JSON] DeleteRequest :> Post '[JSON] NoContent
  :<|> "api" :> "events" :> Get '[JSON] [TodoEventDTO]


