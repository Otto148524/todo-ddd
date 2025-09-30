{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}

module Infrastructure.Web.Server
  ( todoServer
  , app
  ) where

import Application.DTO.Facade (domainOps)
import Application.DTO.TaskDTO
import Application.TodoService

import Infrastructure.Web.Types
import Infrastructure.EventStore.InMemory

import Network.Wai
import Network.Wai.Middleware.Cors
import Servant

todoServer :: AppConfig -> Server TodoAPI
todoServer config =
  getTodos
    :<|> initiateTaskHandler
    :<|> toggleTaskHandler
    :<|> deleteTaskHandler
    :<|> getEvents
  where
    service = mkTodoService domainOps :: TodoService AppM

    getTodos :: Handler TasksResponse
    getTodos = runAppM config $ do
      ts <- getAllTasksImpl service
      stats <- getStatisticsImpl service
      return $ TasksResponse ts stats

    initiateTaskHandler :: InitiateTaskRequest -> Handler InitiateTaskResponse
    initiateTaskHandler req = runAppM config $ do
      taskId' <- initiateTaskImpl service (requestText req)
      return $ InitiateTaskResponse taskId'

    toggleTaskHandler :: ToggleRequest -> Handler NoContent
    toggleTaskHandler req = runAppM config $ do
      _ <- toggleTaskImpl service (toggleId req)
      return NoContent

    deleteTaskHandler :: DeleteRequest -> Handler NoContent
    deleteTaskHandler req = runAppM config $ do
      _ <- deleteTaskImpl service (deleteId req)
      return NoContent

    getEvents :: Handler [TodoEventDTO]
    getEvents = runAppM config $ getEventHistoryImpl service

app :: AppConfig -> Application
app config = cors corsPolicy $ serve (Proxy :: Proxy TodoAPI) (todoServer config)
  where
    corsPolicy =
      const $
        Just
          CorsResourcePolicy
            { corsOrigins = Nothing
            , corsMethods = ["GET", "POST", "PUT", "DELETE", "OPTIONS"]
            , corsRequestHeaders = ["Content-Type"]
            , corsExposedHeaders = Nothing
            , corsMaxAge = Nothing
            , corsVaryOrigin = False
            , corsRequireOrigin = False
            , corsIgnoreFailures = False
            }
