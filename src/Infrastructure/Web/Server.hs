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
    :<|> createTodoHandler
    :<|> toggleTodoHandler
    :<|> deleteTodoHandler
    :<|> getEvents
  where
    service = mkTodoService domainOps :: TodoService AppM

    getTodos :: Handler TodosResponse
    getTodos = runAppM config $ do
      ts <- getAllTodosImpl service
      stats <- getStatisticsImpl service
      return $ TodosResponse ts stats

    createTodoHandler :: CreateTodoRequest -> Handler CreateTodoResponse
    createTodoHandler req = runAppM config $ do
      todoId' <- createTodoImpl service (requestText req)
      return $ CreateTodoResponse todoId'

    toggleTodoHandler :: ToggleRequest -> Handler NoContent
    toggleTodoHandler req = runAppM config $ do
      _ <- toggleTodoImpl service (toggleId req)
      return NoContent

    deleteTodoHandler :: DeleteRequest -> Handler NoContent
    deleteTodoHandler req = runAppM config $ do
      _ <- deleteTodoImpl service (deleteId req)
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
