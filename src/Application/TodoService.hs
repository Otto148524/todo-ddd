{-# LANGUAGE OverloadedStrings #-}

module Application.TodoService
  ( TodoService(..)
  , mkTodoService
  ) where

import Application.DTO.Facade
import Application.DTO.TaskDTO
import Application.Ports.EventStorePort
import Application.Ports.NotificationPort


import Control.Monad.IO.Class
import qualified Data.Text as T
import Data.Time
import System.Random

data TodoService m = TodoService
  { createTodoImpl :: String -> m String
  , toggleTodoImpl :: String -> m Bool
  , deleteTodoImpl :: String -> m Bool
  , getAllTodosImpl :: m [TaskDTO]
  , getStatisticsImpl :: m TodoStatisticsDTO
  , getEventHistoryImpl :: m [TodoEventDTO]
  }

mkTodoService :: (EventStore m, NotificationPort m, MonadIO m) => DomainOperations -> TodoService m
mkTodoService ops = TodoService
  { createTodoImpl = \todoTextStr -> do
      now <- liftIO getCurrentTime
      randNum <- liftIO $ randomRIO (1000000, 9999999 :: Int)
      let todoIdStr = show randNum
      case createTodoDTO ops todoIdStr todoTextStr now of
        Left err -> error $ T.unpack err
        Right eventDto -> do
          appendEventDto eventDto
          notifyEventDto "TodoCreated" eventDto
          return todoIdStr
  , toggleTodoImpl = \targetIdStr -> do
      eventDtos <- getAllEventDtos
      case findTodoDTOById ops targetIdStr eventDtos of
        Just todo -> do
          now <- liftIO getCurrentTime
          let eventResult = if taskDtoIsCompleted todo
              then uncompleteTodoDTO ops targetIdStr now
              else completeTodoDTO ops targetIdStr now
          case eventResult of
            Left _ -> return False
            Right eventDto -> do
              appendEventDto eventDto
              notifyEventDto (getEventType eventDto) eventDto
              return True
                where
                  getEventType (TodoCompletedDTO _ _) = "TodoCompleted"
                  getEventType (TodoUncompletedDTO _ _) = "TodoUncompleted"
                  getEventType _ = ""
        Nothing -> return False

  , deleteTodoImpl = \targetIdStr -> do
      now <- liftIO getCurrentTime
      case deleteTodoDTO ops targetIdStr now of
        Left _ -> return False
        Right eventDto -> do
          appendEventDto eventDto
          notifyEventDto "TodoDeleted" eventDto
          return True

  , getAllTodosImpl = do
      getAllTodosDTO ops <$> getAllEventDtos
  , getStatisticsImpl = do
      getStatisticsDTO ops <$> getAllEventDtos

  , getEventHistoryImpl = getAllEventDtos
  }
