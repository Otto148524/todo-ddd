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
  { initiateTaskImpl :: String -> m String
  , toggleTaskImpl :: String -> m Bool
  , deleteTaskImpl :: String -> m Bool
  , getAllTasksImpl :: m [TaskDTO]
  , getStatisticsImpl :: m TasksStatisticsDTO
  , getEventHistoryImpl :: m [TodoEventDTO]
  }

mkTodoService :: (EventStore m, NotificationPort m, MonadIO m) => DomainOperations -> TodoService m
mkTodoService ops = TodoService
  { initiateTaskImpl = \taskDescStr -> do
      now <- liftIO getCurrentTime
      randNum <- liftIO $ randomRIO (1000000, 9999999 :: Int)
      let taskIdStr = show randNum
      case initiateTaskDTO ops taskIdStr taskDescStr now of
        Left err -> error $ T.unpack err
        Right eventDto -> do
          appendEventDto eventDto
          notifyEventDto "TaskInitiated" eventDto
          return taskIdStr
  , toggleTaskImpl = \targetIdStr -> do
      eventDtos <- getAllEventDtos
      case findTaskDTOById ops targetIdStr eventDtos of
        Just todo -> do
          now <- liftIO getCurrentTime
          let eventResult = if taskDtoIsCompleted todo
              then reopenTaskDTO ops targetIdStr now
              else completeTaskDTO ops targetIdStr now
          case eventResult of
            Left _ -> return False
            Right eventDto -> do
              appendEventDto eventDto
              notifyEventDto (getEventType eventDto) eventDto
              return True
                where
                  getEventType (TaskCompletedDTO _ _) = "TaskCompleted"
                  getEventType (TaskReopenedDTO _ _) = "TaskReopened"
                  getEventType _ = ""
        Nothing -> return False

  , deleteTaskImpl = \targetIdStr -> do
      now <- liftIO getCurrentTime
      case deleteTaskDTO ops targetIdStr now of
        Left _ -> return False
        Right eventDto -> do
          appendEventDto eventDto
          notifyEventDto "TaskDeleted" eventDto
          return True

  , getAllTasksImpl = do
      getAllTaskDTOs ops <$> getAllEventDtos
  , getStatisticsImpl = do
      getStatisticsDTO ops <$> getAllEventDtos

  , getEventHistoryImpl = getAllEventDtos
  }
