{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

module Application.DTO.Facade
  ( DomainOperations(..)
  , domainOps
  ) where

import Domain.Todo.Aggregate

import Application.DTO.TaskDTO

import Data.Text (Text)
import qualified Data.Text as T
import Data.Time (UTCTime)
import qualified Data.Maybe

-- ドメイン層の関数のファサード
data DomainOperations = DomainOperations
  { -- Todo作成・更新（DTOベース）
    initiateTaskDTO :: String -> String -> UTCTime -> Either Text TodoEventDTO
  , completeTaskDTO :: String -> UTCTime -> Either Text TodoEventDTO
  , reopenTaskDTO :: String -> UTCTime -> Either Text TodoEventDTO
  , deleteTaskDTO :: String -> UTCTime -> Either Text TodoEventDTO

    -- クエリ（DTOベース）
  , getAllTaskDTOs :: [TodoEventDTO] -> [TaskDTO]
  , findTaskDTOById :: String -> [TodoEventDTO] -> Maybe TaskDTO
  , getStatisticsDTO :: [TodoEventDTO] -> TasksStatisticsDTO

    -- Event変換（DTOベース）
  , eventDTOsFromDomainEvents :: [TodoEventDTO] -> [TodoEventDTO] -- id
  }

-- ファサードの実装
domainOps :: DomainOperations
domainOps = DomainOperations
  { initiateTaskDTO = \taskId' desc' timestamp' ->
    let req = TaskInitiationRequest taskId' desc' timestamp'
        facade = mkTodoDomainFacade
    in case initiateTaskFromRequest facade req of
      Left (InvalidTaskId err) -> Left err
      Left (InvalidTaskDescription err) -> Left err
      Left (DomainLogicError err) -> Left err
      Left (TaskNotFound err) -> Left err
      Right domainEvent ->
        let domainView = eventToRecord facade domainEvent
            (eventType', eid, mDesc, ts) = taskEventRecordToTodoEventDto (dtoConversion facade) domainView
        in case eventType' of
          "TaskInitiated" -> Right $ TaskInitiatedDTO eid (Data.Maybe.fromMaybe "" mDesc) ts
          _ -> Left $ T.pack $ "Unexpected event type: " ++ eventType'
  , completeTaskDTO = \taskId' timestamp' ->
      let req = TaskUpdateRequest taskId' timestamp'
          facade = mkTodoDomainFacade
      in case completeTaskFromRequest facade req of
        Left (InvalidTaskId err) -> Left err
        Left (InvalidTaskDescription err) -> Left err
        Left (DomainLogicError err) -> Left err
        Left (TaskNotFound err) -> Left err
        Right domainEvent ->
          let domainView = eventToRecord facade domainEvent
              (eventType', eid, _, ts) = taskEventRecordToTodoEventDto (dtoConversion facade) domainView
          in case eventType' of
            "TaskCompleted" -> Right $ TaskCompletedDTO eid ts
            _ -> Left $ T.pack $ "Unexpected event type: " ++ eventType'
  , reopenTaskDTO = \taskId' timestamp' ->
      let req = TaskUpdateRequest taskId' timestamp'
          facade = mkTodoDomainFacade
      in case reopenTaskFromRequest facade req of
        Left (InvalidTaskId err) -> Left err
        Left (InvalidTaskDescription err) -> Left err
        Left (DomainLogicError err) -> Left err
        Left (TaskNotFound err) -> Left err
        Right domainEvent ->
          let domainRecord = eventToRecord facade domainEvent
              (eventType', eid, _, ts) = taskEventRecordToTodoEventDto (dtoConversion facade) domainRecord
          in case eventType' of
            "TaskReopened" -> Right $ TaskReopenedDTO eid ts
            _ -> Left $ T.pack $ "Unexpected event type: " ++ eventType'
  , deleteTaskDTO = \taskId' timestamp' ->
      let req = TaskUpdateRequest taskId' timestamp'
          facade = mkTodoDomainFacade
      in case deleteTaskFromRequest facade req of
        Left (InvalidTaskId err) -> Left err
        Left (InvalidTaskDescription err) -> Left err
        Left (DomainLogicError err) -> Left err
        Left (TaskNotFound err) -> Left err
        Right domainEvent ->
          let domainRecord = eventToRecord facade domainEvent
              (eventType', eid, _, ts) = taskEventRecordToTodoEventDto (dtoConversion facade) domainRecord
          in case eventType' of
            "TaskDeleted" -> Right $ TaskDeletedDTO eid ts
            _ -> Left $ T.pack $ "Unexpected event type: " ++ eventType'
  , getAllTaskDTOs = \eventDtos ->
      let facade = mkTodoDomainFacade
          -- DTOからTaskEventRecordに変換
          domainRecords = map (\case
              TaskInitiatedDTO eid txt ts -> todoEventDtoToTaskEventRecord (dtoConversion facade) ("TaskInitiated", eid, Just txt, ts)
              TaskCompletedDTO eid ts -> todoEventDtoToTaskEventRecord (dtoConversion facade) ("TaskCompleted", eid, Nothing, ts)
              TaskReopenedDTO eid ts -> todoEventDtoToTaskEventRecord (dtoConversion facade) ("TaskReopened", eid, Nothing, ts)
              TaskDeletedDTO eid ts -> todoEventDtoToTaskEventRecord (dtoConversion facade) ("TaskDeleted", eid, Nothing, ts)
              ) eventDtos
          -- TaskEventRecordからTaskSnapshotに投影
          taskSnapshots = takeSnapshotsFromEventRecords facade domainRecords
          -- TaskSnapshotからDTOに変換
      in map (\snapshot ->
          let (eid, desc, isCompleted') = taskSnapshotToTodoDto (dtoConversion facade) snapshot
          in TaskDTO eid desc isCompleted') taskSnapshots
  , findTaskDTOById = \targetId eventDtos ->
      case getAllTaskDTOs domainOps eventDtos of
        tasks' -> case filter (\dto -> taskDtoId dto == targetId) tasks' of
          (task:_) -> Just task
          [] -> Nothing
  , getStatisticsDTO = \eventDtos ->
      let tasks' = getAllTaskDTOs domainOps eventDtos
          total = length tasks'
          completed' = length $ filter taskDtoIsCompleted tasks'
          active = total - completed'
      in TasksStatisticsDTO total active completed'
  , eventDTOsFromDomainEvents = id
  }
