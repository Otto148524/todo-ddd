{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wno-missing-signatures #-}

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
      Left err -> Left (domainErrorToText err)
      Right domainEvent ->
        let domainRecord = eventToRecord facade domainEvent
            (eventType', eid, mDesc, ts) = taskEventRecordToTodoEventDto (dtoConversion facade) domainRecord
        in case eventType' of
          "TaskInitiated" -> Right $ TaskInitiatedDTO eid (Data.Maybe.fromMaybe "" mDesc) ts
          _ -> Left $ T.pack $ "Unexpected event type: " ++ eventType'
  , completeTaskDTO = processTaskUpdate completeTaskFromRequest "TaskCompleted" TaskCompletedDTO
  , reopenTaskDTO = processTaskUpdate reopenTaskFromRequest "TaskReopened" TaskReopenedDTO
  , deleteTaskDTO = processTaskUpdate deleteTaskFromRequest "TaskDeleted" TaskDeletedDTO
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

-- エラー変換抽象化
domainErrorToText :: DomainError -> Text
domainErrorToText (InvalidTaskId err) = err
domainErrorToText (InvalidTaskDescription err) = err
domainErrorToText (DomainLogicError err) = err
domainErrorToText (TaskNotFound err) = err

processTaskUpdate domainFunc expectedEventType dtoConstructor taskId' timestamp' =
  let req = TaskUpdateRequest taskId' timestamp'
      facade = mkTodoDomainFacade
  in case domainFunc facade req of
    Left err -> Left (domainErrorToText err)
    Right domainEvent ->
      let domainRecord = eventToRecord facade domainEvent
          (eventType', eid, _, ts) = taskEventRecordToTodoEventDto (dtoConversion facade) domainRecord
      in if eventType' == expectedEventType
        then Right $ dtoConstructor eid ts
        else Left $ T.pack $ "Unexpected event type: " ++ eventType'
