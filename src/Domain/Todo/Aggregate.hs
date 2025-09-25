{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

module Domain.Todo.Aggregate
  ( TodoDomainFacade(..)
  , mkTodoDomainFacade
  , TaskInitiationRequest(..)
  , TaskUpdateRequest(..)
  , TaskSnapshot(..)
  , TaskEventRecord(..)
  , DomainError(..)
  -- DTO変換のためのエクスポート
  , DTOConversionSupport(..)
  ) where

import Domain.Todo.Entity
import Domain.Todo.Events
import Domain.Todo.ValueObject

import Data.Text (Text)
import Data.Time (UTCTime)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (mapMaybe)
import Data.List (foldl')



-- Domain Error Types
-- These represent all possible domain-level failures in task management
data DomainError
  = InvalidTaskId Text
  | InvalidTaskDescription Text
  | TaskNotFound Text
  | DomainLogicError Text
  deriving (Show, Eq)

-- 外部向けのスナップショット型（DTOの代わり）
data TaskSnapshot = TaskSnapshot
  { snapshotTaskId :: String
  , snapshotTaskDescription :: String
  , snapshotTaskCompleted :: Bool
  } deriving (Show, Eq)

data TaskEventRecord= TaskEventRecord
  { recordType :: String
  , recordTaskId :: String
  , recordTaskDescription :: Maybe String
  , recordTimestamp :: UTCTime
  } deriving (Show, Eq)

data TaskInitiationRequest = TaskInitiationRequest
  { initiationTaskId :: String
  , initiationTaskDescription :: String
  , initiationTimestamp :: UTCTime
  } deriving (Show, Eq)

data TaskUpdateRequest = TaskUpdateRequest
  { updateTaskId :: String
  , updateTimestamp :: UTCTime
  } deriving (Show, Eq)

-- DTO変換サポート型
data DTOConversionSupport = DTOConversionSupport
  { -- TodoDTO <-> TaskSnapshot 変換
    todoDtoToTaskSnapshot :: (String, String, Bool) -> TaskSnapshot
  , taskSnapshotToTodoDto :: TaskSnapshot -> (String, String, Bool)

    -- TodoEventDTO <-> TaskEventRecord 変換
  , todoEventDtoToTaskEventRecord :: (String, String, Maybe String, UTCTime) -> TaskEventRecord
  , taskEventRecordToTodoEventDto :: TaskEventRecord -> (String, String, Maybe String, UTCTime)

    -- statisticsDTO変換
  , statisticsTupleToDto :: (Int, Int, Int) -> (Int, Int, Int) -- そのまま
  }

-- 統一インターフェース
data TodoDomainFacade = TodoDomainFacade
  { -- Task作成
    initiateTaskFromRequest :: TaskInitiationRequest -> Either DomainError TodoEvent

    -- Task状態変更
  , completeTaskFromRequest :: TaskUpdateRequest -> Either DomainError TodoEvent
  , reopenTaskFromRequest :: TaskUpdateRequest -> Either DomainError TodoEvent
  , deleteTaskFromRequest :: TaskUpdateRequest -> Either DomainError TodoEvent

    -- イベント投影とクエリ
  , projectEventsToSnapshots :: [TodoEvent] -> [TaskSnapshot]
  , findTaskById :: String -> [TodoEvent] -> Maybe TaskSnapshot
  , getTaskStatistics :: [TodoEvent] -> (Int, Int, Int) -- (total number of tasks, completed tasks, active tasks)

    -- イベント変換
  , eventToRecords :: TodoEvent -> TaskEventRecord
  , eventsToRecord :: [TodoEvent] -> [TaskEventRecord]

    -- バリデーション
  , validateTaskId :: String -> Either DomainError TaskId
  , validateTaskDescription :: String -> Either DomainError TaskDescription

    -- TaskEventRecordからTaskSnapshotへの直接変換
  , projectEventRecordsDirectly :: [TaskEventRecord] -> [TaskSnapshot]

    -- DTO変換サポート
  , dtoConversion :: DTOConversionSupport
  }

-- ファサードの実装
mkTodoDomainFacade :: TodoDomainFacade
mkTodoDomainFacade = TodoDomainFacade
  { initiateTaskFromRequest = \req -> do
      taskId' <- validateTaskId mkTodoDomainFacade (initiationTaskId req)
      taskDescription' <- validateTaskDescription mkTodoDomainFacade (initiationTaskDescription req)
      return $ TaskInitiated taskId' taskDescription' (initiationTimestamp req)
  , completeTaskFromRequest = \req -> do
      taskId' <- validateTaskId mkTodoDomainFacade (updateTaskId req)
      return $ TaskCompleted taskId' (updateTimestamp req)
  , reopenTaskFromRequest = \req -> do
      taskId' <- validateTaskId mkTodoDomainFacade (updateTaskId req)
      return $ TaskReopened taskId' (updateTimestamp req)
  , deleteTaskFromRequest = \req -> do
      taskId' <- validateTaskId mkTodoDomainFacade (updateTaskId req)
      return $ TaskDeleted taskId' (updateTimestamp req)
  , projectEventsToSnapshots = Map.elems . Map.map taskToSnapshot . projectEvents
  , findTaskById = \targetId events ->
      let tasks = projectEventsToSnapshots mkTodoDomainFacade events
      in case filter (\t -> snapshotTaskId t == targetId) tasks of
        (task:_) -> Just task
        [] -> Nothing
  , getTaskStatistics = \events ->
    let tasks = projectEventsToSnapshots mkTodoDomainFacade events
        total = length tasks
        completedCount' = length $ filter snapshotTaskCompleted tasks
        active = total - completedCount'
    in (total, completedCount', active)
  , eventToRecords = \case
      TaskInitiated tid desc timestamp' -> TaskEventRecord
        { recordType = "TodoCreated"
        , recordTaskId = toIdString tid
        , recordTaskDescription = Just $ toDescString desc
        , recordTimestamp = timestamp'
        }
      TaskCompleted tid timestamp' -> TaskEventRecord
        { recordType = "TodoCompleted"
        , recordTaskId = toIdString tid
        , recordTaskDescription = Nothing
        , recordTimestamp = timestamp'
        }
      TaskReopened tid timestamp' -> TaskEventRecord
        { recordType = "TodoUncompleted"
        , recordTaskId = toIdString tid
        , recordTaskDescription = Nothing
        , recordTimestamp = timestamp'
        }
      TaskDeleted tid timestamp' -> TaskEventRecord
        { recordType = "TodoDeleted"
        , recordTaskId = toIdString tid
        , recordTaskDescription = Nothing
        , recordTimestamp = timestamp'
        }
  , eventsToRecord = map (eventToRecords mkTodoDomainFacade)
  , validateTaskId = \idStr ->
      case mkTaskId idStr of
        Left err -> Left $ InvalidTaskId err
        Right taskId' -> Right taskId'

  , validateTaskDescription = \descStr ->
      case mkTaskDescription descStr of
        Left err -> Left $ InvalidTaskDescription err
        Right taskDescription' -> Right taskDescription'

  , projectEventRecordsDirectly = \eventRecords ->
      let convertRecordToEvent record = case recordType record of
            "TodoCreated" -> case recordTaskDescription record of
              Just desc ->
                let req = TaskInitiationRequest (recordTaskId record) desc (recordTimestamp record)
                in case initiateTaskFromRequest mkTodoDomainFacade req of
                  Left _ -> Nothing
                  Right event -> Just event
              Nothing -> Nothing
            "TodoCompleted" ->
              let req = TaskUpdateRequest (recordTaskId record) (recordTimestamp record)
              in case completeTaskFromRequest mkTodoDomainFacade req of
                Left _ -> Nothing
                Right event -> Just event
            "TodoUncompleted" ->
              let req = TaskUpdateRequest (recordTaskId record) (recordTimestamp record)
              in case reopenTaskFromRequest mkTodoDomainFacade req of
                Left _ -> Nothing
                Right event -> Just event
            "TodoDeleted" ->
              let req = TaskUpdateRequest (recordTaskId record) (recordTimestamp record)
              in case deleteTaskFromRequest mkTodoDomainFacade req of
                Left _ -> Nothing
                Right event -> Just event
            _ -> Nothing
          events = mapMaybe convertRecordToEvent eventRecords
          in projectEventsToSnapshots mkTodoDomainFacade events

    -- DTO変換サポートの実装
  , dtoConversion = DTOConversionSupport
      { todoDtoToTaskSnapshot = \(id', desc', isCompleted') ->
          TaskSnapshot
            { snapshotTaskId = id'
            , snapshotTaskDescription = desc'
            , snapshotTaskCompleted = isCompleted'
            }
      , taskSnapshotToTodoDto = \snapshot ->
        (snapshotTaskId snapshot, snapshotTaskDescription snapshot, snapshotTaskCompleted snapshot)
      , todoEventDtoToTaskEventRecord = \(eventType', todoId', desc, timestamp') -> TaskEventRecord
          { recordType = eventType'
          , recordTaskId = todoId'
          , recordTaskDescription = desc
          , recordTimestamp= timestamp'
          }
      , taskEventRecordToTodoEventDto = \record ->
          (recordType record, recordTaskId record, recordTaskDescription record, recordTimestamp record)
      , statisticsTupleToDto = id -- (Int, Int, Int) -> (Int, Int, Int)
      }
  }

-- 内部ヘルパー関数

-- TaskエンティティをTaskSnapshotに変換する関数
taskToSnapshot :: Task -> TaskSnapshot
taskToSnapshot task = TaskSnapshot
  { snapshotTaskId = toIdString (taskId task)
  , snapshotTaskDescription = toDescString (taskDescription task)
  , snapshotTaskCompleted = isCompleted task
  }

-- 元々Aggregate.hsにあったものを移してきた
projectEvents :: [TodoEvent] -> Map TaskId Task
projectEvents = foldl' applyEvent Map.empty
  where
    applyEvent :: Map TaskId Task -> TodoEvent -> Map TaskId Task
    applyEvent tasks' (TaskInitiated tid desc _) =
      Map.insert tid (Task tid desc False) tasks'
    applyEvent tasks' (TaskCompleted tid _) =
      Map.adjust (\t -> t{isCompleted = True}) tid tasks'
    applyEvent tasks' (TaskReopened tid _) =
      Map.adjust (\t -> t{isCompleted = False}) tid tasks'
    applyEvent tasks' (TaskDeleted tid _) =
      Map.delete tid tasks'
