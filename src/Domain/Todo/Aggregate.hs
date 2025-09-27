{-# LANGUAGE OverloadedStrings #-}


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
import qualified Data.Text as T



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
    initiateTaskFromRequest :: TaskInitiationRequest -> Either DomainError DomainEvent

    -- Task状態変更
  , completeTaskFromRequest :: TaskUpdateRequest -> Either DomainError DomainEvent
  , reopenTaskFromRequest :: TaskUpdateRequest -> Either DomainError DomainEvent
  , deleteTaskFromRequest :: TaskUpdateRequest -> Either DomainError DomainEvent

    -- イベント投影とクエリ
  , projectEventsToSnapshots :: [DomainEvent] -> [TaskSnapshot]
  , findTaskById :: String -> [DomainEvent] -> Maybe TaskSnapshot
  , getTaskStatistics :: [DomainEvent] -> (Int, Int, Int) -- (total number of tasks, completed tasks, active tasks)

    -- イベント変換
  , eventToRecord :: DomainEvent -> TaskEventRecord
  , eventsToRecords :: [DomainEvent] -> [TaskEventRecord]

    -- バリデーション
  , validateTaskId :: String -> Either DomainError TaskId
  , validateTaskDescription :: String -> Either DomainError TaskDescription

    -- TaskEventRecordからTaskSnapshotへの変換
  , takeSnapshotsFromEventRecords :: [TaskEventRecord] -> [TaskSnapshot]

    -- DTO変換サポート
  , dtoConversion :: DTOConversionSupport
  }

-- ファサードの実装
mkTodoDomainFacade :: TodoDomainFacade
mkTodoDomainFacade = TodoDomainFacade
  { initiateTaskFromRequest = \req -> do
      taskId' <- validateTaskId mkTodoDomainFacade (initiationTaskId req)
      taskDescription' <- validateTaskDescription mkTodoDomainFacade (initiationTaskDescription req)
      return $ DomainEvent
        { eventType = TaskInitiated
        , eventTaskId = taskId'
        , eventDescription = Just taskDescription'
        , eventTimestamp = initiationTimestamp req
        }
  , completeTaskFromRequest = \req -> do
      taskId' <- validateTaskId mkTodoDomainFacade (updateTaskId req)
      return $ DomainEvent
        { eventType = TaskCompleted
        , eventTaskId = taskId'
        , eventDescription = Nothing
        , eventTimestamp = updateTimestamp req
        }
  , reopenTaskFromRequest = \req -> do
      taskId' <- validateTaskId mkTodoDomainFacade (updateTaskId req)
      return $ DomainEvent
        { eventType = TaskReopened
        , eventTaskId = taskId'
        , eventDescription = Nothing
        , eventTimestamp = updateTimestamp req
        }
  , deleteTaskFromRequest = \req -> do
      taskId' <- validateTaskId mkTodoDomainFacade (updateTaskId req)
      return $ DomainEvent
        { eventType = TaskDeleted
        , eventTaskId = taskId'
        , eventDescription = Nothing
        , eventTimestamp = updateTimestamp req
        }
  , projectEventsToSnapshots = \events -> case projectEvents events of
      Right tasks -> Map.elems $ Map.map taskToSnapshot tasks
      Left err -> error $ "Failed to project events: " ++ show err -- 後でlogger実装をする
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
  , eventToRecord = \domainEvent -> TaskEventRecord
      { recordType = eventTypeToString (eventType domainEvent)
      , recordTaskId = toIdString (eventTaskId domainEvent)
      , recordTaskDescription = fmap toDescString (eventDescription domainEvent)
      , recordTimestamp = eventTimestamp domainEvent
      }
  , eventsToRecords = map (eventToRecord mkTodoDomainFacade)
  , validateTaskId = \idStr ->
      case mkTaskId idStr of
        Left err -> Left $ InvalidTaskId err
        Right taskId' -> Right taskId'

  , validateTaskDescription = \descStr ->
      case mkTaskDescription descStr of
        Left err -> Left $ InvalidTaskDescription err
        Right taskDescription' -> Right taskDescription'

  , takeSnapshotsFromEventRecords = \eventRecords ->
      let convertRecordToDomainEvent record = do
            evType <- eventTypeFromString (recordType record)
            case evType of
              TaskInitiated -> case recordTaskDescription record of
                Just desc ->
                  let req = TaskInitiationRequest (recordTaskId record) desc (recordTimestamp record)
                  in case initiateTaskFromRequest mkTodoDomainFacade req of
                    Left _ -> Nothing
                    Right domainEvent -> Just domainEvent
                Nothing -> Nothing
              TaskCompleted ->
                let req = TaskUpdateRequest (recordTaskId record) (recordTimestamp record)
                in case completeTaskFromRequest mkTodoDomainFacade req of
                  Left _ -> Nothing
                  Right domainEvent -> Just domainEvent
              TaskReopened ->
                let req = TaskUpdateRequest (recordTaskId record) (recordTimestamp record)
                in case reopenTaskFromRequest mkTodoDomainFacade req of
                  Left _ -> Nothing
                  Right domainEvent -> Just domainEvent
              TaskDeleted ->
                let req = TaskUpdateRequest (recordTaskId record) (recordTimestamp record)
                in case deleteTaskFromRequest mkTodoDomainFacade req of
                  Left _ -> Nothing
                  Right domainEvent -> Just domainEvent
          domainEvents = mapMaybe convertRecordToDomainEvent eventRecords
          in projectEventsToSnapshots mkTodoDomainFacade domainEvents

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

-- DomainEventからの投影
projectEvents :: [DomainEvent] -> Either DomainError (Map TaskId Task)
projectEvents = foldl' applyEvent (Right Map.empty)
  where
    applyEvent :: Either DomainError (Map TaskId Task) -> DomainEvent -> Either DomainError (Map TaskId Task)
    applyEvent (Left err) _ = Left err -- すでにエラーが出ている場合は伝播
    applyEvent (Right tasks') event = case eventType event of
      TaskInitiated -> case eventDescription event of
        Just desc -> Right $ Map.insert (eventTaskId event) (Task (eventTaskId event) desc False) tasks'
        Nothing -> Left $ DomainLogicError $ T.pack $ "TaskInitiated event missing description for task ID: " ++ toIdString (eventTaskId event)
      TaskCompleted -> if Map.member (eventTaskId event) tasks'
        then Right $ Map.adjust (\t -> t{isCompleted = True}) (eventTaskId event) tasks'
        else Left $ TaskNotFound $ T.pack $ "Cannot complete non-existent task for task ID: " ++ toIdString (eventTaskId event)
      TaskReopened -> if Map.member (eventTaskId event) tasks'
        then Right $ Map.adjust (\t -> t{isCompleted = False}) (eventTaskId event) tasks'
        else Left $ TaskNotFound $ T.pack $ "Cannot reopen non-existent task for task ID: " ++ toIdString (eventTaskId event)
      TaskDeleted -> if Map.member (eventTaskId event) tasks'
        then Right $ Map.delete (eventTaskId event) tasks'
        else Left $ TaskNotFound $ T.pack $ "Cannot delete non-existent task for task ID: " ++ toIdString (eventTaskId event)
