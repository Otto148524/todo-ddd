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
  -- ver2用
  , TaskEventRecordV2(..)
  , domainEventToRecordV2
  , eventToRecordsV2
  , upgradeRecord
  , downgradeRecord
  , getTodoDomainFacade
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
  , eventToRecord :: TodoEvent -> TaskEventRecord
  , eventsToRecords :: [TodoEvent] -> [TaskEventRecord]

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
  , eventToRecord = \case
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

-- === ver2: 型安全な実装（DomainEventベース）===

-- 新しいTaskEventRecord（EventTypeをADTで持つ）
data TaskEventRecordV2 = TaskEventRecordV2
  { recordEventTypeV2 :: EventType
  , recordTaskIdV2 :: String
  , recordTaskDescriptionV2 :: Maybe String
  , recordTimestampV2 :: UTCTime
  } deriving (Show, Eq)

-- DomainEventからV2レコードへ変換
domainEventToRecordV2 :: DomainEvent -> TaskEventRecordV2
domainEventToRecordV2 event = TaskEventRecordV2
  { recordEventTypeV2 = eventType event
  , recordTaskIdV2 = toIdString (eventTaskId event)
  , recordTaskDescriptionV2 = fmap toDescString (eventDescription event)
  , recordTimestampV2 = eventTimestamp event
  }

-- TodoEventからV2レコードへ（移行用）
eventToRecordsV2 :: TodoEvent -> TaskEventRecordV2
eventToRecordsV2 todoEvent = domainEventToRecordV2 (legacyToNewEvent todoEvent)

-- 既存RecordからV2への変換
upgradeRecord :: TaskEventRecord -> Maybe TaskEventRecordV2
upgradeRecord old = do
  evType <- eventTypeFromString (recordType old)
  return TaskEventRecordV2
    { recordEventTypeV2 = evType
    , recordTaskIdV2 = recordTaskId old
    , recordTaskDescriptionV2 = recordTaskDescription old
    , recordTimestampV2 = recordTimestamp old
    }

-- V2から既存Recordへの変換（互換性のため）
downgradeRecord :: TaskEventRecordV2 -> TaskEventRecord
downgradeRecord new = TaskEventRecord
  { recordType = eventTypeToString (recordEventTypeV2 new)
  , recordTaskId = recordTaskIdV2 new
  , recordTaskDescription = recordTaskDescriptionV2 new
  , recordTimestamp = recordTimestampV2 new
  }

-- V2用のtakeSnapshotsFromEventRecords（ADTパターンマッチで型安全）
takeSnapshotsFromEventRecordsV2 :: [TaskEventRecordV2] -> [TaskSnapshot]
takeSnapshotsFromEventRecordsV2 eventRecords =
  let convertRecordToEvent record = case recordEventTypeV2 record of -- ADTのパターンマッチ
        EventTaskInitiated ->
          case recordTaskDescriptionV2 record of
            Just desc ->
              let req = TaskInitiationRequest (recordTaskIdV2 record) desc (recordTimestampV2 record)
              in case initiateTaskFromRequest mkTodoDomainFacade req of
                Left _ -> Nothing
                Right event -> Just event
            Nothing -> Nothing
        EventTaskCompleted ->
          let req = TaskUpdateRequest (recordTaskIdV2 record) (recordTimestampV2 record)
          in case completeTaskFromRequest mkTodoDomainFacade req of
            Left _ -> Nothing
            Right event -> Just event
        EventTaskReopened ->
          let req = TaskUpdateRequest (recordTaskIdV2 record) (recordTimestampV2 record)
          in case reopenTaskFromRequest mkTodoDomainFacade req of
            Left _ -> Nothing
            Right event -> Just event
        EventTaskDeleted ->
          let req = TaskUpdateRequest (recordTaskIdV2 record) (recordTimestampV2 record)
          in case deleteTaskFromRequest mkTodoDomainFacade req of
            Left _ -> Nothing
            Right event -> Just event
      events = mapMaybe convertRecordToEvent eventRecords
      in projectEventsToSnapshots mkTodoDomainFacade events

-- 新しいファサード（将来のメイン実装）
mkTodoDomainFacadeV2 :: TodoDomainFacade
mkTodoDomainFacadeV2 = mkTodoDomainFacade
  { -- Task作成(to todoEvent)
    initiateTaskFromRequest = \req -> do
      domainEvent <- initiateTaskV2 req
      case newToLegacyEvent domainEvent of
        Just todoEvent -> Right todoEvent
        Nothing -> Left $ DomainLogicError "Failed to convert DomainEvent to TodoEvent"
    -- Task状態変更（to todoEvent）
  , completeTaskFromRequest = \req -> do
    domainEvent <- completeTaskV2 req
    case newToLegacyEvent domainEvent of
      Just todoEvent -> Right todoEvent
      Nothing -> Left $ DomainLogicError "Failed to convert DomainEvent to TodoEvent"

  , reopenTaskFromRequest = \req -> do
    domainEvent <- reopenTaskV2 req
    case newToLegacyEvent domainEvent of
      Just todoEvent -> Right todoEvent
      Nothing -> Left $ DomainLogicError "Failed to convert DomainEvent to  TodoEvent"
  , deleteTaskFromRequest = \req -> do
    domainEvent <- deleteTaskV2 req
    case newToLegacyEvent domainEvent of
      Just todoEvent -> Right todoEvent
      Nothing -> Left $ DomainLogicError "Failed to convert DomainEvent to TodoEvent"
    -- イベント投影とクエリ
  , projectEventsToSnapshots = Map.elems . Map.map taskToSnapshot . projectEvents
  , findTaskById = \targetId events ->
      let tasks = projectEventsToSnapshots mkTodoDomainFacadeV2 events
      in case filter (\t -> snapshotTaskId t == targetId) tasks of
        (task:_) -> Just task
        [] -> Nothing
  , getTaskStatistics = \events ->
      let tasks = projectEventsToSnapshots mkTodoDomainFacadeV2 events
          total = length tasks
          completedCount' = length $ filter snapshotTaskCompleted tasks
          active = total - completedCount'
      in (total, completedCount', active)
    -- TodoEvent変換
  , eventToRecord = downgradeRecord . eventToRecordsV2
  , eventsToRecords = map (downgradeRecord . eventToRecordsV2)

    -- validation
  , validateTaskId = \idStr ->
      case mkTaskId idStr of
        Left err -> Left $ InvalidTaskId err
        Right taskId' -> Right taskId'

  , validateTaskDescription = \descStr ->
      case mkTaskDescription descStr of
        Left err -> Left $ InvalidTaskDescription err
        Right taskDescription' -> Right taskDescription'
    -- TaskEventRecordからTaskSnapshotへの変換
  , takeSnapshotsFromEventRecords = \eventRecords ->
      let v2Records = mapMaybe upgradeRecord eventRecords
      in takeSnapshotsFromEventRecordsV2 v2Records
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
      , todoEventDtoToTaskEventRecord = \(eventType', todoId', desc, timestamp') ->
          case eventTypeFromString eventType' of
            Just et ->
              downgradeRecord $ TaskEventRecordV2
                { recordEventTypeV2 = et
                , recordTaskIdV2 = todoId'
                , recordTaskDescriptionV2 = desc
                , recordTimestampV2 = timestamp'
                }
            Nothing -> error $ "Invalid event type in DTO: " ++ eventType' ++ ". Migration to V2 required."
      , taskEventRecordToTodoEventDto = \record ->
          case upgradeRecord record of
            Just v2Record ->
              ( eventTypeToString (recordEventTypeV2 v2Record)
              , recordTaskIdV2 v2Record
              , recordTaskDescriptionV2 v2Record
              , recordTimestampV2 v2Record
              )
            Nothing -> error $ "Failed to upgrade TaskEventRecord with type: " ++ recordType record ++ ". Invalid event type."
      , statisticsTupleToDto = id -- (Int, Int, Int) -> (Int, Int, Int)
      }
  }

-- フィーチャーフラグ（切り替え用）
useV2Implementation :: Bool
useV2Implementation = True -- 移行可能になったらTrueに変える

-- 切り替え可能なファサード
getTodoDomainFacade :: TodoDomainFacade
getTodoDomainFacade = if useV2Implementation
  then mkTodoDomainFacadeV2
  else mkTodoDomainFacade

-- V2用のヘルパー関数
initiateTaskV2 :: TaskInitiationRequest -> Either DomainError DomainEvent
initiateTaskV2 req = do
  taskId' <- case mkTaskId (initiationTaskId req) of
    Left err -> Left $ InvalidTaskId err
    Right tid -> Right tid
  taskDescription' <- case mkTaskDescription (initiationTaskDescription req) of
    Left err -> Left $ InvalidTaskDescription err
    Right desc -> Right desc
  return $ DomainEvent
    { eventType = EventTaskInitiated
    , eventTaskId = taskId'
    , eventDescription = Just taskDescription'
    , eventTimestamp = initiationTimestamp req
    }

completeTaskV2 :: TaskUpdateRequest -> Either DomainError DomainEvent
completeTaskV2 req = do
  taskId' <- case mkTaskId (updateTaskId req) of
    Left err -> Left $ InvalidTaskId err
    Right tid -> Right tid
  return $ DomainEvent
    { eventType = EventTaskCompleted
    , eventTaskId = taskId'
    , eventDescription = Nothing
    , eventTimestamp = updateTimestamp req
    }

reopenTaskV2 :: TaskUpdateRequest -> Either DomainError DomainEvent
reopenTaskV2 req = do
  taskId' <- case mkTaskId (updateTaskId req) of
    Left err -> Left $ InvalidTaskId err
    Right tid -> Right tid
  return $ DomainEvent
    { eventType = EventTaskReopened
    , eventTaskId = taskId'
    , eventDescription = Nothing
    , eventTimestamp = updateTimestamp req
    }

deleteTaskV2 :: TaskUpdateRequest -> Either DomainError DomainEvent
deleteTaskV2 req = do
  taskId' <- case mkTaskId (updateTaskId req) of
    Left err -> Left $ InvalidTaskId err
    Right tid -> Right tid
  return $ DomainEvent
    { eventType = EventTaskDeleted
    , eventTaskId = taskId'
    , eventDescription = Nothing
    , eventTimestamp = updateTimestamp req
    }


