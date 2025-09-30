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


import Domain.Todo.Events
import Domain.Todo.ValueObject
import Domain.Todo.DomainService

import Data.Text (Text)
import Data.Time (UTCTime)
import Data.Maybe (mapMaybe)


-- Domain Error Types
-- These represent all possible domain-level failures in task management
data DomainError
  = InvalidTaskId Text
  | InvalidTaskDescription Text
  | TaskNotFound Text
  | DomainLogicError Text
  deriving (Show, Eq)

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
  { -- Command Handlers
    initiateTaskFromRequest :: TaskInitiationRequest -> Either DomainError DomainEvent
  , completeTaskFromRequest :: TaskUpdateRequest -> Either DomainError DomainEvent
  , reopenTaskFromRequest :: TaskUpdateRequest -> Either DomainError DomainEvent
  , deleteTaskFromRequest :: TaskUpdateRequest -> Either DomainError DomainEvent

    -- イベント投影とクエリ
  , projectEventsToSnapshots :: [DomainEvent] -> [TaskSnapshot]
  , findTaskById :: String -> [DomainEvent] -> Maybe TaskSnapshot
  , getTaskStatistics :: [DomainEvent] -> TaskStatistics

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
  , completeTaskFromRequest = mkTaskUpdateEvent TaskCompleted
  , reopenTaskFromRequest = mkTaskUpdateEvent TaskReopened
  , deleteTaskFromRequest = mkTaskUpdateEvent TaskDeleted
  -- DomainService
  , projectEventsToSnapshots = projectToSnapshots
  , findTaskById = findTaskInProjection
  , getTaskStatistics = takeStatistics
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
      let -- TaskUpdate系イベントの変換を抽象化
          convertTaskUpdateEvent handler record =
            let req = TaskUpdateRequest (recordTaskId record) (recordTimestamp record)
            in case handler req of
              Left _ -> Nothing
              Right domainEvent -> Just domainEvent

          convertRecordToDomainEvent record = do
            evType <- eventTypeFromString (recordType record)
            case evType of
              TaskInitiated -> case recordTaskDescription record of
                Just desc ->
                  let req = TaskInitiationRequest (recordTaskId record) desc (recordTimestamp record)
                  in case initiateTaskFromRequest mkTodoDomainFacade req of
                    Left _ -> Nothing
                    Right domainEvent -> Just domainEvent
                Nothing -> Nothing
              TaskCompleted -> convertTaskUpdateEvent (completeTaskFromRequest mkTodoDomainFacade) record
              TaskReopened -> convertTaskUpdateEvent (reopenTaskFromRequest mkTodoDomainFacade) record
              TaskDeleted -> convertTaskUpdateEvent (deleteTaskFromRequest mkTodoDomainFacade) record

          domainEvents = mapMaybe convertRecordToDomainEvent eventRecords
          in projectEventsToSnapshots mkTodoDomainFacade domainEvents

    -- DTO変換サポートの実装
  , dtoConversion = DTOConversionSupport
      { todoDtoToTaskSnapshot = \(id', desc', isCompleted') ->
          TaskSnapshot
            { snapshotTaskId = id'
            , snapshotTaskDesc = desc'
            , snapshotTaskIsCompleted = isCompleted'
            }
      , taskSnapshotToTodoDto = \snapshot ->
        (snapshotTaskId snapshot, snapshotTaskDesc snapshot, snapshotTaskIsCompleted snapshot)
      , todoEventDtoToTaskEventRecord = \(eventType', taskId', desc, timestamp') -> TaskEventRecord
          { recordType = eventType'
          , recordTaskId = taskId'
          , recordTaskDescription = desc
          , recordTimestamp= timestamp'
          }
      , taskEventRecordToTodoEventDto = \record ->
          (recordType record, recordTaskId record, recordTaskDescription record, recordTimestamp record)
      , statisticsTupleToDto = id -- (Int, Int, Int) -> (Int, Int, Int)
      }
  }

-- fromRequest抽象化
mkTaskUpdateEvent :: EventType -> TaskUpdateRequest -> Either DomainError DomainEvent
mkTaskUpdateEvent eventType' req = do
  taskId' <- validateTaskId mkTodoDomainFacade (updateTaskId req)
  return $ DomainEvent
    { eventType = eventType'
    , eventTaskId = taskId'
    , eventDescription = Nothing
    , eventTimestamp = updateTimestamp req
    }
