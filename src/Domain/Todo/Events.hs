{-# LANGUAGE DeriveGeneric #-}

module Domain.Todo.Events
  ( TodoEvent(..) -- later deprecated
  , EventType(..) -- TodoEventからこちらに移行予定（その時に型の名前をTaskInitiatedなどに変える）
  , DomainEvent(..) -- イベントソーシング用のレコード型
  , getEventType
  , eventTypeToString
  , eventTypeFromString
    -- 移行用
  , legacyToNewEvent
  , newToLegacyEvent
  ) where

import Domain.Todo.ValueObject

import Data.Time
import GHC.Generics

data TodoEvent -- later deprecated
  = TaskInitiated TaskId TaskDescription UTCTime -- 動詞的な名称で型と一貫していない
  | TaskCompleted TaskId UTCTime
  | TaskReopened TaskId UTCTime
  | TaskDeleted TaskId UTCTime
  deriving (Show, Eq, Generic)

-- === ADT for Event Types ===
data EventType
  = EventTaskInitiated
  | EventTaskCompleted
  | EventTaskReopened
  | EventTaskDeleted
  deriving (Show, Eq, Enum, Bounded)

-- ドメインイベント（データとタイプを分離）
data DomainEvent = DomainEvent
  { eventType :: EventType
  , eventTaskId :: TaskId
  , eventDescription :: Maybe TaskDescription -- TaskInitiateの時のみ必要なためMaybe型
  , eventTimestamp :: UTCTime
  } deriving (Show, Eq)

-- === 相互変換（移行用） ===

-- 旧Event -> 新Event
legacyToNewEvent :: TodoEvent -> DomainEvent
legacyToNewEvent (TaskInitiated tid desc time)
  = DomainEvent
    { eventType = EventTaskInitiated
    , eventTaskId = tid
    , eventDescription = Just desc
    , eventTimestamp = time
    }
legacyToNewEvent (TaskCompleted tid time)
  = DomainEvent
    { eventType = EventTaskCompleted
    , eventTaskId = tid
    , eventDescription = Nothing
    , eventTimestamp = time
    }
legacyToNewEvent (TaskReopened tid time)
  = DomainEvent
    { eventType = EventTaskReopened
    , eventTaskId = tid
    , eventDescription = Nothing
    , eventTimestamp = time
    }
legacyToNewEvent (TaskDeleted tid time)
  = DomainEvent
    { eventType = EventTaskDeleted
    , eventTaskId = tid
    , eventDescription = Nothing
    , eventTimestamp = time
    }

-- 新Event -> 旧Event（後方互換性のため）
newToLegacyEvent :: DomainEvent -> Maybe TodoEvent
newToLegacyEvent (DomainEvent EventTaskInitiated tid (Just desc) time)
  = Just $ TaskInitiated tid desc time
newToLegacyEvent (DomainEvent EventTaskCompleted tid _ time)
  = Just $ TaskCompleted tid time
newToLegacyEvent (DomainEvent EventTaskReopened tid _ time)
  = Just $ TaskReopened tid time
newToLegacyEvent (DomainEvent EventTaskDeleted tid _ time)
  = Just $ TaskDeleted tid time
newToLegacyEvent _ = Nothing -- 不正なデータの組み合わせの場合

-- TodoEventからEventTypeを取得（互換性のため）
getEventType :: TodoEvent -> EventType
getEventType (TaskInitiated {}) = EventTaskInitiated
getEventType (TaskCompleted {}) = EventTaskCompleted
getEventType (TaskReopened {}) = EventTaskReopened
getEventType (TaskDeleted {}) = EventTaskDeleted

-- 外部表現への変換（Application層境界で使用, ただのStringだと型安全でなくなることに注意）
eventTypeToString :: EventType -> String
eventTypeToString EventTaskInitiated = "TaskInitiated"
eventTypeToString EventTaskCompleted = "TaskCompleted"
eventTypeToString EventTaskReopened = "TaskReopened"
eventTypeToString EventTaskDeleted = "TaskDeleted"

-- 外部表現からのパース（互換性のため）
eventTypeFromString :: String -> Maybe EventType
eventTypeFromString "TaskInitiated" = Just EventTaskInitiated
eventTypeFromString "TaskCompleted" = Just EventTaskCompleted
eventTypeFromString "TaskReopened" = Just EventTaskReopened
eventTypeFromString "TaskDeleted" = Just EventTaskDeleted
-- 後方互換性
eventTypeFromString "TodoCreated" = Just EventTaskInitiated
eventTypeFromString "TodoCompleted" = Just EventTaskCompleted
eventTypeFromString "TodoUncompleted" = Just EventTaskReopened
eventTypeFromString "TodoDeleted" = Just EventTaskDeleted
-- 不正な文字列
eventTypeFromString _ = Nothing
