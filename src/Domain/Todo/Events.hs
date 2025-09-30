module Domain.Todo.Events
  (
    EventType(..) -- TodoEventからこちらに移行予定（その時に型の名前をTaskInitiatedなどに変える）
  , DomainEvent(..) -- イベントソーシング用のレコード型
  , eventTypeToString
  , eventTypeFromString
  ) where

import Domain.Todo.ValueObject

import Data.Time

-- === ADT for Event Types ===
data EventType
  = TaskInitiated
  | TaskCompleted
  | TaskReopened
  | TaskDeleted
  deriving (Show, Eq, Enum, Bounded)

-- ドメインイベント（データとタイプを分離）
data DomainEvent = DomainEvent
  { eventType :: EventType
  , eventTaskId :: TaskId
  , eventDescription :: Maybe TaskDescription -- TaskInitiateの時のみ必要なためMaybe型
  , eventTimestamp :: UTCTime
  } deriving (Show, Eq)

-- 外部表現への変換（Application層境界で使用, ただのStringだと型安全でなくなることに注意）
eventTypeToString :: EventType -> String
eventTypeToString TaskInitiated = "TaskInitiated"
eventTypeToString TaskCompleted = "TaskCompleted"
eventTypeToString TaskReopened = "TaskReopened"
eventTypeToString TaskDeleted = "TaskDeleted"

-- 外部表現からのパース（互換性のため）
eventTypeFromString :: String -> Maybe EventType
eventTypeFromString "TaskInitiated" = Just TaskInitiated
eventTypeFromString "TaskCompleted" = Just TaskCompleted
eventTypeFromString "TaskReopened" = Just TaskReopened
eventTypeFromString "TaskDeleted" = Just TaskDeleted
-- 後方互換性
eventTypeFromString "TodoCreated" = Just TaskInitiated
eventTypeFromString "TodoCompleted" = Just TaskCompleted
eventTypeFromString "TodoUncompleted" = Just TaskReopened
eventTypeFromString "TodoDeleted" = Just TaskDeleted
-- 不正な文字列
eventTypeFromString _ = Nothing
