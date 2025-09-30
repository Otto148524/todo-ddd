module Domain.Todo.DomainService
  ( -- イベント投影
    projectEvents
  , projectToSnapshots
    -- スナップショット変換
  , taskToSnapshot
    -- クエリ
  , findTaskInProjection
  , takeStatistics
  , TaskSnapshot(..)
  , TaskStatistics(..)
  ) where

import Domain.Todo.Entity
import Domain.Todo.Events
import Domain.Todo.ValueObject

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.List (foldl')
import Data.Text (Text)


-- === Domain Error ===
data ProjectionError
  = MissingDescription TaskId
  | TaskNotFoundInProjection TaskId
  | ProjectionLogicError Text
  deriving (Show, Eq)

-- === スナップショット型（Read Model）===
data TaskSnapshot = TaskSnapshot
  { snapshotTaskId :: String
  , snapshotTaskDesc :: String
  , snapshotTaskIsCompleted :: Bool
  } deriving (Show, Eq)

-- === 統計情報型 ===
data TaskStatistics = TaskStatistics
  { totalTasks :: Int
  , completedTasks :: Int
  , activeTasks :: Int
  } deriving (Show, Eq)

-- === イベント投影（Event Sourcing） ===
projectEvents :: [DomainEvent] -> Either ProjectionError (Map TaskId Task)
projectEvents = foldl' applyEvent (Right Map.empty)
  where
    applyEvent :: Either ProjectionError (Map TaskId Task)
                -> DomainEvent
                -> Either ProjectionError (Map TaskId Task)
    applyEvent (Left err) _ = Left err
    applyEvent (Right tasks) event = case eventType event of

      TaskInitiated -> case eventDescription event of
        Just desc -> Right $ Map.insert (eventTaskId event) (Task (eventTaskId event) desc False) tasks
        Nothing -> Left $ MissingDescription (eventTaskId event)

      TaskCompleted ->
        if Map.member (eventTaskId event) tasks
        then Right $ Map.adjust (\t -> t{isCompleted = True}) (eventTaskId event) tasks
        else Left $ TaskNotFoundInProjection (eventTaskId event)

      TaskReopened ->
        if Map.member (eventTaskId event) tasks
        then Right $ Map.adjust (\t -> t{isCompleted = False}) (eventTaskId event) tasks
        else Left $ TaskNotFoundInProjection (eventTaskId event)

      TaskDeleted ->
        if Map.member (eventTaskId event) tasks
        then Right $ Map.delete (eventTaskId event) tasks
        else Left $ TaskNotFoundInProjection (eventTaskId event)

-- === スナップショット生成 ===
projectToSnapshots :: [DomainEvent] -> [TaskSnapshot]
projectToSnapshots events = case projectEvents events of
  Right tasks -> Map.elems $ Map.map taskToSnapshot tasks
  Left err -> error $ "Projection failed: " ++ show err


taskToSnapshot :: Task -> TaskSnapshot
taskToSnapshot task = TaskSnapshot
  { snapshotTaskId = toIdString (taskId task)
  , snapshotTaskDesc = toDescString (taskDescription task)
  , snapshotTaskIsCompleted = isCompleted task
  }

-- === クエリ関数 ===
findTaskInProjection :: String -> [DomainEvent] -> Maybe TaskSnapshot
findTaskInProjection targetId events =
  let snapshots = projectToSnapshots events
  in case filter (\s -> snapshotTaskId s == targetId) snapshots of
    (snapshot:_) -> Just snapshot
    [] -> Nothing

takeStatistics :: [DomainEvent] -> TaskStatistics
takeStatistics events =
  let snapshots = projectToSnapshots events
      total = length snapshots
      completed = length $ filter snapshotTaskIsCompleted snapshots
      active = total - completed
  in TaskStatistics total completed active
