{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

module Domain.Todo.Aggregate
  ( TodoDomainFacade(..)
  , mkTodoDomainFacade
  , TodoCreationRequest(..)
  , TodoUpdateRequest(..)
  , DomainTodoView(..)
  , DomainEventView(..)
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



-- ドメインエラーの統一定義
data DomainError
  = InvalidTodoId Text
  | InvalidTodoText Text
  | TodoNotFound Text
  | DomainLogicError Text
  deriving (Show, Eq)

-- 外部向けのビュー型（DTOの代わり）
data DomainTodoView = DomainTodoView
  { todoViewId :: String
  , todoViewText :: String
  , todoViewCompleted :: Bool
  } deriving (Show, Eq)

data DomainEventView = DomainEventView
  { eventViewType :: String
  , eventViewTodoId :: String
  , eventViewTodoText :: Maybe String
  , eventViewTimestamp :: UTCTime
  } deriving (Show, Eq)

data TodoCreationRequest = TodoCreationRequest
  { creationRequestId :: String
  , creationRequestText :: String
  , creationRequestTimestamp :: UTCTime
  } deriving (Show, Eq)

data TodoUpdateRequest = TodoUpdateRequest
  { updateRequestId :: String
  , updateRequestTimestamp :: UTCTime
  } deriving (Show, Eq)

-- DTO変換サポート型
data DTOConversionSupport = DTOConversionSupport
  { -- TodoDTO <-> DomainTodoView 変換
    todoDtoToDomainView :: (String, String, Bool) -> DomainTodoView
  , domainViewToTodoDto :: DomainTodoView -> (String, String, Bool)

    -- TodoEventDTO <-> DomainEventView 変換
  , todoEventDtoToDomainView :: (String, String, Maybe String, UTCTime) -> DomainEventView
  , domainViewToTodoEventDto :: DomainEventView -> (String, String, Maybe String, UTCTime)

    -- statisticsDTO変換
  , statisticsTupleToDto :: (Int, Int, Int) -> (Int, Int, Int) -- そのまま
  }

-- 統一インターフェース
data TodoDomainFacade = TodoDomainFacade
  { -- Todo作成
    createTodoFromRequest :: TodoCreationRequest -> Either DomainError TodoEvent

    -- Todo状態変更
  , completeTodoFromRequest :: TodoUpdateRequest -> Either DomainError TodoEvent
  , uncompleteTodoFromRequest :: TodoUpdateRequest -> Either DomainError TodoEvent
  , deleteTodoFromRequest :: TodoUpdateRequest -> Either DomainError TodoEvent

    -- イベント投影とクエリ
  , projectEventsToViews :: [TodoEvent] -> [DomainTodoView]
  , findTodoById :: String -> [TodoEvent] -> Maybe DomainTodoView
  , getTodoStatistics :: [TodoEvent] -> (Int, Int, Int) -- (total, completed, active)

    -- イベント変換
  , eventToView :: TodoEvent -> DomainEventView
  , eventsToViews :: [TodoEvent] -> [DomainEventView]

    -- バリデーション
  , validateTodoId :: String -> Either DomainError TodoId
  , validateTodoText :: String -> Either DomainError TodoText

    -- DomainEventViewからDomainTodoViewへの直接変換
  , projectEventViewsDirectly :: [DomainEventView] -> [DomainTodoView]

    -- DTO変換サポート
  , dtoConversion :: DTOConversionSupport
  }

-- ファサードの実装
mkTodoDomainFacade :: TodoDomainFacade
mkTodoDomainFacade = TodoDomainFacade
  { createTodoFromRequest = \req -> do
      todoId' <- validateTodoIdStr (creationRequestId req)
      todoText' <- validateTodoTextStr (creationRequestText req)
      return $ TodoCreated todoId' todoText' (creationRequestTimestamp req)
  , completeTodoFromRequest = \req -> do
      todoId' <- validateTodoIdStr (updateRequestId req)
      return $ TodoCompleted todoId' (updateRequestTimestamp req)
  , uncompleteTodoFromRequest = \req -> do
      todoId' <- validateTodoIdStr (updateRequestId req)
      return $ TodoUncompleted todoId' (updateRequestTimestamp req)
  , deleteTodoFromRequest = \req -> do
      todoId' <- validateTodoIdStr (updateRequestId req)
      return $ TodoDeleted todoId' (updateRequestTimestamp req)
  , projectEventsToViews = Map.elems . Map.map todoToView . projectEvents
  , findTodoById = \targetId events ->
      let todos = projectEventsToViews mkTodoDomainFacade events
      in case filter (\t -> todoViewId t == targetId) todos of
        (todo:_) -> Just todo
        [] -> Nothing
  , getTodoStatistics = \events ->
    let todos = projectEventsToViews mkTodoDomainFacade events
        total = length todos
        completed' = length $ filter todoViewCompleted todos
        active = total - completed'
    in (total, completed', active)
  , eventToView = \case
      TodoCreated tid txt timestamp -> DomainEventView
        { eventViewType = "TodoCreated"
        , eventViewTodoId = toIdString tid
        , eventViewTodoText = Just $ toTextString txt
        , eventViewTimestamp = timestamp
        }
      TodoCompleted tid timestamp -> DomainEventView
        { eventViewType = "TodoCompleted"
        , eventViewTodoId = toIdString tid
        , eventViewTodoText = Nothing
        , eventViewTimestamp = timestamp
        }
      TodoUncompleted tid timestamp -> DomainEventView
        { eventViewType = "TodoUncompleted"
        , eventViewTodoId = toIdString tid
        , eventViewTodoText = Nothing
        , eventViewTimestamp = timestamp
        }
      TodoDeleted tid timestamp -> DomainEventView
        { eventViewType = "TodoDeleted"
        , eventViewTodoId = toIdString tid
        , eventViewTodoText = Nothing
        , eventViewTimestamp = timestamp
        }
  , eventsToViews = map (eventToView mkTodoDomainFacade)
  , validateTodoId = validateTodoIdStr
  , validateTodoText = validateTodoTextStr
  , projectEventViewsDirectly = \eventViews ->
      let convertViewToEvent view = case eventViewType view of
            "TodoCreated" -> case eventViewTodoText view of
              Just txt ->
                let req = TodoCreationRequest (eventViewTodoId view) txt (eventViewTimestamp view)
                in case createTodoFromRequest mkTodoDomainFacade req of
                  Left _ -> Nothing
                  Right event -> Just event
              Nothing -> Nothing
            "TodoCompleted" ->
              let req = TodoUpdateRequest (eventViewTodoId view) (eventViewTimestamp view)
              in case completeTodoFromRequest mkTodoDomainFacade req of
                Left _ -> Nothing
                Right event -> Just event
            "TodoUncompleted" ->
              let req = TodoUpdateRequest (eventViewTodoId view) (eventViewTimestamp view)
              in case uncompleteTodoFromRequest mkTodoDomainFacade req of
                Left _ -> Nothing
                Right event -> Just event
            "TodoDeleted" ->
              let req = TodoUpdateRequest (eventViewTodoId view) (eventViewTimestamp view)
              in case deleteTodoFromRequest mkTodoDomainFacade req of
                Left _ -> Nothing
                Right event -> Just event
            _ -> Nothing
          events = mapMaybe convertViewToEvent eventViews
          in projectEventsToViews mkTodoDomainFacade events

    -- DTO変換サポートの実装
  , dtoConversion = DTOConversionSupport
      { todoDtoToDomainView = \(id', text', completed') ->
          DomainTodoView
            { todoViewId = id'
            , todoViewText = text'
            , todoViewCompleted = completed'
            }
      , domainViewToTodoDto = \view ->
        (todoViewId view, todoViewText view, todoViewCompleted view)
      , todoEventDtoToDomainView = \(eventType, todoId', mText, timestamp) -> DomainEventView
          { eventViewType = eventType
          , eventViewTodoId = todoId'
          , eventViewTodoText = mText
          , eventViewTimestamp = timestamp
          }
      , domainViewToTodoEventDto = \view ->
          (eventViewType view, eventViewTodoId view, eventViewTodoText view, eventViewTimestamp view)
      , statisticsTupleToDto = id -- (Int, Int, Int) -> (Int, Int, Int)
      }
  }

-- 内部ヘルパー関数
validateTodoIdStr :: String -> Either DomainError TodoId
validateTodoIdStr idStr =
  case mkTodoId idStr of
    Left err -> Left $ InvalidTodoId err
    Right todoId' -> Right todoId'

validateTodoTextStr :: String -> Either DomainError TodoText
validateTodoTextStr textStr =
  case mkTodoText textStr of
    Left err -> Left $ InvalidTodoText err
    Right todoText' -> Right todoText'

-- TodoエンティティをDomainTodoViewに変換する関数
todoToView :: Todo -> DomainTodoView
todoToView todo = DomainTodoView
  { todoViewId = toIdString (todoId todo)
  , todoViewText = toTextString (text todo)
  , todoViewCompleted = completed todo
  }

-- 元々Aggregate.hsにあったものを移してきた
projectEvents :: [TodoEvent] -> Map TodoId Todo
projectEvents = foldl' applyEvent Map.empty
  where
    applyEvent :: Map TodoId Todo -> TodoEvent -> Map TodoId Todo
    applyEvent todos' (TodoCreated tid txt _) =
      Map.insert tid (Todo tid txt False) todos'
    applyEvent todos' (TodoCompleted tid _) =
      Map.adjust (\t -> t{completed = True}) tid todos'
    applyEvent todos' (TodoUncompleted tid _) =
      Map.adjust (\t -> t{completed = False}) tid todos'
    applyEvent todos' (TodoDeleted tid _) =
      Map.delete tid todos'
