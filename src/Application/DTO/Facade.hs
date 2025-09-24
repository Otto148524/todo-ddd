{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

module Application.DTO.Facade
  ( DomainOperations(..)
  , domainOps
  ) where

import Domain.Todo.Aggregate

import Application.DTO.TodoDTO

import Data.Text (Text)
import Data.Time (UTCTime)

-- ドメイン層の関数のファサード
data DomainOperations = DomainOperations
  { -- Todo作成・更新（DTOベース）
    createTodoDTO :: String -> String -> UTCTime -> Either Text TodoEventDTO
  , completeTodoDTO :: String -> UTCTime -> Either Text TodoEventDTO
  , uncompleteTodoDTO :: String -> UTCTime -> Either Text TodoEventDTO
  , deleteTodoDTO :: String -> UTCTime -> Either Text TodoEventDTO

    -- クエリ（DTOベース）
  , getAllTodosDTO :: [TodoEventDTO] -> [TodoDTO]
  , findTodoDTOById :: String -> [TodoEventDTO] -> Maybe TodoDTO
  , getStatisticsDTO :: [TodoEventDTO] -> TodoStatisticsDTO

    -- Event変換（DTOベース）
  , eventDTOsFromDomainEvents :: [TodoEventDTO] -> [TodoEventDTO] -- id
  }

-- ファサードの実装
domainOps :: DomainOperations
domainOps = DomainOperations
  { createTodoDTO = \todoId' text' timestamp ->
    let req = TodoCreationRequest todoId' text' timestamp
        facade = mkTodoDomainFacade
    in case createTodoFromRequest facade req of
      Left (InvalidTodoId err) -> Left err
      Left (InvalidTodoText err) -> Left err
      Left (DomainLogicError err) -> Left err
      Left (TodoNotFound err) -> Left err
      Right domainEvent ->
        let domainView = eventToView facade domainEvent
            (eventType, eid, mText, ts) = domainViewToTodoEventDto (dtoConversion facade) domainView
        in case eventType of
          "TodoCreated" -> Right $ TodoCreatedDTO eid (maybe "" id mText) ts
          _ -> Left "Unexpected event type"
  , completeTodoDTO = \todoId' timestamp ->
      let req = TodoUpdateRequest todoId' timestamp
          facade = mkTodoDomainFacade
      in case completeTodoFromRequest facade req of
        Left (InvalidTodoId err) -> Left err
        Left (InvalidTodoText err) -> Left err
        Left (DomainLogicError err) -> Left err
        Left (TodoNotFound err) -> Left err
        Right domainEvent ->
          let domainView = eventToView facade domainEvent
              (_, eid, _, ts) = domainViewToTodoEventDto (dtoConversion facade) domainView
          in Right $ TodoCompletedDTO eid ts
  , uncompleteTodoDTO = \todoId' timestamp ->
      let req = TodoUpdateRequest todoId' timestamp
          facade = mkTodoDomainFacade
      in case uncompleteTodoFromRequest facade req of
        Left (InvalidTodoId err) -> Left err
        Left (InvalidTodoText err) -> Left err
        Left (DomainLogicError err) -> Left err
        Left (TodoNotFound err) -> Left err
        Right domainEvent ->
          let domainView = eventToView facade domainEvent
              (_, eid, _, ts) = domainViewToTodoEventDto (dtoConversion facade) domainView
          in Right $ TodoUncompletedDTO eid ts
  , deleteTodoDTO = \todoId' timestamp ->
      let req = TodoUpdateRequest todoId' timestamp
          facade = mkTodoDomainFacade
      in case deleteTodoFromRequest facade req of
        Left (InvalidTodoId err) -> Left err
        Left (InvalidTodoText err) -> Left err
        Left (DomainLogicError err) -> Left err
        Left (TodoNotFound err) -> Left err
        Right domainEvent ->
          let domainView = eventToView facade domainEvent
              (_, eid, _, ts) = domainViewToTodoEventDto (dtoConversion facade) domainView
          in Right $ TodoDeletedDTO eid ts
  , getAllTodosDTO = \eventDtos ->
      let facade = mkTodoDomainFacade
          -- DTOからDomainViewに変換
          domainViews = map (\case
              TodoCreatedDTO eid txt ts -> todoEventDtoToDomainView (dtoConversion facade) ("TodoCreated", eid, Just txt, ts)
              TodoCompletedDTO eid ts -> todoEventDtoToDomainView (dtoConversion facade) ("TodoCompleted", eid, Nothing, ts)
              TodoUncompletedDTO eid ts -> todoEventDtoToDomainView (dtoConversion facade) ("TodoUncompleted", eid, Nothing, ts)
              TodoDeletedDTO eid ts -> todoEventDtoToDomainView (dtoConversion facade) ("TodoDeleted", eid, Nothing, ts)
              ) eventDtos
          -- DomainViewからTodoViewに投影
          todoViews = projectEventViewsDirectly facade domainViews
          -- TodoViewからDTOに変換
      in map (\view ->
          let (eid, txt, completed') = domainViewToTodoDto (dtoConversion facade) view
          in TodoDTO eid txt completed') todoViews
  , findTodoDTOById = \targetId eventDtos ->
      case getAllTodosDTO domainOps eventDtos of
        todos' -> case filter (\dto -> todoDtoId dto == targetId) todos' of
          (todo:_) -> Just todo
          [] -> Nothing
  , getStatisticsDTO = \eventDtos ->
      let todos' = getAllTodosDTO domainOps eventDtos
          total = length todos'
          completed' = length $ filter todoDtoCompleted todos'
          active = total - completed'
      in TodoStatisticsDTO total active completed'
  , eventDTOsFromDomainEvents = id
  }
