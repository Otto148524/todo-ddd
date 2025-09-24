{-# LANGUAGE DeriveGeneric #-}

module Domain.Todo.Entity
  ( Todo(..)
  ) where

import Domain.Todo.ValueObject

import GHC.Generics

data Todo = Todo
  { todoId :: TodoId
  , text :: TodoText
  , completed :: Bool
  } deriving (Show, Eq, Generic)
