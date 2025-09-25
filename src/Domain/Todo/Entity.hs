{-# LANGUAGE DeriveGeneric #-}

module Domain.Todo.Entity
  ( Task(..)
  ) where

import Domain.Todo.ValueObject

import GHC.Generics

data Task = Task
  { taskId :: TaskId
  , taskDescription :: TaskDescription
  , isCompleted :: Bool
  } deriving (Show, Eq, Generic)
