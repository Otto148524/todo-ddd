{-# LANGUAGE DeriveGeneric #-}

module Domain.Todo.Events
  ( TodoEvent(..)
  ) where

import Domain.Todo.ValueObject

import Data.Time
import GHC.Generics

data TodoEvent
  = TodoCreated TodoId TodoText UTCTime
  | TodoCompleted TodoId UTCTime
  | TodoUncompleted TodoId UTCTime
  | TodoDeleted TodoId UTCTime
  deriving (Show, Eq, Generic)
