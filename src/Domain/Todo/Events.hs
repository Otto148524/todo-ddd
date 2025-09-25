{-# LANGUAGE DeriveGeneric #-}

module Domain.Todo.Events
  ( TodoEvent(..)
  ) where

import Domain.Todo.ValueObject

import Data.Time
import GHC.Generics

data TodoEvent
  = TaskInitiated TaskId TaskDescription UTCTime
  | TaskCompleted TaskId UTCTime
  | TaskReopened TaskId UTCTime
  | TaskDeleted TaskId UTCTime
  deriving (Show, Eq, Generic)
