{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Domain.Todo.ValueObject
  ( TaskId(..)
  , TaskDescription(..)
  , mkTaskId
  , mkTaskDescription
  , IdValue(..)
  , DescriptionValue(..)
  ) where

import Data.Text (Text)
import GHC.Generics

-- Value Objects
newtype TaskId = TaskId { getTaskId :: String }
  deriving (Show, Eq, Ord, Generic)

newtype TaskDescription = TaskDescription { getTaskDescription :: String }
  deriving (Show, Eq, Generic)

-- Smart constructors
mkTaskId :: String -> Either Text TaskId
mkTaskId tid
  | null tid = Left "TaskId is required"
  | otherwise = Right (TaskId tid)

mkTaskDescription :: String -> Either Text TaskDescription
mkTaskDescription desc
  | null desc = Left "Task description is required"
  | otherwise = Right (TaskDescription desc)

-- Type classes fro abstraction
class IdValue a where
  toIdString :: a -> String
  fromIdString :: String -> Either Text a

class DescriptionValue a where
  toDescString :: a -> String
  fromDescString :: String -> Either Text a

instance IdValue TaskId where
  toIdString = getTaskId
  fromIdString = mkTaskId

instance DescriptionValue TaskDescription where
  toDescString = getTaskDescription
  fromDescString = mkTaskDescription
