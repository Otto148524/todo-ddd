{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Domain.Todo.ValueObject
  ( TodoId(..)
  , TodoText(..)
  , mkTodoId
  , mkTodoText
  , IdValue(..)
  , TextValue(..)
  ) where

import Data.Text (Text)
import GHC.Generics

-- Value Objects
newtype TodoId = TodoId {unTodoId :: String }
  deriving (Show, Eq, Ord, Generic)

newtype TodoText = TodoText { unTodoText :: String }
  deriving (Show, Eq, Generic)

-- Smart constructors
mkTodoId :: String -> Either Text TodoId
mkTodoId tid
  | null tid = Left "TodoId cannot be empty"
  | otherwise = Right (TodoId tid)

mkTodoText :: String -> Either Text TodoText
mkTodoText txt
  | null txt = Left "TodoText cannot be empty"
  | otherwise = Right (TodoText txt)

-- Type classes fro abstraction
class IdValue a where
  toIdString :: a -> String
  fromIdString :: String -> Either Text a

class TextValue a where
  toTextString :: a -> String
  fromTextString :: String -> Either Text a

instance IdValue TodoId where
  toIdString = unTodoId
  fromIdString = mkTodoId

instance TextValue TodoText where
  toTextString = unTodoText
  fromTextString = mkTodoText
