module Application.Ports.NotificationPort
  ( NotificationPort(..)
  ) where

import Application.DTO.TodoDTO

class (Monad m) => NotificationPort m where
  notifyEventDto :: String -> TodoEventDTO -> m ()
