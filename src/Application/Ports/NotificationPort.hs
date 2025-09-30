module Application.Ports.NotificationPort
  ( NotificationPort(..)
  ) where

import Application.DTO.TaskDTO

class (Monad m) => NotificationPort m where
  notifyEventDto :: String -> TodoEventDTO -> m ()
