module Application.Ports.EventStorePort
  ( EventStore(..)
  ) where

import Application.DTO.TaskDTO

class (Monad m) => EventStore m where
  appendEventDto :: TodoEventDTO -> m ()
  getAllEventDtos :: m [TodoEventDTO]
