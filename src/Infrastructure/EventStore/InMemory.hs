{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE InstanceSigs #-}

module Infrastructure.EventStore.InMemory
  ( AppConfig(..)
  , AppM(..)
  , AppIO(..)
  , runAppM
  , runAppIO
  , initializeApp
  ) where

import Application.DTO.TaskDTO
import Application.Ports.EventStorePort
import Application.Ports.NotificationPort

import Control.Concurrent.STM
import Control.Monad.IO.Class
import Control.Monad.Reader
import Servant

data AppConfig = AppConfig
  { eventStoreTVar :: TVar [TodoEventDTO]
  , notificationChannel :: TChan (String, TodoEventDTO)
  }

newtype AppM a = AppM (ReaderT AppConfig Handler a)
  deriving (Functor, Applicative, Monad, MonadIO, MonadReader AppConfig)

newtype AppIO a = AppIO (ReaderT AppConfig IO a)
  deriving (Functor, Applicative, Monad, MonadIO, MonadReader AppConfig)

runAppM :: AppConfig -> AppM a -> Handler a
runAppM config (AppM action) = runReaderT action config

runAppIO :: AppConfig -> AppIO a -> IO a
runAppIO config (AppIO action) = runReaderT action config

instance EventStore AppM where
  appendEventDto eventDto = do
    config <- ask
    liftIO $
      atomically $
        modifyTVar' (eventStoreTVar config) (++ [eventDto])

  getAllEventDtos = do
    config <- ask
    liftIO $ readTVarIO (eventStoreTVar config)

instance EventStore AppIO where
  appendEventDto eventDto = do
    config <- ask
    liftIO $
      atomically $
        modifyTVar' (eventStoreTVar config) (++ [eventDto])

  getAllEventDtos = do
    config <- ask
    liftIO $ readTVarIO (eventStoreTVar config)

instance NotificationPort AppM where
  notifyEventDto eventType eventDto = do
    config <- ask
    liftIO $
      atomically $
        writeTChan (notificationChannel config) (eventType, eventDto)

instance NotificationPort AppIO where
  notifyEventDto :: String -> TodoEventDTO -> AppIO ()
  notifyEventDto eventType eventDto = do
    config <- ask
    liftIO $
      atomically $
        writeTChan (notificationChannel config) (eventType, eventDto)

initializeApp :: IO AppConfig
initializeApp = do
  eventStore <- newTVarIO []
  AppConfig eventStore <$> newBroadcastTChanIO
