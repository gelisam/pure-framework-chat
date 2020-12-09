{-# LANGUAGE ScopedTypeVariables #-}
module PureFramework.TUI.Threads where

import Control.Concurrent.Async
import Control.Concurrent.STM
import Control.Concurrent.STM.TMQueue
import Control.Monad
import Data.Binary (Binary)
import Data.Foldable

import ImperativeVty
import PureFramework.ClientServer.Types
import PureFramework.Threads


-- All the functions in this file are intended to be spawned by withAsync.


-- Emits the keys as the user types them.
keyboardThread
  :: TQueue Key  -- output channel
  -> IO ()
keyboardThread = inputThread waitForKey

-- Emits the initial world, then closes the channel once the user presses a key.
pressAnyKeyThread
  :: world
  -> TMQueue world  -- output channel, closed on the first keypress
  -> IO ()
pressAnyKeyThread world outQueue = do
  atomically $ writeTMQueue outQueue world
  _ <- waitForKey
  atomically $ closeTMQueue outQueue

-- Receives toClient messages and keypresses, applies pure handlers, emits
-- worlds and toServer messages.
pureClientTuiThread
  :: forall world toClient toServer. (Binary toClient, Binary toServer)
  => ([toServer], world)
  -> ( world
    -> Key
    -> Maybe ([toServer], world)
     )  -- handle key presses
  -> ( world
    -> Maybe toClient
    -> Maybe ([toServer], world)
     )  -- handle messages from the server
  -> TQueue Key  -- input channel
  -> TMQueue toClient  -- input channel
  -> TQueue toServer  -- output channel
  -> TMQueue world  -- output channel, closed on Nothing
  -> IO ()
pureClientTuiThread (msgs0, world0) handleKey handleToClient
                    keyQueue toClientQueue toServerQueue worldQueue
                  = do
  for_ msgs0 $ \toServer -> do
    atomically $ writeTQueue toServerQueue toServer
  anythingQueue <- newTQueueIO
  let toClientThread = do
        forever $ do
          maybeToClient <- atomically $ readTMQueue toClientQueue
          atomically $ writeTQueue anythingQueue
                     $ Right maybeToClient
  let handleAnything
        :: world
        -> Either Key (Maybe toClient)
        -> IO (Maybe world)
      handleAnything world input = do
        let output = case input of
              Left key
                -> handleKey world key
              Right maybeToClient
                -> handleToClient world maybeToClient
        case output of
          Just (msgs, world') -> do
            for_ msgs $ \toServer -> do
              atomically $ writeTQueue toServerQueue toServer
            pure $ Just world'
          Nothing -> do
            pure Nothing
  withAsync (mappingThread
               Left
               keyQueue
               anythingQueue) $ \_ -> do
    withAsync toClientThread $ \_ -> do
      foldingThread
        world0
        handleAnything
        anythingQueue
        worldQueue

-- Receives server events and keypresses, applies pure handlers, emits
-- worlds and messages to clients.
pureServerTuiThread
  :: forall world toClient toServer. (Binary toClient, Binary toServer)
  => world
  -> ( world
    -> Key
    -> Maybe ([SendToClient toClient], world)
     )  -- handle key presses
  -> ( world
    -> ServerEvent toServer
    -> Maybe ([SendToClient toClient], world)
     )  -- handle messages from the clients
  -> TQueue Key  -- input channel
  -> TQueue (ServerEvent toServer)  -- input channel
  -> TQueue (SendToClient toClient)  -- output channel
  -> TMQueue world  -- output channel, closed on Nothing
  -> IO ()
pureServerTuiThread world0 handleKey handleToServer
                    keyQueue toServerQueue toClientQueue worldQueue
                  = do
  anythingQueue <- newTQueueIO
  let toServerThread = do
        forever $ do
          toServer <- atomically $ readTQueue toServerQueue
          atomically $ writeTQueue anythingQueue
                     $ Right toServer
  let handleAnything
        :: world
        -> Either Key (ServerEvent toServer)
        -> IO (Maybe world)
      handleAnything world input = do
        let output = case input of
              Left key
                -> handleKey world key
              Right toServer
                -> handleToServer world toServer
        case output of
          Just (msgs, world') -> do
            for_ msgs $ \toClient -> do
              atomically $ writeTQueue toClientQueue toClient
            pure $ Just world'
          Nothing -> do
            pure Nothing
  withAsync (mappingThread
               Left
               keyQueue
               anythingQueue) $ \_ -> do
    withAsync toServerThread $ \_ -> do
      foldingThread
        world0
        handleAnything
        anythingQueue
        worldQueue
