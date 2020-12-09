{-# LANGUAGE LambdaCase #-}
module PureFramework.Threads where

import Control.Concurrent.STM
import Control.Concurrent.STM.TMQueue
import Control.Monad


-- A first attempt at making composable pure frameworks. For example, I would
-- like to take a pure framework for interacting with a TUI and to transform it
-- into a pure framework for a client-server program whose frontend is a TUI.
-- This is more complicated than just providing one more source of input (the
-- network packets), because I also want the TUI to be optimistically updated
-- based on the input which the user has typed but which the server has not yet
-- confirmed.
--
-- My plan is to split the logic for those pure frameworks into many tiny
-- threads which process incoming data and emit outcoming data. This way I get
-- to reuse those bits and pieces, which is not quite the same as composition,
-- but it's a start.
--
-- The plan is that the main thread will spawn and connect a bunch of threads
-- which watch various kinds of user input and eventually combine them into a
-- stream of worlds, and then that main thread will call a, err, let's call it
-- a "stream framework" such as roboPlayTUI, which is controlled by a stream of
-- worlds rather than user input. Once roboPlayTUI decides to quit, all the
-- other threads get cancelled because they've all been forked via withAsync.


-- All the functions in this file are intended to be spawned by withAsync.


-- Runs a blocking IO action over and over, emitting the results.
inputThread
  :: IO a
  -> TQueue a  -- output channel
  -> IO ()
inputThread waitForValue outQueue = do
  forever $ do
    a <- waitForValue
    atomically $ writeTQueue outQueue a

-- Run an IO action on every value received.
outputThread
  :: (a -> IO ())
  -> TQueue a  -- input channel
  -> IO ()
outputThread actOnValue inQueue = do
  forever $ do
    a <- atomically $ readTQueue inQueue
    actOnValue a

-- Like foldl', but with a TQueue instead of a list, and a Nothing closes the
-- output channel.
foldingThread
  :: b
  -> (b -> a -> IO (Maybe b))
  -> TQueue a  -- input channel
  -> TMQueue b  -- output channel, closed on Nothing
  -> IO ()
foldingThread b f inQueue outQueue = do
  atomically $ writeTMQueue outQueue b
  a <- atomically $ readTQueue inQueue
  f b a >>= \case
    Just b' -> do
      foldingThread b' f inQueue outQueue
    Nothing -> do
      atomically $ closeTMQueue outQueue

-- Like fmap, but with TQueue's instead of lists.
mappingThread
  :: (a -> b)
  -> TQueue a  -- input channel
  -> TQueue b  -- output channel
  -> IO ()
mappingThread f inQueue outQueue = do
  forever $ do
    a <- atomically $ readTQueue inQueue
    atomically $ writeTQueue outQueue (f a)
