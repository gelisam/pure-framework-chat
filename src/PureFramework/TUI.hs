{-# LANGUAGE LambdaCase, OverloadedStrings, RecordWildCards, ScopedTypeVariables, ViewPatterns #-}
module PureFramework.TUI
  ( module PureFramework.TUI
  , Key(..)
  ) where

import Control.Concurrent.Async
import Control.Concurrent.STM
import Control.Concurrent.STM.TMQueue
import Control.Monad
import Data.Function
import System.Environment (getArgs, getProgName)
import System.Exit (exitFailure)
--import qualified Data.ByteString as ByteString
import qualified Network.Simple.TCP as TCP

import ImperativeVty


data TextPicture
  = Text String
  | Translated (Int, Int) TextPicture
  | Over TextPicture TextPicture

instance Semigroup TextPicture where
  (<>) = Over

instance Monoid TextPicture where
  mempty = Text ""

drawTextPicture :: TextPicture -> IO ()
drawTextPicture = go (0, 0)
  where
    go :: (Int, Int) -> TextPicture -> IO ()
    go (x, y) = \case
      Text s -> do
        putStrAt (x, y) s
      Translated (dx, dy) pic -> do
        go (x + dx, y + dy) pic
      Over pic1 pic2 -> do
        go (x, y) pic1
        go (x, y) pic2

displayTUI :: ((Int, Int) -> TextPicture) -> IO ()
displayTUI mkTextPicture = withTerminal $ do
  clearScreen
  screenSize <- getScreenSize
  drawTextPicture (mkTextPicture screenSize)
  void waitForKey

-- A variant of playTUI in which the current world is controlled
-- programmatically via a TMQueue instead of being controlled by the user's
-- keypresses.
roboPlayTUI
  :: (world -> (Int, Int) -> TextPicture)
  -> TMQueue world  -- input channel, close to quit
  -> IO ()
roboPlayTUI mkTextPicture worldQueue = do
  screenSize <- getScreenSize
  fix $ \loop -> do
    atomically (readTMQueue worldQueue) >>= \case
      Just world -> do
        clearScreen
        drawTextPicture (mkTextPicture world screenSize)
        loop
      Nothing -> do
        -- quit
        pure ()

-- Emits Keys as they are pressed. More reusable than if it was emitting worlds.
-- Intended to be spawned by withAsync.
keyboardThread
  :: TQueue Key  -- output channel
  -> IO ()
keyboardThread keyQueue = do
  key <- waitForKey
  atomically $ writeTQueue keyQueue key
  keyboardThread keyQueue

-- Receives Keys, emits worlds.
-- Intended to be spawned by withAsync.
keyHandlingThread
  :: world
  -> (world -> Key -> Maybe world)
  -> TQueue Key  -- input channel
  -> TMQueue world  -- output channel, closed on Nothing
  -> IO ()
keyHandlingThread world handleKey keyQueue worldQueue = do
  atomically $ writeTMQueue worldQueue world
  key <- atomically $ readTQueue keyQueue
  case handleKey world key of
    Just world' -> do
      keyHandlingThread world' handleKey keyQueue worldQueue
    Nothing -> do
      atomically $ closeTMQueue worldQueue

playTUI
  :: forall world
   . world
  -> (world -> (Int, Int) -> TextPicture)
  -> (world -> Key -> Maybe world)
  -> IO ()
playTUI world0 mkTextPicture handleKey = withTerminal $ do
  keyQueue <- newTQueueIO
  worldQueue <- newTMQueueIO
  withAsync (keyboardThread keyQueue) $ \_ -> do
    withAsync (keyHandlingThread world0 handleKey keyQueue worldQueue) $ \_ -> do
      roboPlayTUI mkTextPicture worldQueue

multiplayTUI
  :: world
  -> (world -> Int -> (Int, Int) -> TextPicture)
  -> (world -> Int -> Key -> Maybe world)
  -> IO ()
multiplayTUI _world0 _mkTextPicture _handleKey = do
  getArgs >>= \case
    ["server", host, port] -> do
      putStrLn "spinning server..."
      TCP.listen (TCP.Host host) port $ \(acceptor, _) -> do
        putStrLn "accepting connections..."
        TCP.accept acceptor $ \(client, _) -> do
          putStrLn "connected."
          TCP.send client "hello from server"
          msg <- TCP.recv client 1024
          putStrLn $ "client says: " ++ show msg
    ["client", host, port] -> do
      putStrLn "connecting..."
      TCP.connect host port $ \(server, _) -> do
        putStrLn "connected."
        TCP.send server "hello from client"
        msg <- TCP.recv server 1024
        putStrLn $ "server says: " ++ show msg
    _ -> do
      progName <- getProgName
      putStrLn "usage:"
      putStrLn $ "  " ++ progName ++ " server 0.0.0.0 PORT"
      putStrLn $ "  " ++ progName ++ " client SERVER_IP PORT"
      exitFailure
