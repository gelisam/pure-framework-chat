{-# LANGUAGE LambdaCase, OverloadedStrings, ScopedTypeVariables #-}
module PureFramework.TUI
  ( module PureFramework.ClientServer.Types
  , module PureFramework.TUI
  , module PureFramework.TUI.Types
  ) where

import Control.Concurrent.Async
import Control.Concurrent.STM
import Control.Concurrent.STM.TMQueue
import Data.Binary (Binary)
import System.Environment (getArgs, getProgName)
import System.Exit (exitFailure)
import qualified Network.Simple.TCP as TCP

import ImperativeVty
import PureFramework.ClientServer.Types
import PureFramework.Threads
import PureFramework.TUI.Threads
import PureFramework.TUI.Types
import PureFramework.TUI.StreamFramework
import qualified PureFramework.ClientServer.IOFramework as IOFramework


displayTUI :: ((Int, Int) -> TextPicture) -> IO ()
displayTUI mkTextPicture = withTerminal $ do
  worldQueue <- newTMQueueIO
  withAsync (pressAnyKeyThread () worldQueue) $ \_ -> do
    roboPlayTUI (\() -> mkTextPicture) worldQueue

playTUI
  :: forall world. world
  -> (world -> (Int, Int) -> TextPicture)
  -> (world -> Key -> Maybe world)
  -> IO ()
playTUI world0 mkTextPicture handleKey = withTerminal $ do
  keyQueue <- newTQueueIO
  worldQueue <- newTMQueueIO
  withAsync (keyboardThread keyQueue) $ \_ -> do
    let handleKeyIO :: world -> Key -> IO (Maybe world)
        handleKeyIO world key = do
          pure $ handleKey world key
    withAsync (foldingThread world0 handleKeyIO keyQueue worldQueue) $ \_ -> do
      roboPlayTUI mkTextPicture worldQueue

-- A pure framework for a program which only acts as a client.
-- Nothing to quit, otherwise update the world state and optionally send some
-- messages to the server.
clientTUI
  :: (Binary toClient, Binary toServer)
  => ([toServer], world)
  -> (world -> (Int, Int) -> TextPicture)
  -> ( world
    -> Key
    -> Maybe ([toServer], world)
     )  -- handle key presses
  -> ( world
    -> Maybe toClient
    -> Maybe ([toServer], world)
     )  -- handle messages from the server
  -> IO ()
clientTUI init0 mkTextPicture handleKey handleToClient = withTerminal $ do
  IOFramework.client $ \toClientQueue toServerQueue -> do
    keyQueue <- newTQueueIO
    worldQueue <- newTMQueueIO
    withAsync (keyboardThread keyQueue) $ \_ -> do
      withAsync (pureClientTuiThread
                   init0
                   handleKey
                   handleToClient
                   keyQueue
                   toClientQueue
                   toServerQueue
                   worldQueue) $ \_ -> do
        roboPlayTUI mkTextPicture worldQueue

-- A pure framework for a program which only acts as a server.
-- Nothing to quit, otherwise update the world state and optionally send some
-- messages to one or more clients.
serverTUI
  :: (Binary toClient, Binary toServer)
  => world
  -> (world -> (Int, Int) -> TextPicture)
  -> ( world
    -> Key
    -> Maybe ([SendToClient toClient], world)
     )  -- handle key presses
  -> ( world
    -> ServerEvent toServer
    -> Maybe ([SendToClient toClient], world)
     )  -- handle messages from the server
  -> IO ()
serverTUI world0 mkTextPicture handleKey handleToServer = withTerminal $ do
  IOFramework.server $ \toServerQueue toClientQueue -> do
    keyQueue <- newTQueueIO
    worldQueue <- newTMQueueIO
    withAsync (keyboardThread keyQueue) $ \_ -> do
      withAsync (pureServerTuiThread
                   world0
                   handleKey
                   handleToServer
                   keyQueue
                   toServerQueue
                   toClientQueue
                   worldQueue) $ \_ -> do
        roboPlayTUI mkTextPicture worldQueue

-- A pure framework for a program which can either act as a client or a server
-- depending on its command-line arguments.
clientServerTUI
  :: (Binary toClient, Binary toServer)
  => ([toServer], clientWorld)
  -> (clientWorld -> (Int, Int) -> TextPicture)
  -> ( clientWorld
    -> Key
    -> Maybe ([toServer], clientWorld)
     )  -- handle key presses
  -> ( clientWorld
    -> Maybe toClient
    -> Maybe ([toServer], clientWorld)
     )  -- handle messages from the server
  -> serverWorld
  -> (serverWorld -> (Int, Int) -> TextPicture)
  -> ( serverWorld
    -> Key
    -> Maybe ([SendToClient toClient], serverWorld)
     )  -- handle key presses
  -> ( serverWorld
    -> ServerEvent toServer
    -> Maybe ([SendToClient toClient], serverWorld)
     )  -- handle messages from the server
  -> IO ()
clientServerTUI clientInit0 clientMkTextPicture
                  clientHandleKey clientHandleToClient
                serverWorld0 serverMkTextPicture
                  serverHandleKey serverHandleToServer
              = withTerminal $ do
  let client toClientQueue toServerQueue = do
        keyQueue <- newTQueueIO
        worldQueue <- newTMQueueIO
        withAsync (keyboardThread keyQueue) $ \_ -> do
          withAsync (keyboardThread keyQueue) $ \_ -> do
            withAsync (pureClientTuiThread
                         clientInit0
                         clientHandleKey
                         clientHandleToClient
                         keyQueue
                         toClientQueue
                         toServerQueue
                         worldQueue) $ \_ -> do
              roboPlayTUI clientMkTextPicture worldQueue
  let server toServerQueue toClientQueue = do
        keyQueue <- newTQueueIO
        worldQueue <- newTMQueueIO
        withAsync (keyboardThread keyQueue) $ \_ -> do
          withAsync (pureServerTuiThread
                       serverWorld0
                       serverHandleKey
                       serverHandleToServer
                       keyQueue
                       toServerQueue
                       toClientQueue
                       worldQueue) $ \_ -> do
            roboPlayTUI serverMkTextPicture worldQueue
  IOFramework.clientServer client server

networkedPlayTUI
  :: (Binary toServer, Binary toClient)
  => server
  -> (server -> (Int, Int) -> TextPicture)
  -> (server -> Key -> Maybe ([(Int, toClient)], server))
  -> (server -> ServerEvent toServer -> Maybe ([(Int, toClient)], server))
  -> client
  -> (client -> (Int, Int) -> TextPicture)
  -> (client -> Key -> Maybe client)
  -> (client -> toClient -> Maybe ([toServer], client))
  -> IO ()
networkedPlayTUI _serverWorld0 _serverMkTextPicture
                   _serverHandleKey _serverHandleNetwork
                 _clientWorld0 _clientMkTextPicture
                   _clientHandleKey _clientHandleNetwork
               = do
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
