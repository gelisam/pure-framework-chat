{-# LANGUAGE LambdaCase #-}
module PureFramework.ClientServer.IOFramework where

import Control.Concurrent.Async
import Control.Concurrent.STM
import Control.Concurrent.STM.TMQueue
import Data.Binary (Binary)
import System.Environment (getArgs, getProgName)
import System.Exit (exitFailure)
import qualified Network.Simple.TCP as TCP

import PureFramework.ClientServer.Threads
import PureFramework.ClientServer.Types


-- An IO framework for a program which only acts as a server.
server
  :: (Binary toClient, Binary toServer)
  => ( TQueue (ServerEvent toServer)  -- input channel
    -> TQueue (SendToClient toClient)  -- output channel
    -> IO ()
     )  -- continuation
  -> IO ()
server cc = do
  getArgs >>= \case
    [host, port] -> do
      TCP.listen (TCP.Host host) port $ \(metaSocket, _) -> do
        toClientQueue <- newTQueueIO
        toServerQueue <- newTQueueIO
        withAsync (serverThread metaSocket toClientQueue toServerQueue) $ \_ -> do
          cc toServerQueue toClientQueue
    _ -> do
      progName <- getProgName
      putStrLn "usage:"
      putStrLn $ "  " ++ progName ++ " 0.0.0.0 PORT"
      exitFailure

-- An IO framework for a program which only acts as a client.
client
  :: (Binary toClient, Binary toServer)
  => ( TMQueue toClient  -- input channel
    -> TQueue toServer  -- output channel
    -> IO ()
     )  -- continuation
  -> IO ()
client cc = do
  getArgs >>= \case
    [host, port] -> do
      TCP.connect host port $ \(socket, _) -> do
        toClientQueue <- newTMQueueIO
        toServerQueue <- newTQueueIO
        withAsync (socketThread socket toServerQueue toClientQueue) $ \_ -> do
          cc toClientQueue toServerQueue
    _ -> do
      progName <- getProgName
      putStrLn "usage:"
      putStrLn $ "  " ++ progName ++ " SERVER_IP PORT"
      exitFailure

-- An IO framework for a program which can act either as a client or a server,
-- depending on its arguments.
clientServer
  :: (Binary toClient, Binary toServer)
  => ( TMQueue toClient  -- input channel
    -> TQueue toServer  -- output channel
    -> IO ()
     )  -- client continuation
  -> ( TQueue (ServerEvent toServer)  -- input channel
    -> TQueue (SendToClient toClient)  -- output channel
    -> IO ()
     )  -- server continuation
  -> IO ()
clientServer clientCC serverCC = do
  getArgs >>= \case
    ["client", host, port] -> do
      TCP.connect host port $ \(socket, _) -> do
        toClientQueue <- newTMQueueIO
        toServerQueue <- newTQueueIO
        withAsync (socketThread socket toServerQueue toClientQueue) $ \_ -> do
          clientCC toClientQueue toServerQueue
    ["server", host, port] -> do
      TCP.listen (TCP.Host host) port $ \(metaSocket, _) -> do
        toClientQueue <- newTQueueIO
        toServerQueue <- newTQueueIO
        withAsync (serverThread metaSocket toClientQueue toServerQueue) $ \_ -> do
          serverCC toServerQueue toClientQueue
    _ -> do
      progName <- getProgName
      putStrLn "usage:"
      putStrLn $ "  " ++ progName ++ " server 0.0.0.0 PORT"
      putStrLn $ "  " ++ progName ++ " client SERVER_IP PORT"
      exitFailure
