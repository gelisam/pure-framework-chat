{-# LANGUAGE LambdaCase #-}
module PureFramework.ClientServer.Threads where

import Control.Concurrent.Async
import Control.Concurrent.STM
import Control.Concurrent.STM.TMQueue
import Control.Monad
import Data.Binary (Binary)
import Data.Function
import Data.Map ((!))
import Network.Simple.TCP (Socket)
import qualified Data.Binary as Binary
import qualified Data.ByteString.Lazy as ByteString
import qualified Data.Map as Map
import qualified Network.Simple.TCP as TCP

import PureFramework.ClientServer.Types
import qualified Network.Simple.TCP.Extra as TCP


-- Intended to be spawned by withAsync.


-- sendingThread and receivingThread convert between queues and sockets. We
-- first send the number of bytes and then the bytes, otherwise the receiving
-- thread would have no idea when each message stops.

sendingThread
  :: Binary a
  => TQueue a  -- input channel
  -> Socket  -- output socket
  -> IO ()
sendingThread inQueue outSocket = do
  forever $ do
    a <- atomically $ readTQueue inQueue
    let encoded = Binary.encode a
    TCP.sendPacket outSocket encoded

receivingThread
  :: Binary a
  => Socket  -- input socket
  -> TMQueue a  -- output channel, closed when the socket is
  -> IO ()
receivingThread inSocket outQueue = do
  TCP.recvPacket inSocket >>= \case
    Just encoded -> do
      let a = Binary.decode . ByteString.fromStrict $ encoded
      atomically $ writeTMQueue outQueue a
      receivingThread inSocket outQueue
    Nothing -> do
      atomically $ closeTMQueue outQueue

socketThread
  :: (Binary fromSocket, Binary toSocket)
  => Socket
  -> TQueue toSocket  -- input channel
  -> TMQueue fromSocket  -- output channel, closed when the socket closes
  -> IO ()
socketThread socket inQueue outQueue = do
  withAsync (sendingThread inQueue socket) $ \_ -> do
    receivingThread socket outQueue

-- Spawns more threads as clients connect.
-- Intended to be spawned by withAsync. All spawned threads are killed when
-- this one is.
clientThreads
  :: Socket  -- TCP.listen's socket
  -> (ClientNumber -> Socket -> IO ())  -- spawned for each client
  -> IO ()
clientThreads metaSocket clientThread = do
  flip fix 1 $ \loop clientNumber -> do
    TCP.accept metaSocket $ \(socket, _) -> do
      withAsync (clientThread clientNumber socket) $ \_ -> do
        -- consumes stack space, could be problematic with a large number of
        -- clients
        loop (clientNumber + 1)

serverThread
  :: (Binary toServer, Binary toClient)
  => Socket  -- TCP.listen's socket
  -> TQueue (SendToClient toClient)  -- input channel
  -> TQueue (ServerEvent toServer)  -- output channel
  -> IO ()
serverThread metaSocket inQueue outQueue = do
  toClientQueuesVar <- newTVarIO Map.empty
  let sendToClientThread = do
        forever $ do
          SendToClient clientNumber toClient <- atomically $ readTQueue inQueue
          toClientQueues <- atomically $ readTVar toClientQueuesVar
          let toClientQueue = toClientQueues ! clientNumber
          atomically $ writeTQueue toClientQueue toClient
  withAsync sendToClientThread $ \_ -> do
    clientThreads metaSocket $ \clientNumber socket -> do
      fromClientQueue <- newTMQueueIO
      toClientQueue <- newTQueueIO
      atomically $ modifyTVar toClientQueuesVar
                 $ Map.insert clientNumber toClientQueue
      atomically $ writeTQueue outQueue
                 $ ClientConnected clientNumber 
      withAsync (socketThread socket toClientQueue fromClientQueue) $ \_ -> do
        fix $ \loop -> do
          (atomically $ readTMQueue fromClientQueue) >>= \case
            Just toServer -> do
              atomically $ writeTQueue outQueue
                         $ MessageFromClient clientNumber toServer
              loop
            Nothing -> do
              atomically $ writeTQueue outQueue
                         $ ClientDisconnected clientNumber
