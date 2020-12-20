{-# LANGUAGE DeriveAnyClass, DeriveGeneric, LambdaCase, ScopedTypeVariables #-}
module PureFramework.TUI
  ( module PureFramework.ClientServer.Types
  , module PureFramework.TUI
  , module PureFramework.TUI.Types
  ) where

import Control.Concurrent.Async
import Control.Concurrent.STM
import Control.Concurrent.STM.TMQueue
import Data.Binary (Binary)
import Data.List (foldl')
import Data.Maybe (fromMaybe)
import GHC.Generics (Generic)

import ImperativeVty
import PureFramework.ClientServer.Types
import PureFramework.Threads
import PureFramework.TUI.TextPicture
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


data ClientStatus world
  = ClientWaiting
      Int  -- number of clients who have connected so far
  | ClientRejectedClientLimitReached
  | ClientActive
      ClientNumber
      world
      [Key]  -- ^ sent to the server but not yet confirmed
  deriving (Binary, Generic)

data ServerStatus world
  = ServerWaiting
      [ClientNumber]  -- clients which have connected so far
  | ServerActive
      world
  deriving (Binary, Generic)

data ServerToClient
  = ClientsConnected
      Int  -- number of clients who have connected so far
  | ClientLimitReached  -- so you cannot join
  | Begin
      ClientNumber  -- here's your client number
  | Continue
      ClientNumber  -- the client who typed this Key
      Key
  deriving (Binary, Generic)

multiplayTUI
  :: forall world
   . world
  -> Int  -- number of clients (+ 1 for the server)
  -> (world -> Int -> (Int, Int) -> TextPicture)
  -> (world -> Int -> Key -> Maybe world)
  -> IO ()
multiplayTUI world0 clientCount mkTextPicture handleKey = do
  -- optimistically apply the keys which have been sent but not yet confirmed,
  -- hoping that they'll be confirmed with no interference from other players
  -- in-between.
  let optimistic
        :: Int
        -> [Key]
        -> world
        -> Maybe world
      optimistic clientNumber keys w0
        = foldl' (\maybeWorld key -> do
                  world <- maybeWorld
                  handleKey world clientNumber key)
                 (Just w0)
                 keys

  let clientInit0
        :: ([Key], ClientStatus world)
      clientInit0 = ([], ClientWaiting 0)

  let clientMkTextPicture
        :: ClientStatus world
        -> (Int, Int)
        -> TextPicture
      clientMkTextPicture = \case
        ClientWaiting 0
          -> centeredTextBlock ["connecting..."]
        ClientWaiting n
          -> centeredTextBlock [ "wait for "
                              ++ show (clientCount - n)
                              ++ " more"
                               , "clients to connect"
                               ]
        ClientRejectedClientLimitReached
          -> centeredTextBlock [ "client limit reached,"
                               , "sorry"
                               ]
        ClientActive clientNumber world sentKeys
          -> mkTextPicture
               (fromMaybe world $ optimistic clientNumber sentKeys world)
               clientNumber

  let clientHandleKey
        :: ClientStatus world
        -> Key
        -> Maybe ([Key], ClientStatus world)
      clientHandleKey status key = do
        case status of
          ClientActive clientNumber world sentKeys -> do
            let sentKeys' = sentKeys ++ [key]

            -- don't wait for the server to confirm if the user wants to quit
            _ <- optimistic clientNumber sentKeys' world

            pure ([key], ClientActive clientNumber world sentKeys')
          _ -> do
            case key of
              KEsc -> do
                Nothing
              _ -> do
                pure ([], status)

  let clientHandleToClient
        :: ClientStatus world
        -> Maybe (ServerToClient)
        -> Maybe ([Key], ClientStatus world)
      clientHandleToClient status maybeEvent = do
        event <- maybeEvent
        case (status, event) of
          (ClientWaiting _, ClientsConnected n') -> do
            pure ([], ClientWaiting n')
          (_, ClientLimitReached) -> do
            pure ([], ClientRejectedClientLimitReached)
          (_, Begin clientNumber) -> do
            pure ([], ClientActive clientNumber world0 [])
          (ClientActive clientNumber world sentKeys, Continue i key) -> do
            let sentKeys' = if i == clientNumber
                            then -- assert (key == head sentKeys)
                                 drop 1 sentKeys
                            else sentKeys
            world' <- handleKey world i key
            pure ([], ClientActive clientNumber world' sentKeys')
          _ -> do
            pure ([], status)

  let serverWorld0
        :: ServerStatus world
      serverWorld0 = ServerWaiting []

  let serverMkTextPicture
        :: ServerStatus world
        -> (Int, Int)
        -> TextPicture
      serverMkTextPicture = \case
        ServerWaiting clients
          -> centeredTextBlock [ "wait for "
                              ++ show (clientCount - length clients)
                              ++ " more"
                               , "clients to connect"
                               ]
        ServerActive world
          -> mkTextPicture world 0

  let serverHandleKey
        :: ServerStatus world
        -> Key
        -> Maybe ([SendToClient ServerToClient], ServerStatus world)
      serverHandleKey status key = do
        case status of
          ServerActive world -> do
            let msgs = [ SendToClient i (Continue 0 key)
                       | i <- [1..clientCount]
                       ]
            world' <- handleKey world 0 key
            pure (msgs, ServerActive world')
          _ -> do
            case key of
              KEsc -> do
                Nothing
              _ -> do
                pure ([], status)

  let serverHandleToServer
        :: ServerStatus world
        -> ServerEvent Key
        -> Maybe ([SendToClient ServerToClient], ServerStatus world)
      serverHandleToServer status event = do
        case (status, event) of
          (ServerWaiting clients, ClientConnected i) -> do
            let clients' = i:clients
            let n = length clients'
            if n == clientCount
              then do
                let msgs = [ SendToClient j (Begin j)
                           | j <- [1..clientCount]
                           ]
                pure (msgs, ServerActive world0)
              else do
                let msgs = [ SendToClient j (ClientsConnected n)
                           | j <- clients'
                           ]
                pure (msgs, ServerWaiting clients')
          (ServerActive world, MessageFromClient i key) -> do
            let msgs = [ SendToClient j (Continue i key)
                       | j <- [1..clientCount]
                       ]
            world' <- handleKey world i key
            pure (msgs, ServerActive world')
          _ -> do
            pure ([], status)

  clientServerTUI clientInit0 clientMkTextPicture
                    clientHandleKey clientHandleToClient
                  serverWorld0 serverMkTextPicture
                    serverHandleKey serverHandleToServer
