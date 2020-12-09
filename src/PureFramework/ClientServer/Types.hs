{-# LANGUAGE DeriveFunctor #-}
module PureFramework.ClientServer.Types where

import qualified Network.Simple.TCP as TCP


type Host = TCP.HostName
type Port = TCP.ServiceName
type ClientNumber = Int  -- starting at 1

data ServerEvent toServer
  = ClientConnected ClientNumber
  | ClientDisconnected ClientNumber
  | MessageFromClient ClientNumber toServer
  deriving Functor

data SendToClient toClient = SendToClient
  { clientTarget :: ClientNumber
  , clientMessage :: toClient
  }
  deriving Functor
