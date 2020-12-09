{-# LANGUAGE LambdaCase #-}
module Network.Simple.TCP.Extra where

import Data.Word
import Network.Simple.TCP
import qualified Data.Binary as Binary
import qualified Data.ByteString as Strict
import qualified Data.ByteString.Lazy as Lazy


-- a variant of recv which receives exactly N bytes instead of up to N
recvN :: Socket -> Int -> IO (Maybe Strict.ByteString)
recvN socket n = do
  recv socket n >>= \case
    Just bytestring -> do
      let len = Strict.length bytestring
      if len == n
        then do
          pure (Just bytestring)
        else do
          (fmap . fmap) (bytestring <>) $ recvN socket (n - len)
    Nothing -> do
      pure Nothing

-- send both N and N bytes, so that the next recvPacket receives exactly N bytes.
sendPacket :: Socket -> Lazy.ByteString -> IO ()
sendPacket socket bytestring = do
  let len32 :: Word32
      len32 = fromIntegral . Lazy.length $ bytestring
  sendLazy socket (Binary.encode len32 <> bytestring)

recvPacket :: Socket -> IO (Maybe Strict.ByteString)
recvPacket socket = do
  recvN socket 4 >>= \case
    Just encodedLen32 -> do
      let len32 :: Word32
          len32 = Binary.decode . Lazy.fromStrict $ encodedLen32
      recvN socket (fromIntegral len32)
    Nothing -> do
      pure Nothing
