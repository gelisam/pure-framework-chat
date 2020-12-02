{-# LANGUAGE LambdaCase #-}
module ImperativeVty
  ( withTerminal, initializeTerminal, finalizeTerminal
  , clearScreen
  , getScreenSize
  , putStrAt
  , refreshScreen
  , waitForKey, Key(..)
  ) where

import Control.Concurrent.STM
import Control.Exception
import Data.IORef
import Graphics.Vty
import System.IO.Unsafe (unsafePerformIO)


-- The vty library already provides a nice value-based API, but for pedagogical
-- purposes, I want to start from an imperative API. So I must first create an
-- imperative API out of that nice API.


-- First, a few globals holding the various Vty handles, so that our imperative
-- functions don't have to ask for a Vty handle.

{-# NOINLINE output #-}
output :: Output
output = unsafePerformIO $ outputForConfig =<< standardIOConfig

{-# NOINLINE input #-}
input :: Input
input = unsafePerformIO $ inputForConfig =<< standardIOConfig

getDisplayRegion :: IO DisplayRegion
getDisplayRegion = displayBounds output

getDisplayContext :: IO DisplayContext
getDisplayContext = do
  displayRegion <- getDisplayRegion
  displayContext output displayRegion


-- Then, a global value holding the current contents of the screen.

{-# NOINLINE screen #-}
screen :: IORef Picture
screen = unsafePerformIO $ newIORef emptyPicture

updateScreen :: Image -> IO ()
updateScreen image = do
  picture <- readIORef screen
  let picture' = addToTop picture image
  writeIORef screen picture'


-- Finally, our imperative API.

-- Must be paired with 'finalizeTerminal'
initializeTerminal :: IO ()
initializeTerminal = do
  -- prevent a "nested atomically" error. input's unsafePerformIO block
  -- apparently calls atomically, so we force it now in order to make sure it
  -- isn't evaluated during a future atomically block.
  _ <- evaluate input

  reserveDisplay output

finalizeTerminal :: IO ()
finalizeTerminal = do
  shutdownInput input
  releaseDisplay output
  releaseTerminal output

withTerminal :: IO () -> IO ()
withTerminal = bracket_ initializeTerminal finalizeTerminal

clearScreen :: IO ()
clearScreen = do
  writeIORef screen emptyPicture
  updateScreen emptyImage

getScreenSize :: IO (Int, Int)
getScreenSize = displayBounds output

putStrAt :: (Int, Int) -> String -> IO ()
putStrAt (x, y) = updateScreen
                . translate x y
                . string defAttr

-- to avoid flickering, we only update the screen when this is called, not
-- every time we call putStrAt.
refreshScreen :: IO ()
refreshScreen = do
  displayCtx <- getDisplayContext
  picture <- readIORef screen
  outputPicture displayCtx picture

waitForEvent :: IO Event
waitForEvent = do
  let eventChannel = _eventChannel input
  atomically $ readTChan eventChannel

waitForKey :: IO Key
waitForKey = do
  waitForEvent >>= \case
    EvKey k [] -> do
      pure k
    _ -> do
      waitForKey
