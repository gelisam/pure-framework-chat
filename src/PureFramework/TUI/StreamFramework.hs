{-# LANGUAGE LambdaCase #-}
module PureFramework.TUI.StreamFramework where

import Control.Concurrent.STM
import Control.Concurrent.STM.TMQueue
import Data.Function

import ImperativeVty
import PureFramework.TUI.Types


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
        refreshScreen
        loop
      Nothing -> do
        -- quit
        pure ()
