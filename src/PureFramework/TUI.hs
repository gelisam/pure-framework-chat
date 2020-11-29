{-# LANGUAGE LambdaCase #-}
module PureFramework.TUI
  ( module PureFramework.TUI
  , Key(..)
  ) where

import Control.Monad
import Data.Function

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

playTUI
  :: world
  -> (world -> (Int, Int) -> TextPicture)
  -> (world -> Key -> Maybe world)
  -> IO ()
playTUI world0 mkTextPicture handleKey = withTerminal $ do
  screenSize <- getScreenSize
  flip fix world0 $ \loop world -> do
    clearScreen
    drawTextPicture (mkTextPicture world screenSize)
    key <- waitForKey
    case handleKey world key of
      Nothing -> do
        -- quit
        pure ()
      Just world' -> do
        loop world'
