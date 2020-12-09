{-# LANGUAGE LambdaCase #-}
module PureFramework.TUI.Types
  ( module PureFramework.TUI.Types
  , Key(..)
  ) where

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
