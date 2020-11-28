module Main where

import Control.Monad
import Data.Foldable

import ImperativeVty


drawCenteredTextBlock :: [String] -> IO ()
drawCenteredTextBlock ss = do
  (ww, hh) <- getScreenSize
  let w = maximum (0 : fmap length ss)
  let h = length ss
  let x = (ww - w) `div` 2
  let y = (hh - h) `div` 2
  for_ (zip [0..] ss) $ \(i, s) -> do
    putStrAt (x, y + i) s

main :: IO ()
main = withTerminal $ do
  clearScreen
  drawCenteredTextBlock ["hello world"]
  void waitForKey
