module Main where

import PureFramework.TUI


textBlock :: [String] -> TextPicture
textBlock ss
  = mconcat [ Translated (0, y) (Text s)
            | (y, s) <- zip [0..] ss
            ]

centeredTextBlock :: [String] -> (Int, Int) -> TextPicture
centeredTextBlock ss (ww, hh)
  = Translated (x, y) (textBlock ss)
  where
    w = maximum (0 : fmap length ss)
    h = length ss
    x = (ww - w) `div` 2
    y = (hh - h) `div` 2

main :: IO ()
main = displayTUI (centeredTextBlock ["hello world"])
