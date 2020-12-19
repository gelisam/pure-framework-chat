module PureFramework.TUI.TextPicture where

import PureFramework.TUI.Types


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

-- (x1,y1)-----------+
--    |              |
--    |              |
--    +-----------(x2,y2)
border :: (Int, Int) -> (Int, Int) -> TextPicture
border (x1,y1) (x2,y2)
  = Translated (x1, y1) fullRow
 <> mconcat [ Translated (x1, y) emptyRow
            | y <- [y1+1..y2-1]
            ]
 <> Translated (x1, y2) fullRow
 where
   fullRow = Text ("+" ++ replicate (x2-x1-1) '-' ++ "+")
   emptyRow = Text "|" <> Translated (x2-x1, 0) (Text "|")
