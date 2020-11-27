{-# LANGUAGE OverloadedStrings #-}
module Main where

import Graphics.Vty
import Graphics.Vty.Inline

displayTUI
  :: (DisplayRegion -> Image)
  -> IO ()
displayTUI mkPicture = withVty $ \vty -> do
  let out = outputIface vty
  displayRegion <- displayBounds out
  displayCtx <- displayContext out displayRegion
  let picture = picForImage (mkPicture displayRegion)
  outputPicture displayCtx picture
  _ <- getLine
  pure ()

center
  :: DisplayRegion
  -> Image
  -> Image
center (ww, hh) img = translate ((ww - w) `div` 2)
                                ((hh - h) `div` 2)
                                img
  where
    w = imageWidth img
    h = imageHeight img

main
  :: IO ()
main = displayTUI $ \displayRegion
  -> center displayRegion
   $ string defAttr "hello world"
