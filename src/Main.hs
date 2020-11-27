{-# LANGUAGE OverloadedStrings #-}
module Main where

import Graphics.Vty
import Graphics.Vty.Inline

displayTUI
  :: (DisplayRegion -> Image)
  -> IO ()
displayTUI mkPicture = withVty $ \vty -> do
  displayRegion <- displayBounds (outputIface vty)
  let picture = picForImage (mkPicture displayRegion)
  update vty picture
  _ <- nextEvent vty
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
