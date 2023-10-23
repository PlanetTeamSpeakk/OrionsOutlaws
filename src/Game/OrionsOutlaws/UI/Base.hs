{-# OPTIONS_GHC -Wno-partial-fields #-}
module Game.OrionsOutlaws.UI.Base where

import Game.OrionsOutlaws.Model (Position)
import Graphics.Gloss (Picture, rectangleSolid, color, withAlpha)
import Graphics.Gloss.Data.Color (black)

data UI = UI 
  { elements   :: [UIElement] -- | The elements to render
  , background :: Picture     -- | The UI's background, will get scaled to the window size and is expected to be 1280x720.
  }

data UIElement =
  UIText {
    text :: String,
    size :: Float,
    pos  :: Position
  } |
  UIImage {
    image :: String,
    scale :: Float,
    pos   :: Position
  }

toPicture :: UIElement -> Picture
toPicture (UIText t s p) = undefined

defaultBackground :: Picture
defaultBackground = color (withAlpha 0.5 black) $ rectangleSolid 1280 720
