{-# OPTIONS_GHC -Wno-partial-fields #-}
module Game.OrionsOutlaws.UI.Base
  ( UI (..)
  , UIElement (..)
  , uiToPicture
  , elemToPicture
  , defaultBackground
  ) where

import Game.OrionsOutlaws.Model (Position)
import Game.OrionsOutlaws.Assets (pixeboyFont)
import Game.OrionsOutlaws.Font (renderString, TextAlignment (LeftToRight), renderStringCentered)

import Graphics.Gloss (Picture, rectangleSolid, color, withAlpha)
import Graphics.Gloss.Data.Color (black)
import Graphics.Gloss.Data.Picture (translate, scale, pictures)

data UI = UI
  { elements   :: [UIElement] -- | The elements to render
  , background :: Picture     -- | The UI's background, will get scaled to the window size and is expected to be 1280x720.
  }

data UIElement =
  UIText
    { text :: String    -- | The text to render
    , size :: Float     -- | The size of the text
    , pos  :: Position  -- | The position of the text
    } |
  UIImage
    { image :: Picture  -- | The image to render
    , scale :: Float    -- | The scale of the image
    , pos   :: Position -- | The position of the image
    } |
  UIButton
    { text    :: String         -- | The text on the button
    , btnSize :: (Float, Float) -- | The width and height of the button
    , pos     :: Position       -- | The position of the button
    , action  :: IO ()          -- | The action to perform when the button is clicked
    }

-- | Converts a UI to a picture.
uiToPicture :: UI -> Picture
uiToPicture (UI elems bg) = pictures $ bg : map elemToPicture elems

-- | Converts a UI element to a picture.
elemToPicture :: UIElement -> Picture
elemToPicture (UIText t s p) = transformSP s p $ renderString LeftToRight pixeboyFont t
elemToPicture (UIImage i s p) = transformSP s p i
elemToPicture (UIButton t (w, h) (x, y) _) = translate x y $ pictures 
  [ color (withAlpha 0.8 black) $ rectangleSolid w h -- Background
  , renderStringCentered pixeboyFont t               -- Text
  ]

transformSP :: Float -> Position -> Picture -> Picture
transformSP s (x, y) = translate x y . Graphics.Gloss.Data.Picture.scale s s

-- | The default background for a UI. Simply transparent black.
defaultBackground :: Picture
defaultBackground = color (withAlpha 0.5 black) $ rectangleSolid 1280 720
