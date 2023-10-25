{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -Wno-partial-fields #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Game.OrionsOutlaws.UI.Base
  ( UI (..)
  , UIElement (..)
  , Justification (..)
  , ElementKey
  , modifyElement
  , uiToPicture
  , elemToPicture
  , handleClick
  , defaultBackground
  ) where

import Game.OrionsOutlaws.Font (renderString, TextAlignment (LeftToRight, RightToLeft), renderStringCentered, Font (..), stringWidth)

import Graphics.Gloss.Data.Color (black, white, withAlpha)
import Graphics.Gloss.Data.Picture as Pic (Picture, rectangleSolid, color, translate, scale, pictures, blank)
import Control.Monad (when)

data UI = UI
  { elements   :: [UIElement] -- | The elements to render
  , background :: Picture     -- | The UI's background, will get scaled to the window size and is expected to be 1280x720.
  } deriving (Show, Eq)

data UIElement =
  UIText
    { text :: String        -- | The text to render
    , just :: Justification -- | The justification of the text
    , font :: Font          -- | The font to render the text with
    , size :: Float         -- | The size of the text
    , pos  :: Position      -- | The position of the text
    } |
  UIImage
    { image :: Picture  -- | The image to render
    , scale :: Float    -- | The scale of the image
    , pos   :: Position -- | The position of the image
    } |
  -- We cannot make button actions a GameState -> IO GameState function
  -- as that would create a circular dependency between the UI and Model modules.
  -- Hence, you should enqueue tasks in the button action if you wish to use the gamestate.
  UIButton
    { text    :: String         -- | The text on the button
    , font    :: Font           -- | The font to render the text with
    , btnSize :: (Float, Float) -- | The width and height of the button
    , pos     :: Position       -- | The position of the button
    , action  :: IO ()          -- | The action to perform when the button is clicked
    } |
  -- | UI element that can later be modified
  ModifiableUIElement
    { key     :: ElementKey
    , element :: UIElement
    }
    deriving Show

data Justification = JustLeft | JustCentered | JustRight deriving (Show, Eq)

instance Show (IO ()) where show _ = "IO ()"

instance Eq UIElement where
  UIText t1 j1 f1 s1 p1 == UIText t2 j2 f2 s2 p2 = t1 == t2 && j1 == j2 && f1 == f2 && s1 == s2 && p1 == p2
  UIImage i1 s1 p1 == UIImage i2 s2 p2 = i1 == i2 && s1 == s2 && p1 == p2
  UIButton t1 f1 s1 p1 _ == UIButton t2 f2 s2 p2 _ = t1 == t2 && f1 == f2 && s1 == s2 && p1 == p2
  _ == _ = False

-- | Modifies the element with the given key, if one exists.
modifyElement :: UI -> ElementKey -> (UIElement -> UIElement) -> UI
modifyElement ui k m = ui { elements = applyModifier $ elements ui }
  where
    applyModifier :: [UIElement] -> [UIElement]
    applyModifier [] = []
    applyModifier (e@(ModifiableUIElement k' _):es) 
      | k' == k   = m e : es
      | otherwise = e : applyModifier es
    applyModifier (e:es) = e : applyModifier es

-- | Converts a UI to a picture.
uiToPicture :: Maybe MousePosition -> AxialScale -> UI -> Picture
uiToPicture mousePos s@(hs, vs) (UI elems bg) = let s' = min hs vs in pictures 
  [ Pic.scale hs vs bg -- Scaled background
  , Pic.scale s' s' $ pictures $ map (elemToPicture mousePos s) elems -- Elements
  ]

-- | Converts a UI element to a picture.
elemToPicture :: Maybe MousePosition -> AxialScale -> UIElement -> Picture
elemToPicture _ _ (UIText txt jst fnt sze p)
  | jst == JustLeft  = textToPic $ renderString LeftToRight
  | jst == JustRight = textToPic $ renderString RightToLeft
  | otherwise        = textToPic renderStringCentered
  where
    textToPic f = transformSP sze p $ f fnt txt
elemToPicture _ _ (UIImage i s p) = transformSP s p i
elemToPicture mousePos s (UIButton t f (w, h) (x, y) _) =
  let textHeight  = fontLineHeight f -- The height of a line of text in the font
      textWidth   = stringWidth f t  -- The width of the text in the font
      ts          = min ((w * 0.9) / fromIntegral textWidth) (h / fromIntegral textHeight) -- The scale of the text
      borderColor = withAlpha 0.8 white
      borderWidth = 5 in
  translate x y $ pictures
    [ translate 0 ( (h + borderWidth) / 2) $ color borderColor $ rectangleSolid (w + 2 * borderWidth) 5 -- Top border
    , translate 0 (-(h + borderWidth) / 2) $ color borderColor $ rectangleSolid (w + 2 * borderWidth) 5 -- Bottom border
    , translate (-(w + borderWidth) / 2) 0 $ color borderColor $ rectangleSolid 5 h -- Left border
    , translate ( (w + borderWidth) / 2) 0 $ color borderColor $ rectangleSolid 5 h -- Right border
    , color (withAlpha 0.8 black) $ rectangleSolid w h   -- Background
    , Pic.scale ts ts $ renderStringCentered f t         -- Text
    , if isInBounds mousePos (x, y) (w, h) borderWidth s -- Highlight
      then color (withAlpha 0.35 black) $ rectangleSolid (w + 2 * borderWidth) (h + 2 * borderWidth)
      else blank
    ]
elemToPicture mp s (ModifiableUIElement _ e) = elemToPicture mp s e

-- | Checks whether the given mouse position is in bounds
isInBounds :: Maybe MousePosition -> Position -> Size -> BorderWidth -> AxialScale -> Bool
isInBounds Nothing _ _ _ _                            = False
isInBounds (Just (mx, my)) (ex, ey) (w, h) b (hs, vs) = 
  let s = min hs vs -- The smaller of the two scales
      (x, y) = (mx - (ex * s), my - (ey * s))          -- Apply scaling to the position
      (sw, sh) = ((w / 2 + b) * s, (h / 2 + b) * s) in -- Apply scaling to the size
  x >= -sw && x <= sw && y >= -sh && y <= sh -- Check if scaled position is in scaled bounds

-- | Transforms a picture by a scale and a position.
--   Internal use only.
transformSP :: Float -> Position -> Picture -> Picture
transformSP s (x, y) = translate x y . Pic.scale s s

-- | Handles a click on a UI.
--   Checks if the click is on a button and if so, performs the button's action.
--   Does nothing if the click is not on a button.
handleClick :: UI -> MousePosition -> AxialScale -> IO ()
handleClick ui mousePos s = mapM_ handleClick' $ elements ui
  where
    handleClick' (UIButton _ _ (w, h) (x, y) a) = when (isInBounds (Just mousePos) (x, y) (w, h) 5 s) a
    handleClick' (ModifiableUIElement _ e)      = handleClick' e
    handleClick' _                              = return ()

-- | The default background for a UI. Simply transparent black.
defaultBackground :: Picture
defaultBackground = color (withAlpha 0.5 black) $ rectangleSolid 1280 720

type Position      = (Float, Float)
type MousePosition = Position
type BorderWidth   = Float
type Size          = (Float, Float)
type AxialScale    = (Float, Float)
type ElementKey    = String
