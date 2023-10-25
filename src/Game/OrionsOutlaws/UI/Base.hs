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
  , handleMouse
  , handleMotion
  , defaultBackground
  ) where

import Game.OrionsOutlaws.Font (renderString, TextAlignment (LeftToRight, RightToLeft), renderStringCentered, Font (..), stringWidth)

import Graphics.Gloss.Data.Color (black, white, withAlpha)
import Graphics.Gloss.Data.Picture as Pic (Picture, rectangleSolid, color, translate, scale, pictures, blank)
import Control.Monad (when)
import Graphics.Gloss.Interface.IO.Game (KeyState (..))
import Data.Ord (clamp)

data UI = UI
  { elements   :: [UIElement] -- | The elements to render
  , background :: Picture     -- | The UI's background, will get scaled to the window size and is expected to be 1280x720.
  } deriving (Show, Eq)

data UIElement =
  UIText
    { text     :: String        -- | The text to render
    , just     :: Justification -- | The justification of the text
    , font     :: Font          -- | The font to render the text with
    , textSize :: Float         -- | The size of the text
    , pos      :: Position      -- | The position of the text
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
    , pos     :: Position       -- | The position of the button
    , btnSize :: (Float, Float) -- | The width and height of the button
    , action  :: IO ()          -- | The action to perform when the button is clicked
    } |
  UISlider
    { pos          :: Position       -- | The position of the slider
    , size         :: (Float, Float) -- | The width and height of the slider
    , value        :: Float          -- | The current value of the slider
    , sliderAction :: Float -> IO () -- | The action to perform when the slider is moved
    , active       :: Bool           -- | Whether the slider is currently being moved. Must always be False when creating a new slider.
    } |
  -- | UI element that can later be modified
  ModifiableUIElement
    { key     :: ElementKey
    , element :: UIElement
    }
    deriving Show

data Justification = JustLeft | JustCentered | JustRight deriving (Show, Eq)

instance Show (IO ()) where show _ = "IO ()"
instance Show (Float -> IO ()) where show _ = "Float -> IO ()"

instance Eq UIElement where
  UIText t1 j1 f1 s1 p1 == UIText t2 j2 f2 s2 p2 = t1 == t2 && j1 == j2 && f1 == f2 && s1 == s2 && p1 == p2
  UIImage i1 s1 p1 == UIImage i2 s2 p2 = i1 == i2 && s1 == s2 && p1 == p2
  UIButton t1 f1 p1 s1 _ == UIButton t2 f2 p2 s2 _ = t1 == t2 && f1 == f2 && p1 == p2 && s1 == s2
  UISlider p1 s1 v1 _ a1 == UISlider p2 s2 v2 _ a2 = p1 == p2 && s1 == s2 && v1 == v2 && a1 == a2
  ModifiableUIElement k1 e1 == ModifiableUIElement k2 e2 = k1 == k2 && e1 == e2
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
-- Render UI Text
elemToPicture _ _ (UIText txt jst fnt sze p)
  | jst == JustLeft  = textToPic $ renderString LeftToRight
  | jst == JustRight = textToPic $ renderString RightToLeft
  | otherwise        = textToPic renderStringCentered
  where
    textToPic f = transformSP sze p $ f fnt txt

-- Render UI Image
elemToPicture _ _ (UIImage i s p) = transformSP s p i

-- Render UI Button
elemToPicture mousePos s (UIButton t f p (w, h) _) =
  let textHeight  = fontLineHeight f -- The height of a line of text in the font
      textWidth   = stringWidth f t  -- The width of the text in the font
      ts          = if null t then 0 else min ((w * 0.9) / fromIntegral textWidth) (h / fromIntegral textHeight) -- The scale of the text
  in renderButton mousePos s 5 (w, h) p p False $ Pic.scale ts ts $ renderStringCentered f t

-- Render UI Slider
elemToPicture mousePos s (UISlider (x, y) (w, h) v _ a) = translate x y $ pictures
  [ color (withAlpha 0.8 white) $ rectangleSolid w 10   -- Line to slide over
  , renderButton mousePos s 3 (0.67 * h, h) ((v - 0.5) * w, 0) (x + ((v - 0.5) * w), y) a blank -- Slider
  ]

-- Render Modifiable UI Element
elemToPicture mp s (ModifiableUIElement _ e) = elemToPicture mp s e

-- | Renders a button
--              mousePos               scale         border         size              position    button position  hover overwrite  content
renderButton :: Maybe MousePosition -> AxialScale -> BorderWidth -> (Float, Float) -> Position -> Position ->      Bool ->          Picture -> Picture
renderButton mousePos s borderWidth (w, h) (x, y) (bx, by) ho p =
  let borderColor = withAlpha 0.8 white
  in translate x y $ pictures
    [ translate 0 ( (h + borderWidth) / 2) $ color borderColor $ rectangleSolid (w + 2 * borderWidth) borderWidth -- Top border
    , translate 0 (-(h + borderWidth) / 2) $ color borderColor $ rectangleSolid (w + 2 * borderWidth) borderWidth -- Bottom border
    , translate (-(w + borderWidth) / 2) 0 $ color borderColor $ rectangleSolid borderWidth h -- Left border
    , translate ( (w + borderWidth) / 2) 0 $ color borderColor $ rectangleSolid borderWidth h -- Right border
    , color (withAlpha 0.8 black) $ rectangleSolid w h   -- Background
    , p                                                  -- Content (either blank or some text)
    , if ho || isInBounds mousePos (bx, by) (w, h) borderWidth s -- Highlight
      then color (withAlpha 0.35 black) $ rectangleSolid (w + 2 * borderWidth) (h + 2 * borderWidth)
      else blank
    ]

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
handleMouse :: UI -> KeyState -> MousePosition -> AxialScale -> IO UI
handleMouse ui state mousePos s = do
  es <- mapM handleMouse' $ elements ui
  return $ ui { elements = es }
  where
    handleMouse' :: UIElement -> IO UIElement
    -- For buttons, click them if KeyState is Down.
    handleMouse' b@(UIButton _ _ (x, y) (w, h) a) | state == Down = do
      when (isInBounds (Just mousePos) (x, y) (w, h) 5 s) a
      return b

    -- For sliders, toggle the active state.
    handleMouse' sl@(UISlider (x, y) (w, h) v _ _)
      -- Release slider if mouse is released
      | state == Up = do
          sliderAction sl v
          return sl { active = False }
      -- If mouse is pressed and mouse is in bounds, activate slider
      | isInBounds (Just mousePos) (x + ((v - 0.5) * w), y) (w, h) 3 s = return sl { active = True }
      -- Do nothing if mouse is not in bounds
      | otherwise                                                      = return sl
    -- For modifiable elements, propagate the event.
    handleMouse' (ModifiableUIElement _ e)                      = handleMouse' e

    -- For anything else, do nothing.
    handleMouse' e                                              = return e

handleMotion :: UI -> MousePosition -> AxialScale -> IO UI
handleMotion ui mousePos (hs, vs) = do
  es <- mapM handleMotion' $ elements ui
  return $ ui { elements = es }
  where
    s = min hs vs -- Scale to use
    handleMotion' :: UIElement -> IO UIElement
    -- For sliders, update the value if the slider is active.
    handleMotion' sl@(UISlider (x, _) (w, _) _ _ True) = do
      let v' = (fst mousePos - (x * s)) / w / s + 0.5 -- Calculate new value
      let v'' = clamp (0, 1) v'                       -- Clamp value to [0, 1]
      return sl { value = v'' }                       -- Update slider value
    -- For modifiable elements, propagate the event.
    handleMotion' (ModifiableUIElement _ e) = handleMotion' e
    -- For anything else, do nothing.
    handleMotion' e                         = return e

-- | The default background for a UI. Simply transparent black.
defaultBackground :: Picture
defaultBackground = color (withAlpha 0.5 black) $ rectangleSolid 1280 720

type Position      = (Float, Float)
type MousePosition = Position
type BorderWidth   = Float
type Size          = (Float, Float)
type AxialScale    = (Float, Float)
type ElementKey    = String
