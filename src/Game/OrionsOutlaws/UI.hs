{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -Wno-partial-fields #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Game.OrionsOutlaws.UI
  ( UI (..)
  , UIElement (..)
  , Justification (..)
  , ElementKey

  -- * Utility constructor functions
  , ui
  , uiWithBg
  , text
  , image
  , button
  , slider
  , modifiable

  -- * Utility functions
  , modifyElement

  -- * Rendering functions
  , uiToPicture
  , elemToPicture

  -- * Event handling functions
  , handleMouse
  , handleMotion

  -- * Constants
  , defaultBackground
  ) where

import Game.OrionsOutlaws.Font (renderString, TextAlignment (LeftToRight, RightToLeft), renderStringCentered, Font (..), stringWidth)

import Graphics.Gloss.Data.Color (black, white, withAlpha)
import Graphics.Gloss.Data.Picture as Pic (Picture, rectangleSolid, color, translate, scale, pictures, blank)
import Control.Monad (when)
import Graphics.Gloss.Interface.IO.Game (KeyState (..))
import Data.Ord (clamp)
import qualified Data.Map as Map

data UI = UI
  { elements   :: Map.Map (Maybe ElementKey) [UIElement] -- | The elements to render
  , background :: Picture                            -- | The UI's background, will get scaled to the window size and is expected to be 1280x720.
  } deriving (Show, Eq)

data UIElement =
  UIText
    { textText   :: String         -- | The text to render
    , textJust   :: Justification  -- | The justification of the text
    , textFont   :: Font           -- | The font to render the text with
    , textSize   :: TextSize       -- | The size of the text
    , elemPos    :: Position       -- | The position of the text
    } |
  UIImage
    { imageImg   :: Picture        -- | The image to render
    , imageScale :: Float          -- | The scale of the image
    , elemPos    :: Position       -- | The position of the image
    } |
  -- We cannot make button actions a GameState -> IO GameState function
  -- as that would create a circular dependency between the UI and Model modules.
  -- Hence, you should enqueue tasks in the button action if you wish to use the gamestate.
  UIButton
    { btnText    :: String         -- | The text on the button
    , btnFont    :: Font           -- | The font to render the text with
    , elemPos    :: Position       -- | The position of the button
    , btnSize    :: Size           -- | The width and height of the button
    , btnAction  :: IO ()          -- | The action to perform when the button is clicked
    } |
  UISlider
    { elemPos    :: Position             -- | The position of the slider
    , sldrSize   :: Size                 -- | The width and height of the slider
    , sldrValue  :: SliderValue          -- | The current value of the slider
    , sldrAction :: SliderValue -> IO () -- | The action to perform when the slider is moved
    , sldrActive :: Bool                 -- | Whether the slider is currently being moved. Must always be False when creating a new slider.
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
modifyElement ui' k m = case Map.lookup (Just k) $ elements ui' of
  Just e  -> ui' { elements = Map.insert (Just k) (applyModifier e) $ elements ui' }
  Nothing -> ui'
  where
    -- The list should not be empty nor should it contain more than one element
    -- nor should that one element be anything other than a ModifiableUIElement.
    applyModifier :: [UIElement] -> [UIElement]
    applyModifier (e@(ModifiableUIElement {}):es) = e { element = m e } : applyModifier es
    applyModifier e                               = error $ "modifyElement: Unexpected UI element: " ++ show e

-- | Converts a UI to a picture.
uiToPicture :: Maybe MousePosition -> AxialScale -> UI -> Picture
uiToPicture mousePos s@(hs, vs) (UI es bg) = let s' = min hs vs in pictures
  [ Pic.scale hs vs bg -- Scaled background
  , Pic.scale s' s' $ pictures $ map (elemToPicture mousePos s) $ concat $ Map.elems es -- Elements
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
  [ color (withAlpha 0.8 white) $ rectangleSolid w 10                                           -- Line to slide over
  , renderButton mousePos s 3 (0.67 * h, h) ((v - 0.5) * w, 0) (x + ((v - 0.5) * w), y) a blank -- Slider
  ]

-- Render Modifiable UI Element
elemToPicture mp s (ModifiableUIElement _ e) = elemToPicture mp s e

-- | Renders a button
--              mousePos               scale         border         size              position    abs position  hover overwrite  content
renderButton :: Maybe MousePosition -> AxialScale -> BorderWidth -> (Float, Float) -> Position -> Position ->   Bool ->          Picture -> Picture
renderButton mousePos s borderWidth (w, h) (x, y) (bx, by) ho p =
  let borderColor = withAlpha 0.8 white
  in translate x y $ pictures
    [ translate 0 ( (h + borderWidth) / 2) $ color borderColor $ rectangleSolid (w + 2 * borderWidth) borderWidth -- Top border
    , translate 0 (-(h + borderWidth) / 2) $ color borderColor $ rectangleSolid (w + 2 * borderWidth) borderWidth -- Bottom border
    , translate (-(w + borderWidth) / 2) 0 $ color borderColor $ rectangleSolid borderWidth h -- Left border
    , translate ( (w + borderWidth) / 2) 0 $ color borderColor $ rectangleSolid borderWidth h -- Right border
    , color (withAlpha 0.8 black) $ rectangleSolid w h           -- Background
    , p                                                          -- Content (either blank or some text)
    , if ho || isInBounds mousePos (bx, by) (w, h) borderWidth s -- Hover highlight
      then color (withAlpha 0.35 black) $ rectangleSolid (w + 2 * borderWidth) (h + 2 * borderWidth)
      else blank
    ]

-- | Checks whether the given mouse position is in bounds
isInBounds :: Maybe MousePosition -> Position -> Size -> BorderWidth -> AxialScale -> Bool
isInBounds Nothing _ _ _ _                            = False
isInBounds (Just (mx, my)) (ex, ey) (w, h) b (hs, vs) =
  let s = min hs vs -- The smaller of the two scales
      (x, y) = (mx - (ex * s), my - (ey * s))       -- Apply scaling to the position
      (sw, sh) = ((w / 2 + b) * s, (h / 2 + b) * s) -- Apply scaling to the size
  in x >= -sw && x <= sw && y >= -sh && y <= sh     -- Check if scaled position is in scaled bounds

-- | Transforms a picture by a scale and a position.
--   Internal use only.
transformSP :: Float -> Position -> Picture -> Picture
transformSP s (x, y) = translate x y . Pic.scale s s

-- | Handles a click on a UI.
--   Checks if the click is on a button or slider and if so, performs the button's action/activates the slider.
--   Does nothing if the click is not on a button or a slider.
handleMouse :: UI -> KeyState -> MousePosition -> AxialScale -> IO UI
handleMouse ui' state mousePos s = do
  es <- mapM handleMouse' $ elements ui'
  return $ ui' { elements = es }
  where
    handleMouse' :: [UIElement] -> IO [UIElement]
    handleMouse' = mapM handleMouse''

    handleMouse'' :: UIElement -> IO UIElement
    -- For buttons, click them if KeyState is Down.
    handleMouse'' b@(UIButton _ _ (x, y) (w, h) a) | state == Down = do
      when (isInBounds (Just mousePos) (x, y) (w, h) 5 s) a
      return b

    -- For sliders, toggle the active state.
    handleMouse'' sl@(UISlider (x, y) (w, h) v _ _)
      -- Release slider if mouse is released
      | state == Up = do
          sldrAction sl v
          return sl { sldrActive = False }
      -- If mouse is pressed and mouse is in bounds, activate slider
      | isInBounds (Just mousePos) (x + ((v - 0.5) * w), y) (0.67 * h, h) 3 s = return sl { sldrActive = True }
      -- Do nothing if mouse is not in bounds
      | otherwise  = return sl
    -- For modifiable elements, propagate the event.
    handleMouse'' (ModifiableUIElement _ e) = handleMouse'' e

    -- For anything else, do nothing.
    handleMouse'' e                         = return e

-- | Handles a mouse motion event on a UI.
handleMotion :: UI -> MousePosition -> AxialScale -> IO UI
handleMotion ui' mousePos (hs, vs) = do
  es <- mapM handleMotion' $ elements ui'
  return $ ui' { elements = es }
  where
    s = min hs vs -- Scale to use
    handleMotion' :: [UIElement] -> IO [UIElement]
    handleMotion' = mapM handleMotion''

    handleMotion'' :: UIElement -> IO UIElement
    -- For sliders, update the value if the slider is active.
    handleMotion'' sl@(UISlider (x, _) (w, _) _ _ True) = do
      let v' = (fst mousePos - (x * s)) / w / s + 0.5 -- Calculate new value
      let v'' = clamp (0, 1) v'                       -- Clamp value to [0, 1]
      return sl { sldrValue = v'' }                   -- Update slider value
    -- For modifiable elements, propagate the event.
    handleMotion'' (ModifiableUIElement _ e) = handleMotion'' e
    -- For anything else, do nothing.
    handleMotion'' e                         = return e

-- | The default background for a UI. Simply transparent black.
defaultBackground :: Picture
defaultBackground = color (withAlpha 0.5 black) $ rectangleSolid 1280 720

-- | Builds a map from a list of UI elements.
--   Internal use only.
buildMap :: [UIElement] -> Map.Map (Maybe ElementKey) [UIElement]
buildMap = Map.fromListWith (++) . map (\e -> (key' e, [e]))
  where
    key' :: UIElement -> Maybe ElementKey
    key' (ModifiableUIElement k _) = Just k
    key' _                         = Nothing

--- Utility constructor functions
-- | Constructs a UI from a list of UI elements.
--   Uses 'defaultBackground' as the background.
ui :: [UIElement] -> UI
ui es = uiWithBg es defaultBackground

-- | Constructs a UI from a list of UI elements and a background.
uiWithBg :: [UIElement] -> Picture -> UI
uiWithBg es = UI $ buildMap es

-- | Constructs a text UI element.
text :: String -> Justification -> Font -> TextSize -> Position -> UIElement
text = UIText

-- | Constructs an image UI element.
image :: Picture -> Float -> Position -> UIElement
image = UIImage

-- | Constructs a button UI element.
button :: String -> Font -> Position -> Size -> IO () -> UIElement
button = UIButton

-- | Constructs a slider UI element.
slider :: Position -> Size -> SliderValue -> (SliderValue -> IO ()) -> UIElement
slider p s v sa = UISlider p s v sa False

modifiable :: ElementKey -> UIElement -> UIElement
modifiable = ModifiableUIElement


-- Types used in this module
type Position      = (Float, Float)
type Size          = (Float, Float)
type AxialScale    = (Float, Float)
type MousePosition = Position
type BorderWidth   = Float
type TextSize      = Float
type SliderValue   = Float
type ElementKey    = String
