module Game.OrionsOutlaws.Rendering.UI 
  (
  -- * Types
    UI
  , UIElement
  , Justification
  , ElementKey
  , Position
  , MousePosition
  , AxialScale
  , BorderWidth
  , TextSize
  , SliderValue
  , ParentUI

  -- * Fields
  , elements
  , keyedElements
  , background
  , parent

  -- * Utility constructor functions
  , ui
  , uiWithBg
  , uik
  , uikbg
  , text
  , image
  , button
  , slider
  , enk
  , ek
  , withParent
  , withElements

  -- * Utility functions
  , modifyElement
  , addElement
  , addKeyedElement
  , allElements

  -- * Rendering functions
  , renderUI
  , renderUIElement

  -- * Event handling functions
  , handleMouse
  , handleMotion

  -- * Constants
  , defaultBackground
  ) where
  
import Graphics.Gloss.Data.Picture (Picture)
import Graphics.Gloss.Interface.IO.Game (KeyState)
import Game.OrionsOutlaws.Rendering.Font (Font)
import Data.Map (Map)
import Game.OrionsOutlaws.Util.Registry (RegistryEntry)

data UI
instance Show UI
instance Eq UI

elements      :: UI -> [UIElement]
keyedElements :: UI -> Map ElementKey UIElement
background    :: UI -> Picture
parent        :: UI -> Maybe (RegistryEntry UI)

data UIElement
instance Show UIElement
instance Eq UIElement

data Justification
instance Show Justification
instance Eq Justification

modifyElement :: UI -> ElementKey -> (UIElement -> UIElement) -> UI
renderUI :: Maybe MousePosition -> AxialScale -> UI -> Picture
renderUIElement :: Maybe MousePosition -> AxialScale -> UIElement -> Picture
handleMouse :: UI -> KeyState -> MousePosition -> AxialScale -> IO (Bool, UI)
handleMotion :: UI -> MousePosition -> AxialScale -> IO UI

defaultBackground :: Picture

ui :: [UIElement] -> UI
uiWithBg :: [UIElement] -> Picture -> UI
uik :: [(Maybe ElementKey, UIElement)] -> UI
uikbg :: [(Maybe ElementKey, UIElement)] -> Picture -> UI
withParent :: Maybe ParentUI -> UI -> UI
text :: String -> Justification -> Font -> TextSize -> Position -> UIElement
image :: Picture -> Float -> Position -> UIElement
button :: String -> Font -> Position -> Size -> IO () -> UIElement
slider :: Position -> Size -> SliderValue -> (SliderValue -> IO ()) -> UIElement
addElement :: UI -> UIElement -> UI
addKeyedElement :: UI -> ElementKey -> UIElement -> UI
allElements :: UI -> [(Maybe ElementKey, UIElement)]
withElements :: UI -> [(Maybe ElementKey, UIElement)] -> UI
enk :: UIElement -> (Maybe ElementKey, UIElement)
ek :: ElementKey -> UIElement -> (Maybe ElementKey, UIElement)

type Position      = (Float, Float)
type Size          = (Float, Float)
type AxialScale    = (Float, Float)
type MousePosition = Position
type BorderWidth   = Float
type TextSize      = Float
type SliderValue   = Float
type ElementKey    = String
type ParentUI      = RegistryEntry UI 
