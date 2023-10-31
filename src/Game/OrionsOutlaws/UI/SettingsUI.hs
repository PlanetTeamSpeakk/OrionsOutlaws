module Game.OrionsOutlaws.UI.SettingsUI
  ( settingsUI
  ) where

import Game.OrionsOutlaws.Rendering.UI  (UIElement (..), Justification (..), modifyElement, ElementKey, ui, UI, text, slider, modifiable)
import Game.OrionsOutlaws.Assets        (pixeboyFont)
import Game.OrionsOutlaws.Util.Tasks    (queueTask)
import Game.OrionsOutlaws.Model         (GameState (keyListeners, settings, activeUI), Settings (..), Position)
import Game.OrionsOutlaws.Util.Data     (writeSettings)
import Game.OrionsOutlaws.Util.Audio    (setVolume)
import Graphics.Gloss.Interface.IO.Game (Key (..), SpecialKey (KeyEsc), MouseButton (..))
import Data.Char                        (toUpper)
import Data.Maybe                       (fromJust)

-- | Creates a settings UI for the given settings.
settingsUI :: Settings -> UI
settingsUI s = ui
  [ text "Settings" JustCentered pixeboyFont 1 (0, 250)
  , text "Press ESC to continue" JustCentered pixeboyFont 0.3 (0, -220)

  , text "Forward" JustLeft pixeboyFont 0.35 (-150, 150)
  , keyBtn "fwd" (75, 150) s forwardKey  (\s' k -> s' { forwardKey = k })

  , text "Backward" JustLeft pixeboyFont 0.35 (-150, 100)
  , keyBtn "bwd" (75, 100) s backwardKey (\s' k -> s' { backwardKey = k })

  , text "Left" JustLeft pixeboyFont 0.35 (-150, 50)
  , keyBtn "lft" (75, 50)  s leftKey     (\s' k -> s' { leftKey = k })

  , text "Right" JustLeft pixeboyFont 0.35 (-150, 0)
  , keyBtn "rgt" (75, 0)   s rightKey    (\s' k -> s' { rightKey = k })

  , text "Fire" JustLeft pixeboyFont 0.35 (-150, -50)
  , keyBtn "fir" (75, -50) s fireKey     (\s' k -> s' { fireKey = k })

  , text "Volume" JustLeft pixeboyFont 0.35 (-150, -100)
  , slider (75, -100) (180, 20) (volume s) onVolumeChange
  ]

-- | Creates a new keybind button for a keybind.
keyBtn :: ElementKey -> Position -> Settings -> (Settings -> Key) -> (Settings -> Key -> Settings) -> UIElement
keyBtn k p s getter setter = modifiable k $ UIButton (keyToString $ getter s) pixeboyFont p (75, 35) $ onKeyBtn k setter

-- | Queues a key change task.
--   Called when a keybind button is pressed.
onKeyBtn :: ElementKey -> (Settings -> Key -> Settings) -> IO ()
onKeyBtn k setter = queueTask $ \gs -> do
  return gs
    { keyListeners = onKeyChange setter : keyListeners gs
    , activeUI = Just $ modifyElement (fromJust $ activeUI gs) k setBtnText
    }
  where
    setBtnText :: UIElement -> UIElement
    setBtnText e@ModifiableUIElement { element = b@(UIButton {})} = e { element = b { btnText = "Press a key" } }
    setBtnText e = e

-- | Sets the keybind to the given key.
--   Called after a keybind button was pressed and the user pressed a key.
onKeyChange :: (Settings -> Key -> Settings) -> Key -> IO ()
onKeyChange setter k
  | k == SpecialKey KeyEsc = return () -- Do nothing on escape
  | otherwise = queueTask $ \gs -> do
    let settings' = setter (settings gs) k
    writeSettings settings'
    return gs
      { settings = settings'
      , activeUI = Just $ settingsUI settings'
      }

-- | Called when the volume slider is changed.
onVolumeChange :: Float -> IO ()
onVolumeChange v = queueTask $ \gs -> do
  let settings' = (settings gs) { volume = v }
  writeSettings settings'
  setVolume v
  return gs { settings = settings' }

-- | Converts a key to a string.
keyToString :: Key -> String
keyToString (Char c)                           = [toUpper c]                                          -- Char 'e'            -> "E"
keyToString (SpecialKey k)                     = let s = drop 3 $ show k in toUpper (head s) : tail s -- SpecialKey KeySpace -> "Space"
keyToString (MouseButton (AdditionalButton n)) = "Button " ++ show n           -- MouseButton (AdditionalButton 4) -> "Button 4"
keyToString (MouseButton LeftButton)           = "LMB"                         -- MouseButton LeftButton           -> "LMB"
keyToString (MouseButton MiddleButton)         = "MMB"                         -- MouseButton MiddleButton         -> "MMB"
keyToString (MouseButton RightButton)          = "RMB"                         -- MouseButton RightButton          -> "RMB"
keyToString (MouseButton WheelUp)              = "Wheel Up"                    -- MouseButton WheelUp              -> "Wheel Up"
keyToString (MouseButton WheelDown)            = "Wheel Down"                  -- MouseButton WheelDown            -> "Wheel Down"
