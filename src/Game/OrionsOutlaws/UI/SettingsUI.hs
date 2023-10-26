module Game.OrionsOutlaws.UI.SettingsUI
  ( settingsUI
  ) where

import Game.OrionsOutlaws.UI.Base (UI (UI), UIElement (..), defaultBackground, Justification (..), modifyElement, ElementKey)
import Game.OrionsOutlaws.Assets (pixeboyFont)
import Game.OrionsOutlaws.Tasks (queueTask)
import Game.OrionsOutlaws.Model (GameState (keyListeners, settings, activeUI), Settings (..), Position)
import Graphics.Gloss.Interface.IO.Game (Key (..), SpecialKey (KeyEsc), MouseButton (..))
import Data.Char (toUpper)
import Game.OrionsOutlaws.Data (writeSettings)
import Data.Maybe (fromJust)
import Game.OrionsOutlaws.Audio (setVolume)

-- | Creates a settings UI for the given settings.
settingsUI :: Settings -> UI
settingsUI s = UI
  [ UIText "SETTINGS" JustCentered pixeboyFont 1 (0, 250)
  , UIText "PRESS A BUTTON AND THEN A KEY TO CONFIGURE THE KEYBIND" JustCentered pixeboyFont 0.2 (0, 190)
  , UIText "PRESS ESC TO CONTINUE" JustCentered pixeboyFont 0.3 (0, -220)

  , UIText "FORWARD" JustLeft pixeboyFont 0.35 (-150, 150)
  , keyBtn "fwd" (75, 150) s forwardKey  (\s' k -> s' { forwardKey = k })

  , UIText "BACKWARD" JustLeft pixeboyFont 0.35 (-150, 100)
  , keyBtn "bwd" (75, 100) s backwardKey (\s' k -> s' { backwardKey = k })

  , UIText "LEFT" JustLeft pixeboyFont 0.35 (-150, 50)
  , keyBtn "lft" (75, 50)  s leftKey     (\s' k -> s' { leftKey = k })

  , UIText "RIGHT" JustLeft pixeboyFont 0.35 (-150, 0)
  , keyBtn "rgt" (75, 0)   s rightKey    (\s' k -> s' { rightKey = k })

  , UIText "FIRE" JustLeft pixeboyFont 0.35 (-150, -50)
  , keyBtn "fir" (75, -50) s fireKey     (\s' k -> s' { fireKey = k })

  , UIText "VOLUME" JustLeft pixeboyFont 0.35 (-150, -100)
  , UISlider (75, -100) (180, 20) (volume s) onVolumeChange False
  ]
  defaultBackground

-- | Creates a new keybind button for a keybind.
keyBtn :: ElementKey -> Position -> Settings -> (Settings -> Key) -> (Settings -> Key -> Settings) -> UIElement
keyBtn k p s getter setter = ModifiableUIElement k $ UIButton (keyToString $ getter s) pixeboyFont p (75, 35) $ onKeyBtn k setter

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
    setBtnText e@ModifiableUIElement { element = b@(UIButton {})} = e { element = b { text = "PRESS A KEY" } }
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

-- | Converts a key to a string.
keyToString :: Key -> String
keyToString (Char c)                           = [toUpper c]                   -- Char 'e'                         -> "E"
keyToString (SpecialKey k)                     = map toUpper $ drop 3 $ show k -- SpecialKey KeySpace              -> "SPACE"
keyToString (MouseButton (AdditionalButton n)) = "BUTTON " ++ show n           -- MouseButton (AdditionalButton 4) -> "BUTTON 4"
keyToString (MouseButton LeftButton)           = "LMB"                         -- MouseButton LeftButton           -> "LMB"
keyToString (MouseButton MiddleButton)         = "MMB"                         -- MouseButton MiddleButton         -> "MMB"
keyToString (MouseButton RightButton)          = "RMB"                         -- MouseButton RightButton          -> "RMB"
keyToString (MouseButton WheelUp)              = "WHEEL UP"                    -- MouseButton WheelUp              -> "WHEEL UP"
keyToString (MouseButton WheelDown)            = "WHEEL DOWN"                  -- MouseButton WheelDown            -> "WHEEL DOWN"

-- | Called when the volume slider is changed.
onVolumeChange :: Float -> IO ()
onVolumeChange v = queueTask $ \gs -> do
  let settings' = (settings gs) { volume = v }
  writeSettings settings'
  setVolume v
  return gs { settings = settings' }
