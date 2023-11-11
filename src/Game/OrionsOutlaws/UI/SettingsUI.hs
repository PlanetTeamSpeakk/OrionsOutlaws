module Game.OrionsOutlaws.UI.SettingsUI
  ( settingsUI
  ) where

import Game.OrionsOutlaws.Rendering.UI  (UIElement (..), Justification (..), modifyElement, ElementKey, UI, text, slider, uik, enk, button)
import Game.OrionsOutlaws.Assets        (pixeboyFont)
import Game.OrionsOutlaws.Util.Tasks    (queueTask)
import Game.OrionsOutlaws.Model         (GameState (keyListeners, settings, activeUI), Settings (..), Position)
import Game.OrionsOutlaws.Util.Data     (writeSettings)
import Game.OrionsOutlaws.Util.Audio    (setVolume)
import Graphics.Gloss.Interface.IO.Game (Key (..), SpecialKey (KeyEsc), MouseButton (..))
import Data.Char                        (toUpper)
import {-# SOURCE #-} Game.OrionsOutlaws.Util.Registries (getUI)

-- | Creates a settings UI for the given settings.
settingsUI :: Settings -> UI
settingsUI s = uik
  [ enk $ text "Settings" JustCentered pixeboyFont 1 (0, 250)
  , enk $ text "Press ESC to continue" JustCentered pixeboyFont 0.3 (0, -220)

  , enk $ text "Forward" JustLeft pixeboyFont 0.35 (-150, 150)
  , keyBtn "fwd" (75, 150) s forwardKey  (\s' k -> s' { forwardKey = k })

  , enk $ text "Backward" JustLeft pixeboyFont 0.35 (-150, 100)
  , keyBtn "bwd" (75, 100) s backwardKey (\s' k -> s' { backwardKey = k })

  , enk $ text "Left" JustLeft pixeboyFont 0.35 (-150, 50)
  , keyBtn "lft" (75, 50)  s leftKey     (\s' k -> s' { leftKey = k })

  , enk $ text "Right" JustLeft pixeboyFont 0.35 (-150, 0)
  , keyBtn "rgt" (75, 0)   s rightKey    (\s' k -> s' { rightKey = k })

  , enk $ text "Fire" JustLeft pixeboyFont 0.35 (-150, -50)
  , keyBtn "fir" (75, -50) s fireKey     (\s' k -> s' { fireKey = k })

  , enk $ text "Volume" JustLeft pixeboyFont 0.35 (-150, -100)
  , enk $ slider (75, -100) (180, 20) (volume s) onVolumeChange

  , enk $ text "Spawn" JustLeft pixeboyFont 0.25 (-150, -140)
  , enk $ text "Rate" JustLeft pixeboyFont 0.25 (-143, -160)
  , enk $ slider (75, -150) (180, 20) (spawnRate s / 4) onSpawnRateChange
  ]

-- | Creates a new keybind button for a keybind.
keyBtn :: ElementKey -> Position -> Settings -> (Settings -> Key) -> (Settings -> Key -> Settings) -> (Maybe ElementKey, UIElement)
keyBtn k p s getter setter = (Just k, button (keyToString $ getter s) pixeboyFont p (75, 35) $ onKeyBtn k setter)

-- | Queues a key change task.
--   Called when a keybind button is pressed.
onKeyBtn :: ElementKey -> (Settings -> Key -> Settings) -> IO ()
onKeyBtn k setter = queueTask $ \gs -> do
  return gs
    { keyListeners = onKeyChange setter : keyListeners gs
    , activeUI = (\e -> (\ui -> modifyElement ui k setBtnText) <$> e) <$> activeUI gs
    }
  where
    setBtnText :: UIElement -> UIElement
    setBtnText e@(UIButton {}) = e { btnText = "Press a key" }
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
      , activeUI = getUI gs "settings"
      }

-- | Called when the volume slider is changed.
onVolumeChange :: Float -> IO ()
onVolumeChange v = queueTask $ \gs -> do
  let settings' = (settings gs) { volume = v }
  writeSettings settings'
  setVolume v
  return gs { settings = settings' }

onSpawnRateChange :: Float -> IO ()
onSpawnRateChange v = queueTask $ \gs -> do
  let settings' = (settings gs) { spawnRate = v * 4 }
  writeSettings settings'
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
