module Game.OrionsOutlaws.UI.SettingsUI 
  ( settingsUI
  ) where

import Game.OrionsOutlaws.UI.Base (UI (UI), UIElement (..), defaultBackground, Justification (..))
import Game.OrionsOutlaws.Assets (pixeboyFont)
import Game.OrionsOutlaws.Tasks (queueTask)
import Game.OrionsOutlaws.Model (debugLog, GameState (keyListeners, settings, activeUI), Settings (..), Position)
import System.Log.Logger (debugM)
import Graphics.Gloss.Interface.IO.Game (Key (..), SpecialKey (KeyEsc), MouseButton (AdditionalButton))
import Data.Char (toUpper)
import Game.OrionsOutlaws.Data (writeSettings)

-- | Creates a settings UI for the given settings.
settingsUI :: Settings -> UI
settingsUI s = UI
  [ UIText "SETTINGS" JustCentered pixeboyFont 1 (0, 250)
  , UIText "PRESS A BUTTON AND THEN A KEY TO CONFIGURE THE KEYBIND" JustCentered pixeboyFont 0.2 (0, 190)
  , UIText "PRESS ESC TO CONTINUE" JustCentered pixeboyFont 0.3 (0, -220)

  , UIText "FORWARD" JustLeft pixeboyFont 0.35 (-150, 150)
  , keyBtn (75, 150) s forwardKey  (\s' k -> s' { forwardKey = k })

  , UIText "BACKWARD" JustLeft pixeboyFont 0.35 (-150, 100)
  , keyBtn (75, 100) s backwardKey (\s' k -> s' { backwardKey = k })

  , UIText "LEFT" JustLeft pixeboyFont 0.35 (-150, 50)
  , keyBtn (75, 50)  s leftKey     (\s' k -> s' { leftKey = k })

  , UIText "RIGHT" JustLeft pixeboyFont 0.35 (-150, 0)
  , keyBtn (75, 0)   s rightKey    (\s' k -> s' { rightKey = k })

  , UIText "FIRE" JustLeft pixeboyFont 0.35 (-150, -50)
  , keyBtn (75, -50) s fireKey     (\s' k -> s' { fireKey = k })
  ]
  defaultBackground

keyBtn :: Position -> Settings -> (Settings -> Key) -> (Settings -> Key -> Settings) -> UIElement
keyBtn p s getter setter = UIButton (keyToString $ getter s) pixeboyFont (75, 35) p $ onKeyBtn setter

-- | Queues a key change task.
onKeyBtn :: (Settings -> Key -> Settings) -> IO ()
onKeyBtn setter = queueTask $ \gs -> return gs { keyListeners = onKeyChange setter : keyListeners gs }

onKeyChange :: (Settings -> Key -> Settings) -> Key -> IO ()
onKeyChange setter key 
  | key == SpecialKey KeyEsc = return () -- Do nothing on escape
  | otherwise = queueTask $ \gs -> do
    debugM debugLog $ "Key pressed: " ++ show key
    let settings' = setter (settings gs) key
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
keyToString (MouseButton mb)                   = map toUpper $ show mb         -- MouseButton LeftButton           -> "LEFTBUTTON"
