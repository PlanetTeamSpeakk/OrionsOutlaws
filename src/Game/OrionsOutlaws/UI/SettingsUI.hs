module Game.OrionsOutlaws.UI.SettingsUI 
  ( settingsUI
  ) where

import Game.OrionsOutlaws.UI.Base (UI (UI), UIElement (..), defaultBackground, Justification (..))
import Game.OrionsOutlaws.Assets (pixeboyFont)
import Game.OrionsOutlaws.Tasks (queueTask)
import Game.OrionsOutlaws.Model (debugLog, GameState (keyListeners, settings, activeUI), Settings (..), Position)
import System.Log.Logger (debugM)
import Graphics.Gloss.Interface.IO.Game (Key (..))
import Data.Char (toUpper)

settingsUI :: Settings -> UI
settingsUI s@Settings
  { forwardKey  = fk
  , backwardKey = bk
  , leftKey     = lk
  , rightKey    = rk
  , fireKey     = sk
  } = UI
  [ UIText "SETTINGS" JustCentered pixeboyFont 1 (0, 220)
  , UIText "PRESS ESC TO CONTINUE" JustCentered pixeboyFont 0.3 (0, -220)
  , keyBtn (50, 0) s forwardKey (\s' k -> s' { forwardKey = k })
  -- , UIButton "TEST" pixeboyFont (100, 50) (0, 0) $ onKeyBtn forwardKey (\sts k -> sts { forwardKey = k })
  ]
  defaultBackground

keyBtn :: Position -> Settings -> (Settings -> Key) -> (Settings -> Key -> Settings) -> UIElement
keyBtn p s getter setter = UIButton (keyToString $ getter s) pixeboyFont (100, 50) p $ onKeyBtn getter setter

-- | Queues a key change task.
onKeyBtn :: (Settings -> Key) -> (Settings -> Key -> Settings) -> IO ()
onKeyBtn getter setter = queueTask $ \gs -> return gs { keyListeners = onKey setter : keyListeners gs }

onKey :: (Settings -> Key -> Settings) -> Key -> IO ()
onKey setter key = queueTask $ \gs -> do
  debugM debugLog $ "Key pressed: " ++ show key
  let settings' = setter (settings gs) key
  return gs 
    { settings = settings'
    , activeUI = Just $ settingsUI settings'
    }

-- | Converts a key to a string.
keyToString :: Key -> String
keyToString (Char c)         = [toUpper c]
keyToString (SpecialKey k)   = show k
keyToString (MouseButton mb) = show mb
