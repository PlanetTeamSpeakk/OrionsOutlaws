module Game.OrionsOutlaws.UI.PausedUI
  ( pausedUI
  ) where
import Game.OrionsOutlaws.UI.Base (UI (UI), defaultBackground, UIElement (..), Justification (..))
import Game.OrionsOutlaws.Assets (pixeboyFont)
import System.Log.Logger (debugM)
import Game.OrionsOutlaws.Model (debugLog, GameState (activeUI, settings))
import Game.OrionsOutlaws.Tasks (queueTask)
import Game.OrionsOutlaws.UI.SettingsUI (settingsUI)

pausedUI :: UI
pausedUI = UI
  [ UIText "PAUSED" JustCentered pixeboyFont 1.5 (0, 200)
  , UIText "PRESS ESC TO CONTINUE" JustCentered pixeboyFont 0.5 (0, -150)
  , UIButton "SETTINGS" pixeboyFont (100, 50) (0, 0) onSettingsBtn
  ]
  defaultBackground

onSettingsBtn :: IO ()
onSettingsBtn = queueTask $ \gs -> do
  debugM debugLog "Test button pressed on paused menu"
  return gs { activeUI = Just $ settingsUI $ settings gs }
