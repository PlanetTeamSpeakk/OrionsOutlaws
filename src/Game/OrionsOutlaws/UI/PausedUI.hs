module Game.OrionsOutlaws.UI.PausedUI
  ( pausedUI
  ) where
import Game.OrionsOutlaws.UI.Base (UI (UI), defaultBackground, UIElement (..), Justification (..))
import Game.OrionsOutlaws.Assets (pixeboyFont)
import System.Log.Logger (debugM)
import Game.OrionsOutlaws.Model (debugLog, GameState (paused, activeUI))
import Game.OrionsOutlaws.Tasks (queueTask)

pausedUI :: UI
pausedUI = UI
  [ UIText "PAUSED" JustCentered pixeboyFont 1.5 (0, 200)
  , UIText "PRESS ESC TO CONTINUE" JustCentered pixeboyFont 0.5 (0, -150)
  , UIButton "TEST" pixeboyFont (100, 50) (70, 90) onTestBtn
  ]
  defaultBackground

onTestBtn :: IO ()
onTestBtn = queueTask $ \gs -> do
  debugM debugLog "Test button pressed"
  return gs { paused = False, activeUI = Nothing }
