module Game.OrionsOutlaws.UI.PausedUI
  ( pausedUI
  ) where
import Game.OrionsOutlaws.Rendering.UI (Justification (..), ui, UI, text, button)
import Game.OrionsOutlaws.Assets (pixeboyFont)
import Game.OrionsOutlaws.Model (GameState (activeUI, settings, scores), debugLog)
import Game.OrionsOutlaws.Util.Tasks (queueTask)
import Game.OrionsOutlaws.UI.SettingsUI (settingsUI)
import Game.OrionsOutlaws.UI.ScoresUI (scoresUI)
import System.Log.Logger (debugM)

pausedUI :: UI
pausedUI = ui
  [ text "PAUSED" JustCentered pixeboyFont 1.5 (0, 200)
  , text "PRESS ESC TO CONTINUE" JustCentered pixeboyFont 0.5 (0, -150)
  , button "SETTINGS" pixeboyFont (565, 310) (100, 50) onSettingsBtn
  , button "SCORES" pixeboyFont (-565, 310) (100, 50) onScoresBtn -- TODO - this is temporary until we have a proper menu
  ]

onSettingsBtn :: IO ()
onSettingsBtn = queueTask $ \gs -> return gs { activeUI = Just $ settingsUI $ settings gs }

onScoresBtn :: IO ()
onScoresBtn = queueTask $ \gs -> do
  debugM debugLog $ "Scores: " ++ show (scores gs)
  return gs { activeUI = Just $ scoresUI $ scores gs }
