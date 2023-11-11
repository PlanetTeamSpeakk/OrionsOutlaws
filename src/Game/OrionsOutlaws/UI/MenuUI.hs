module Game.OrionsOutlaws.UI.MenuUI
  ( menuUI
  ) where
import Game.OrionsOutlaws.Rendering.UI  (Justification (..), ui, text, button, UI,)
import Game.OrionsOutlaws.Assets        (pixeboyFont)
import Game.OrionsOutlaws.Model         (GameState (settings, scores), openUI)
import Game.OrionsOutlaws.Util.Tasks    (queueTask)
import Game.OrionsOutlaws.UI.SettingsUI (settingsUI)
import Game.OrionsOutlaws.UI.ScoresUI   (scoresUI)

-- | The paused UI. Doesn't ever change and doesn't do much on its own.
menuUI :: UI
menuUI = ui
  [ text "Orion's Outlaws" JustCentered pixeboyFont 1.5 (0, 200)
  , text "Press ESC to continue" JustCentered pixeboyFont 0.5 (0, -150)
  , button "Settings" pixeboyFont (565, 310) (100, 50) onSettingsBtn
  , button "Scores" pixeboyFont (-565, 310) (100, 50) onScoresBtn -- TODO - this is temporary until we have a proper menu
  ]

-- | Called when the settings button is pressed.
onSettingsBtn :: IO ()
onSettingsBtn = queueTask $ \gs -> return $ openUI gs $ Just $ settingsUI $ settings gs

-- | Called when the scores button is pressed.
onScoresBtn :: IO ()
onScoresBtn = queueTask $ \gs -> return $ openUI gs $ Just $ scoresUI $ scores gs
