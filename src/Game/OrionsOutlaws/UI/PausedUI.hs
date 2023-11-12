module Game.OrionsOutlaws.UI.PausedUI
  ( pausedUI
  ) where
import Game.OrionsOutlaws.Rendering.UI  (Justification (..), ui, text, button, UI,)
import Game.OrionsOutlaws.Assets        (pixeboyFont)
import Game.OrionsOutlaws.Model         (openUI)
import Game.OrionsOutlaws.Util.Tasks    (queueTask)
import {-# SOURCE #-} Game.OrionsOutlaws.Util.Registries (getUI)

-- | The paused UI. Doesn't ever change and doesn't do much on its own.
pausedUI :: UI
pausedUI = ui
  [ text "Paused" JustCentered pixeboyFont 1.5 (0, 200)
  , text "Press ESC to continue" JustCentered pixeboyFont 0.5 (0, -150)
  , button "Settings" pixeboyFont (565, 310) (100, 50) onSettingsBtn
  , button "Scores" pixeboyFont (-565, 310) (100, 50) onScoresBtn -- TODO - this is temporary until we have a proper menu
  ]

-- | Called when the settings button is pressed.
onSettingsBtn :: IO ()
onSettingsBtn = queueTask $ \gs -> return $ openUI gs $ getUI gs "settings"

-- | Called when the scores button is pressed.
onScoresBtn :: IO ()
onScoresBtn = queueTask $ \gs -> return $ openUI gs $ getUI gs "scores"
