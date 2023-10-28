module Game.OrionsOutlaws.UI.PausedUI
  ( pausedUI
  ) where
import Game.OrionsOutlaws.Rendering.UI (Justification (..), ui, UI, text, button)
import Game.OrionsOutlaws.Assets (pixeboyFont)
import Game.OrionsOutlaws.Model (GameState (activeUI, settings))
import Game.OrionsOutlaws.Util.Tasks (queueTask)
import Game.OrionsOutlaws.UI.SettingsUI (settingsUI)

pausedUI :: UI
pausedUI = ui
  [ text "PAUSED" JustCentered pixeboyFont 1.5 (0, 200)
  , text "PRESS ESC TO CONTINUE" JustCentered pixeboyFont 0.5 (0, -150)
  , button "SETTINGS" pixeboyFont (565, 310) (100, 50) onSettingsBtn
  ]

onSettingsBtn :: IO ()
onSettingsBtn = queueTask $ \gs -> return gs { activeUI = Just $ settingsUI $ settings gs }
