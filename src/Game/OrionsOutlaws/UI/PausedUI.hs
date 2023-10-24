module Game.OrionsOutlaws.UI.PausedUI
  ( pausedUI
  ) where
import Game.OrionsOutlaws.UI.Base (UI (UI), defaultBackground, UIElement (..), Justification (..))
import Game.OrionsOutlaws.Assets (pixeboyFont)
import Game.OrionsOutlaws.Model (GameState (activeUI, settings))
import Game.OrionsOutlaws.Tasks (queueTask)
import Game.OrionsOutlaws.UI.SettingsUI (settingsUI)

pausedUI :: UI
pausedUI = UI
  [ UIText "PAUSED" JustCentered pixeboyFont 1.5 (0, 200)
  , UIText "PRESS ESC TO CONTINUE" JustCentered pixeboyFont 0.5 (0, -150)
  , UIButton "SETTINGS" pixeboyFont (100, 50) (565, 310) onSettingsBtn
  ]
  defaultBackground

onSettingsBtn :: IO ()
onSettingsBtn = queueTask $ \gs -> return gs { activeUI = Just $ settingsUI $ settings gs }
