module Game.OrionsOutlaws.UI.MenuUI
  ( menuUI
  ) where
import Game.OrionsOutlaws.Rendering.UI    (Justification (..), ui, text, button, UI,)
import Game.OrionsOutlaws.Assets          (pixeboyFont)
import Game.OrionsOutlaws.Model           (openUI)
import Game.OrionsOutlaws.Util.Tasks      (queueTask)
import {-# SOURCE #-} Game.OrionsOutlaws.Util.Registries (getUI)

-- | The paused UI. Doesn't ever change and doesn't do much on its own.
menuUI :: UI
menuUI = ui
  [ text "Orion's Outlaws" JustCentered pixeboyFont 1.3 (0, 200)
  , button "Start game" pixeboyFont (0, 70) (300, 100) onPlayBtn
  , button "Settings" pixeboyFont (0, -50) (200, 100) onSettingsBtn
  , button "Scores" pixeboyFont (0, -170) (180, 100) onScoresBtn -- TODO - this is temporary until we have a proper menu
  ]

-- | Called when the settings button is pressed.
onSettingsBtn :: IO ()
onSettingsBtn = queueTask $ \gs -> return $ openUI gs $ getUI gs "settings"

-- | Called when the scores button is pressed.
onScoresBtn :: IO ()
onScoresBtn = queueTask $ \gs -> return $ openUI gs $ getUI gs "scores"

onPlayBtn :: IO ()
onPlayBtn = queueTask $ \gs -> return $ openUI gs Nothing
