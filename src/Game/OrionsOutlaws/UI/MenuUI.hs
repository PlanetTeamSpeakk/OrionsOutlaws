{-# OPTIONS_GHC -Wno-incomplete-record-updates #-} -- We are certain the element is a button, this is not a problem.
module Game.OrionsOutlaws.UI.MenuUI
  ( menuUI
  ) where
import Game.OrionsOutlaws.Rendering.UI    (Justification (..), ui, text, button, UI, UIElement (btnEnabled),)
import Game.OrionsOutlaws.Assets          (pixeboyFont)
import Game.OrionsOutlaws.Model           (openUI, GameState)
import Game.OrionsOutlaws.Util.Tasks      (queueTask)
import Data.Maybe                         (isJust)
import {-# SOURCE #-} Game.OrionsOutlaws.Util.Registries (getUI)

-- | The paused UI. Doesn't ever change and doesn't do much on its own.
menuUI :: Maybe GameState -> UI
menuUI gs = ui
  [ text "Orion's Outlaws" JustCentered pixeboyFont 1.3 (0, 200)
  , button "Play" pixeboyFont (0, 70) (300, 100) onPlayBtn
  , (button "Continue" pixeboyFont (0, -50) (300, 100) $ onContinueBtn gs) { btnEnabled = isJust gs }
  , button "Settings" pixeboyFont (0, -155) (200, 70) onSettingsBtn
  , button "Scores" pixeboyFont (0, -245) (180, 70) onScoresBtn
  ]

-- | Called when the settings button is pressed, opens the settings UI.
onSettingsBtn :: IO ()
onSettingsBtn = queueTask $ \gs -> return $ openUI gs $ getUI gs "settings"

-- | Called when the scores button is pressed, opens the scores UI.
onScoresBtn :: IO ()
onScoresBtn = queueTask $ \gs -> return $ openUI gs $ getUI gs "scores"

-- | Called when the play button is pressed, closes the UI and starts the game.
onPlayBtn :: IO ()
onPlayBtn = queueTask $ \gs -> return $ openUI gs Nothing

-- | Called when the continue button is pressed, closes the UI and starts the game, restoring the old gamestate.
onContinueBtn :: Maybe GameState -> IO ()
onContinueBtn Nothing = return ()
onContinueBtn (Just gs) = queueTask $ \_ -> return gs
