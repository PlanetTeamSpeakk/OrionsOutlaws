module Game.OrionsOutlaws.UI.GameOverUI
  ( gameOverUI
  ) where
import Game.OrionsOutlaws.Rendering.UI  (Justification (..), ui, text, button, UI,)
import Game.OrionsOutlaws.Assets        (pixeboyFont)
import Game.OrionsOutlaws.Model         (GameState (score), openUI, projectiles, enemies, player, initialPlayer, health, animations, activeUI)
import Game.OrionsOutlaws.Util.Tasks    (queueTask)
import {-# SOURCE #-} Game.OrionsOutlaws.Util.Registries (getUI)

-- | The game over UI. Allows the player to play again or go to the scores.
gameOverUI :: GameState -> UI
gameOverUI gs = ui
  [ text "Game Over" JustCentered pixeboyFont 1.2 (0, 200)
  , text ("Score: " ++ show (score gs)) JustCentered pixeboyFont 0.35 (0, 100)
  , button "Play again" pixeboyFont (0, -150) (200, 70) onPlayAgainBtn
  , button "Scores" pixeboyFont (-565, 310) (100, 50) onScoresBtn -- TODO - this is temporary until we have a proper menu
  ]

-- | Called when the scores button is pressed.
onScoresBtn :: IO ()
onScoresBtn = queueTask $ \gs -> return $ openUI gs $ getUI gs "scores"

-- | Called when the play again button is pressed. Starts a new game.
onPlayAgainBtn :: IO ()
onPlayAgainBtn = queueTask $ \gs -> return gs
              { player      = initialPlayer { health = 3 }
              , enemies     = []
              , projectiles = []
              , animations  = []
              , score       = 0
              , activeUI   = Nothing
              }
