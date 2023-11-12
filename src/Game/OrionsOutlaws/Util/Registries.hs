module Game.OrionsOutlaws.Util.Registries
  ( animationsRegistry
  , uiRegistry
  , getUI
  ) where

import Game.OrionsOutlaws.Model         (Animation, GameState(..))
import Game.OrionsOutlaws.Assets        (explosionAnimation)
import Game.OrionsOutlaws.Rendering.UI  (UI)
import Game.OrionsOutlaws.UI.PausedUI   (pausedUI)
import Game.OrionsOutlaws.UI.SettingsUI (settingsUI)
import Game.OrionsOutlaws.Util.Registry (Registry(..), mkRegistry, getEntry, RegistryEntry)
import Game.OrionsOutlaws.UI.MenuUI     (menuUI)
import Game.OrionsOutlaws.UI.GameOverUI (gameOverUI)
import Game.OrionsOutlaws.UI.ScoresUI (scoresUI)

-- | A registry of animations.
animationsRegistry :: Registry Animation
animationsRegistry = mkRegistry "animations"
  [ ("explosion", explosionAnimation)
  ]

-- | A registry of UIs.
uiRegistry :: Registry (GameState -> UI)
uiRegistry = mkRegistry "ui"
  [ ("menu", menuUI . Just) -- Not really correct, but this isn't really used anyway.
  , ("paused", const pausedUI)
  , ("settings", settingsUI . settings)
  , ("scores", scoresUI . scores)
  , ("gameOver", gameOverUI)
  ]

-- | Get a UI from the registry by its key.
getUI :: GameState -> String -> Maybe (RegistryEntry UI)
getUI gs k = (\e -> (\f -> f gs) <$> e) <$> getEntry uiRegistry k
