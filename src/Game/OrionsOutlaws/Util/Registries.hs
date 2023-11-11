module Game.OrionsOutlaws.Util.Registries
  ( animationsRegistry
  , uiRegistry
  , getUI
  ) where

import Game.OrionsOutlaws.Model (Animation, GameState(..))
import Game.OrionsOutlaws.Assets (explosionAnimation)
import Game.OrionsOutlaws.Rendering.UI (UI)
import Game.OrionsOutlaws.UI.PausedUI (pausedUI)
import Game.OrionsOutlaws.UI.SettingsUI (settingsUI)
import Game.OrionsOutlaws.Util.Registry (Registry(..), mkRegistry, getEntry, RegistryEntry)

-- | A registry of animations.
animationsRegistry :: Registry Animation
animationsRegistry = mkRegistry "animations"
  [ ("explosion", explosionAnimation)
  ]

-- | A registry of UIs.
uiRegistry :: Registry (GameState -> UI)
uiRegistry = mkRegistry "ui"
  [ ("mainMenu", undefined)
  , ("paused", const pausedUI)
  , ("settings", settingsUI . settings)
  ]

-- | Get a UI from the registry by its key.
getUI :: GameState -> String -> Maybe (RegistryEntry UI)
getUI gs k = (\e -> (\f -> f gs) <$> e) <$> getEntry uiRegistry k
