module Game.OrionsOutlaws.Util.Registries
  ( animationsRegistry
  , uiRegistry
  , getUI
  ) where

import Game.OrionsOutlaws.Util.Registry (Registry, RegistryEntry)
import Game.OrionsOutlaws.Model (Animation, GameState)
import Game.OrionsOutlaws.Rendering.UI (UI)

animationsRegistry :: Registry Animation
uiRegistry :: Registry (GameState -> UI)
getUI :: GameState -> String -> Maybe (RegistryEntry UI)
