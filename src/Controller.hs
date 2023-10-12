-- | This module defines how the state changes
--   in response to time and user input
module Controller where

import Model
import Graphics.Gloss.Interface.IO.Game
import Control.Monad (unless)
import System.Log.Logger (debugM)
import View (onScreen)
import Util (msTime)

-- | Handle one iteration of the game
step :: Float -> GameState -> IO GameState
step _ gstate = do
    time <- msTime
    return gstate {
      player      = stepPlayer $ player gstate,
      projectiles = stepProjectiles $ projectiles gstate,
      lastStep    = time
    }
    where
      -- Moves all projectiles forward
      stepProjectiles :: [Projectile] -> [Projectile]
      stepProjectiles = filter (onScreen gstate . position) . map stepProjectile
        where
          stepProjectile p = p { 
            prevProjPos = projPos p,
            projPos = (fst (projPos p) + (speed p * projectileSpeed), snd $ projPos p) 
          }

      -- Decreases the player's cooldown
      stepPlayer :: Player -> Player
      stepPlayer p = p { cooldown = max 0 $ cooldown p - 1 }

-- | Handle user input
input :: Event -> GameState -> IO GameState
-- Mouse events
input e@(EventKey (MouseButton _) Down _ _) gstate = do
  ret <- inputMouse e gstate
  unless (ret == gstate) $ debugM debugLog "New state "
  return ret
input e@(EventKey (SpecialKey _) Down _ _) gstate = do
  ret <- inputKey e gstate
  unless (ret == gstate) $ debugM debugLog $ "New state " ++ show ret
  return ret
input e@(EventResize (nx, ny)) gstate = do
  debugM debugLog $ "Resizing window to " ++ show nx ++ "x" ++ show ny
  return gstate
input _ gstate = return gstate

inputMouse :: Event -> GameState -> IO GameState
inputMouse (EventKey (MouseButton btn) Down _ p@(x, y)) gstate = do
  debugM debugLog $ "Mouse pressed " ++ unwords [show btn, show x, show y]
  return gstate
inputMouse _ gstate = return gstate

inputKey :: Event -> GameState -> IO GameState
inputKey (EventKey (SpecialKey KeySpace) Down _ _) gstate = do
  if cooldown (player gstate) == 0
    then do
      let (px, py) = playerPos $ player gstate
      let proj = createProjectile (px + (playerSize / 2) + 2.5, py) True
      return $ gstate { projectiles = proj : projectiles gstate, player = (player gstate) { cooldown = 5 } }
    else do
      return gstate
inputKey _ gstate = return gstate
