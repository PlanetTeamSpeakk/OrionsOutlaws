-- | This module defines how the state changes
--   in response to time and user input
module Controller where

import Model
import Graphics.Gloss.Interface.IO.Game
import Control.Monad (unless)
import System.Log.Logger (debugM)

-- | Handle one iteration of the game
step :: Float -> GameState -> IO GameState
step _ gstate = do
    return gstate { projectiles = stepProjectiles gstate $ projectiles gstate }

-- Moves all projectiles forward
stepProjectiles :: GameState -> [Projectile] -> [Projectile]
stepProjectiles gstate = filter onScreen . map stepProjectile
    where
        stepProjectile p = p { projPos = (fst (projPos p) + (speed p * 10), snd $ projPos p) }
        -- Ensure the given coordinates are on the screen
        onScreen (RegularProjectile (x, y) _ _) = 
          let (wx, wy)   = windowSize gstate
              (hwx, hwy) = (fromIntegral wx / 2, fromIntegral wy / 2) in
            x > -hwx && x < hwx && y > -hwy && y < hwy


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


createProjectile :: Position -> Bool -> Projectile
createProjectile pos f = RegularProjectile pos f 1
