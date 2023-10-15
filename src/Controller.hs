{-# OPTIONS_GHC -Wno-missing-export-lists #-}
-- | This module defines how the state changes
--   in response to time and user input
module Controller where

import Model
import Graphics.Gloss.Interface.IO.Game
import System.Log.Logger (debugM)
import View (onScreen, inBounds)
import Util (msTime)
import System.Random (randomIO)
import Data.Bifunctor (first, bimap)
import Data.List ((\\)) -- List difference
import System.Exit (exitSuccess)

-- | Handle one iteration of the game
step :: Float -> GameState -> IO GameState
step elapsed gstate = do
    -- Add elapsed time to the gamestate
    if paused gstate
      then return gstate -- Do nothing if paused
    else do
      let gstateNew = gstate { elapsedTime = elapsedTime gstate + elapsed}

      -- Try to spawn an enemy
      gstateN2 <- trySpawnEnemy gstateNew

      let p = stepProjectiles $ projectiles gstateN2
      -- Handle projectile collision with enemies. Filters projectiles and enemies.
      let (fps, es) = handleProjectileCollision (filter friendly p) $ enemies gstateN2

      -- Filter out projectiles that collide with the player
      let nfps  = filter (not . friendly) p
      let cnfps = filter (collidesWith (player gstate)) nfps

      -- Check if any enemies collide with the player
      let eCollision = any (collidesWith (player gstate)) es

      if null cnfps && not eCollision
        then do -- No collision, return new gamestate
          ep <- enemyFire es

          time <- msTime
          return gstateN2 {
            player      = stepPlayer $ player gstateN2,
            enemies     = stepEnemies $ fst ep, -- Step left-over enemies
            -- Friendly projectiles, non-friendly projectiles that didn't collide with the player, and new enemy projectiles
            projectiles = fps ++ (nfps \\ cnfps) ++ snd ep,
            lastStep    = time
          }
        else -- Collision! Exit game if the player has 1 health left, otherwise reset and subtract 1 health
          if health (player gstateN2) == 1
            then do 
              _ <- exitSuccess
              return gstateN2
            else do
              debugM debugLog $ "Collision! " ++ show cnfps
              return gstateN2 {
                player = initialPlayer { health = health (player gstateN2) - 1 },
                enemies = [],
                projectiles = []
              }
    where
      -- Moves all projectiles forward
      stepProjectiles :: [Projectile] -> [Projectile]
      stepProjectiles = filter (onScreen gstate . curPosition) . map stepProjectile
        where
          stepProjectile p = applyMovement (windowSize gstate) p $ speed p * projectileSpeed

      -- Handles projectile collision. For each projectile, check if it collides with any enemy.
      -- If it does, remove the projectile and the enemy it collided with.
      handleProjectileCollision :: [Projectile] -> [Enemy] -> ([Projectile], [Enemy])
      handleProjectileCollision ps [] = (ps, []) -- No enemies, no collision
      handleProjectileCollision [] es = ([], es) -- No projectiles, no collision
      handleProjectileCollision (p:ps) es = let (ps', es') = handleProjectileCollision ps es in
        case filter (collidesWith p) es' of
          []   -> (p:ps', es') -- No collision, keep projectile and all enemies
          es'' -> (ps', es' \\ es'') -- Collision, remove projectile and all enemies that collided

      -- Decreases the player's cooldown and applies movement
      stepPlayer :: Player -> Player
      stepPlayer p = (applyMovement (subtractMargin $ windowSize gstate) p 10) { cooldown = max 0 $ cooldown p - 1 }

      -- Moves all enemies forward
      stepEnemies :: [Enemy] -> [Enemy]
      stepEnemies = let bounds = first (\w -> round $ fromIntegral w + (enemySize * 2)) $ windowSize gstate in
        filter (inBounds bounds . curPosition) . map stepEnemy
        where
          stepEnemy e = (applyMovement (first (\w -> round $ fromIntegral w + (enemySize * 2)) $ windowSize gstate) e 10)
            { enemyCooldown = max 0 $ enemyCooldown e - 1 }

      enemyFire :: [Enemy] -> IO ([Enemy], [Projectile])
      enemyFire [] = return ([], [])
      enemyFire (e:es) = do
        r <- randomIO :: IO Float -- Random value between 0 and 1
        if r < 0.01 && enemyCooldown e == 0
          then do
            let (x, y) = curPosition e
            let proj = createProjectile (x - (enemySize / 2) - 2.5, y) False -- Create a new projectile
            fps <- enemyFire es -- Attempt fire on rest of enemies
            return $ bimap (e {enemyCooldown = 10} :) (proj :) fps -- Set cooldown to 10 steps (0.5 seconds)
          else do
            (es', ps) <- enemyFire es
            return (e : es', ps)

      -- Will spawn an enemy if the last one was spawned long enough ago.
      -- Has a 5% chance of spawning an enemy every step after 4 seconds.
      trySpawnEnemy :: GameState -> IO GameState
      trySpawnEnemy gstateNew = do
        let e = enemies gstateNew

        r <- randomIO :: IO Float -- Random value between 0 and 1
        if elapsedTime gstateNew - lastSpawn gstate > 4 && (r < 0.05)
          then do
            let (wx, wy)    = windowSize gstate -- The window size
            let (hwx, hwy)  = (fromIntegral wx / 2, fromIntegral wy / 2)
            let maxY        = hwy - fromIntegral (snd margin) -- The maximum y value for an enemy to spawn at
            yd <- randomIO :: IO Float -- Random value between 0 and 1
            let (x, y) = (hwx + (enemySize / 2), maxY - (yd * maxY * 2)) -- y will be between -maxY and maxY
            let enemy = RegularEnemy (x, y) (x, y) (Movement True False False False R2L) 0

            debugM debugLog $ "Spawning enemy " ++ show enemy
            return $ gstateNew {
              lastSpawn = elapsedTime gstateNew,
              enemies = enemy : e
            }
          else return gstateNew

-- | Handle user input
input :: Event -> GameState -> IO GameState
-- Mouse events
input e@(EventKey (MouseButton _) Down _ _) gstate = inputMouse e gstate
-- Keyboard events (both special and chars)
input e@(EventKey {}) gstate = inputKey e gstate
-- Resize event
input (EventResize (nx, ny)) gstate = return gstate { windowSize = (nx, ny) }
-- Any other event
input _ gstate = return gstate

inputMouse :: Event -> GameState -> IO GameState
-- Mouse button pressed, doesn't do anything for now.
inputMouse (EventKey (MouseButton btn) Down _ (x, y)) gstate = do
  debugM debugLog $ "Mouse pressed " ++ unwords [show btn, show x, show y]
  return gstate
inputMouse _ gstate = return gstate

inputKey :: Event -> GameState -> IO GameState
-- Escape key, pauses the game.
inputKey (EventKey (SpecialKey KeyEsc) Down _ _) gstate = return $ gstate { paused = not $ paused gstate }
-- Space key, fires a projectile.
inputKey (EventKey (SpecialKey KeySpace) Down _ _) gstate = do
  if cooldown (player gstate) == 0
    then do
      let (px, py) = playerPos $ player gstate
      let proj = createProjectile (px + (playerSize / 2) + 2.5, py) True
      return $ gstate { projectiles = proj : projectiles gstate, player = (player gstate) { cooldown = 5 } }
    else do
      return gstate
-- Movement keys, move the player.
inputKey (EventKey (Char 'w') down _ _) gstate = return $ moveForward   gstate $ down == Down
inputKey (EventKey (Char 'a') down _ _) gstate = return $ moveLeft      gstate $ down == Down
inputKey (EventKey (Char 's') down _ _) gstate = return $ moveBackward  gstate $ down == Down
inputKey (EventKey (Char 'd') down _ _) gstate = return $ moveRight     gstate $ down == Down
inputKey (EventKey (SpecialKey KeyUp)    down _ _) gstate = return $ moveForward   gstate $ down == Down
inputKey (EventKey (SpecialKey KeyLeft)  down _ _) gstate = return $ moveLeft      gstate $ down == Down
inputKey (EventKey (SpecialKey KeyDown)  down _ _) gstate = return $ moveBackward  gstate $ down == Down
inputKey (EventKey (SpecialKey KeyRight) down _ _) gstate = return $ moveRight     gstate $ down == Down
inputKey _ gstate = return gstate

moveForward :: GameState -> Bool -> GameState
moveForward   gstate isDown = movePlayer gstate (\m -> m { forward = isDown })

moveBackward :: GameState -> Bool -> GameState
moveBackward  gstate isDown = movePlayer gstate (\m -> m { backward = isDown })

moveLeft :: GameState -> Bool -> GameState
moveLeft      gstate isDown = movePlayer gstate (\m -> m { left = isDown })

moveRight :: GameState -> Bool -> GameState
moveRight     gstate isDown = movePlayer gstate (\m -> m { right = isDown })

movePlayer :: GameState -> (Movement -> Movement) -> GameState
movePlayer gstate modifier = if paused gstate then gstate else
  gstate { player = (player gstate) { playerMovement = modifier $ playerMovement (player gstate) } }
