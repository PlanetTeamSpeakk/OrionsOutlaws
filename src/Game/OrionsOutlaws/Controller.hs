-- | This module defines how the state changes
--   in response to time and user input
module Game.OrionsOutlaws.Controller (module Game.OrionsOutlaws.Controller) where

import Game.OrionsOutlaws.Model
import Graphics.Gloss.Interface.IO.Game
import System.Log.Logger (debugM)
import Game.OrionsOutlaws.Rendering.View (onScreen, inBounds)
import Game.OrionsOutlaws.Util.Util (msTime, randomElem)
import System.Random (randomIO)
import Data.Bifunctor (first, bimap)
import Data.List ((\\)) -- List difference
import System.Exit (exitSuccess)
import Game.OrionsOutlaws.Assets (explosionAnimation, laser1, laser2, explosion1, explosion2, assetScale)
import Game.OrionsOutlaws.Util.Audio
import Data.Maybe (isJust, fromJust)
import Game.OrionsOutlaws.Rendering.UI (handleMouse, handleMotion)
import Game.OrionsOutlaws.UI.PausedUI (pausedUI)

-- | Handle one iteration of the game
step :: Float -> GameState -> IO GameState
step elapsed gstate = do
  if paused gstate then return gstate -- Do nothing if paused
  else do
    -- Add elapsed time to the gamestate
    let gstateNew = gstate { elapsedTime = elapsedTime gstate + elapsed}

    -- Try to spawn an enemy
    gstateN2 <- trySpawnEnemy gstateNew

    -- Step projectiles
    let p = stepProjectiles $ projectiles gstateN2

    -- Step animations
    let as = stepAnimations $ animations gstateN2

    -- Handle projectile collision with enemies. Filters projectiles and enemies.
    (fps, es, nas) <- handleProjectileCollision (filter friendly p) $ enemies gstateN2
    let hit = length $ enemies gstateN2 \\ es -- Number of enemies that were hit

    -- Filter out projectiles that collide with the player
    let nfps  = filter (not . friendly) p                   -- Non-friendly projectiles
    let cnfps = filter (collidesWith (player gstate)) nfps  -- Colliding non-friendly projectiles

    -- Check if any enemies collide with the player
    let eCollision = any (collidesWith (player gstate)) es

    if null cnfps && not eCollision
      then do -- No collision, return new gamestate
        ep <- enemyFire es

        time <- msTime
        return gstateN2 
          { player      = stepPlayer $ player gstateN2 
          , enemies     = stepEnemies $ fst ep -- Step left-over enemies
          -- Friendly projectiles, non-friendly projectiles that didn't collide with the player, and new enemy projectiles
          , projectiles = fps ++ (nfps \\ cnfps) ++ snd ep
          , animations  = as ++ nas
          , score       = score gstateN2 + hit
          , lastStep    = time
          , steps       = steps gstateN2 + 1
          }
      else -- Collision! Exit game if the player has 1 health left, otherwise reset and subtract 1 health
        if health (player gstateN2) == 1
          then do
            _ <- exitSuccess
            return gstateN2
          else do
            debugM debugLog $ "Collision! " ++ show cnfps
            return gstateN2 
              { player      = initialPlayer { health = health (player gstateN2) - 1 }
              , enemies     = [] 
              , projectiles = []
              , animations  = []
              }
  where
    -- Moves all projectiles forward
    stepProjectiles :: [Projectile] -> [Projectile]
    stepProjectiles = filter (onScreen gstate . curPosition) . map stepProjectile
      where
        stepProjectile p = applyMovement (windowSize gstate) p $ speed p * projectileSpeed

    -- Handles projectile collision. For each projectile, check if it collides with any enemy.
    -- If it does, remove the projectile and the enemy it collided with.
    handleProjectileCollision :: [Projectile] -> [Enemy] -> IO ([Projectile], [Enemy], [PositionedAnimation])
    handleProjectileCollision ps [] = return (ps, [], []) -- No enemies, no collision
    handleProjectileCollision [] es = return ([], es, []) -- No projectiles, no collision
    handleProjectileCollision (p:ps) es = do
      (ps', es', as) <- handleProjectileCollision ps es
      case filter (collidesWith p) es' of
        []   -> return (p:ps', es', as) -- No collision, keep projectile and all enemies
        -- Collision, remove projectile and all enemies that collided. Add an explosion animation for each collided enemy
        es'' -> do
          playExplosionSound
          return (ps', es' \\ es'', map (PositionedAnimation explosionAnimation . curPosition) es'')

    -- Decreases the player's cooldown and applies movement
    stepPlayer :: Player -> Player
    stepPlayer p = (applyMovement (subtractMargin $ windowSize gstate) p 10) { cooldown = max 0 $ cooldown p - 1 }

    stepAnimations :: [PositionedAnimation] -> [PositionedAnimation]
    stepAnimations = filter isOnGoing . map stepAnimation
      where
        stepAnimation (PositionedAnimation a p) = PositionedAnimation (a {
            animationStep = animationStep a + 1,
            curFrame = curFrame a + (if animationStep a `mod` frameDuration a == 0 && (animationStep a /= 0) then 1 else 0)
          }) p
        isOnGoing (PositionedAnimation a _) = animationStep a < (frameDuration a * frameCount a)

    -- Moves all enemies forward
    stepEnemies :: [Enemy] -> [Enemy]
    stepEnemies = let bounds = first (\w -> round $ fromIntegral w + (enemySize * 2)) $ windowSize gstate in
      filter (inBounds bounds . curPosition) . map stepEnemy
      where
        stepEnemy e = (applyMovement (first (\w -> round $ fromIntegral w + (enemySize * 2)) $ windowSize gstate) e 10)
          { enemyCooldown = max 0 $ enemyCooldown e - 1 }

    -- Attempts to fire a projectile from each enemy. If an enemy fires, it will have a cooldown of 10 steps (0.5 seconds)
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
          let enemy = RegularEnemy (x, y) (x, y) (Movement True False False False R2L $ elapsedTime gstateNew) 0

          debugM debugLog $ "Spawning enemy " ++ show enemy
          return $ gstateNew {
            lastSpawn = elapsedTime gstateNew,
            enemies = enemy : e
          }
        else return gstateNew

-- | Handle user input
input :: Event -> GameState -> IO GameState
input e gstate = do
  -- Prioritize pause input
  -- This makes it impossible to bind the escape key to anything,
  -- even when you modify settings.json manually.
  (consumed, gstate') <- inputPause e gstate
  if consumed
    then return gstate' -- Don't handle any other input if inputPause consumed the event
    else do
      -- Ensure mouse input is always handled.
      -- This is so binding anything to LeftButton doesn't break UI buttons.
      gstate'' <- inputMouse e gstate'
      input' e gstate'' -- Continue to next stage (key listeners and configurable keys)

input' :: Event -> GameState -> IO GameState
input' e@(EventKey key down _ _) gstate
  -- If there's a keylistener, call it and remove it from the list
  | key /= SpecialKey KeyEsc && down == Down && not (null $ keyListeners gstate) = do
    let keyListener = head $ keyListeners gstate
    keyListener key
    return gstate { keyListeners = tail $ keyListeners gstate }
  -- Handle configurable keys (fire and movement)
  | d && key == fireKey s = fireProjectile gstate
  | key == forwardKey   s = return $ moveForward  gstate d
  | key == backwardKey  s = return $ moveBackward gstate d
  | key == leftKey      s = return $ moveLeft     gstate d
  | key == rightKey     s = return $ moveRight    gstate $ down == Down
  | otherwise             = input'' e gstate -- Continue to last stage (mouse move and window resize)
    where
      d = down == Down
      s = settings gstate
input' e gstate = input'' e gstate -- Continue to last stage

input'' :: Event -> GameState -> IO GameState
-- Mouse move event
input'' e@(EventMotion {}) gstate = inputMouseMove e gstate
-- Resize event
input'' (EventResize ns) gstate = return gstate { windowSize = ns }
-- Fallback
input'' _ gstate = return gstate

inputMouse :: Event -> GameState -> IO GameState
-- Mouse button pressed, handle UI click.
inputMouse (EventKey (MouseButton btn) state _ (x, y)) gstate = do
  let (ww, wh) = windowSize gstate
  let s = (fromIntegral ww / 1280, fromIntegral wh / 720)
  if btn == LeftButton && isJust (activeUI gstate)
    then do
      ui' <- handleMouse (fromJust $ activeUI gstate) state (x, y) s
      return gstate { activeUI = Just ui' }
    else return gstate
inputMouse _ gstate = return gstate

-- Mouse moved, update mouse position (used for UI).
inputMouseMove :: Event -> GameState -> IO GameState
inputMouseMove (EventMotion mPos) gstate = do
  let (ww, wh) = windowSize gstate
  let s = (fromIntegral ww / 1280, fromIntegral wh / 720)
  
  -- Handle UI motion if there's an active UI
  ui' <- case activeUI gstate of
    Just ui -> Just <$> handleMotion ui mPos s
    Nothing -> return Nothing

  return $ gstate 
    { mousePos = Just mPos 
    , activeUI = ui'
    }
inputMouseMove _ gstate = return gstate

inputPause :: Event -> GameState -> IO (Bool, GameState)
-- Escape key, pauses the game.
inputPause (EventKey (SpecialKey KeyEsc) Down _ _) gstate = do
  debugM debugLog $ if paused gstate then "Unpausing" else "Pausing"

  -- Pause/resume all sounds
  if paused gstate
    then resumeAllSounds
    else pauseAllSounds

  let paused' = paused gstate
  return (True, gstate
    { activeUI = if paused' then Nothing else Just pausedUI
    , player = if paused' then player gstate else
        (player gstate) { playerMovement = emptyMovement L2R } -- Clear movement
    , keyListeners = [] -- Clear key listeners
    })
-- Fallback
inputPause _ gstate = return (False, gstate)

fireProjectile :: GameState -> IO GameState
fireProjectile gstate = do
  if cooldown (player gstate) == 0 && not (paused gstate)
    then do
      let (px, py) = playerPos $ player gstate
      let proj = createProjectile (px + (24 * assetScale / 2) + 2.5, py) True
      playLaserSound
      return $ gstate { projectiles = proj : projectiles gstate, player = (player gstate) { cooldown = 5 } }
    else do
      return gstate

moveForward  :: GameState -> Bool -> GameState
moveForward  gstate isDown = movePlayer gstate (\m -> m { forward = isDown })

moveBackward :: GameState -> Bool -> GameState
moveBackward gstate isDown = movePlayer gstate (\m -> m { backward = isDown })

moveLeft     :: GameState -> Bool -> GameState
moveLeft     gstate isDown = movePlayer gstate (\m -> m { left = isDown })

moveRight    :: GameState -> Bool -> GameState
moveRight    gstate isDown = movePlayer gstate (\m -> m { right = isDown })

-- | Sets the player's movement. Does not directly modify the position, that's done in the next step.
movePlayer :: GameState -> (Movement -> Movement) -> GameState
movePlayer gstate modifier = if paused gstate then gstate else
  gstate { player = (player gstate) { playerMovement = (modifier $ playerMovement (player gstate)) { lastChange = elapsedTime gstate } } }

playLaserSound :: IO ()
playLaserSound = do
  sound <- randomElem [laser1, laser2]
  playSound sound

playExplosionSound :: IO ()
playExplosionSound = do
  sound <- randomElem [explosion1, explosion2]
  playSound sound
