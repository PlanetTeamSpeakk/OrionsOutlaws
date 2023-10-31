-- | This module defines how the state changes in response to time and user input
module Game.OrionsOutlaws.Controller (module Game.OrionsOutlaws.Controller) where

import Game.OrionsOutlaws.Model
import Game.OrionsOutlaws.Rendering.View  (onScreen, inBounds)
import Game.OrionsOutlaws.Util.Util       (msTime, randomElem, distanceSq)
import Game.OrionsOutlaws.Assets          (explosionAnimation, laser1, laser2, explosion1, explosion2, missile1, missile2)
import Game.OrionsOutlaws.Util.Audio      (pauseAllSounds, playSound, resumeAllSounds)
import Game.OrionsOutlaws.Rendering.UI    (handleMouse, handleMotion)
import Game.OrionsOutlaws.UI.PausedUI     (pausedUI)
import Game.OrionsOutlaws.Util.Data       (writeScores)
import Graphics.Gloss.Interface.IO.Game   (KeyState(Down), Key(SpecialKey, MouseButton), MouseButton(LeftButton), SpecialKey(KeyEsc), Event(..))
import Graphics.Gloss.Geometry.Angle      (degToRad)
import System.Log.Logger                  (debugM)
import System.Random                      (randomIO)
import System.Exit                        (exitSuccess)
import Data.Bifunctor                     (first, bimap)
import Data.List                          ((\\)) -- List difference
import Data.Maybe                         (isJust, fromJust)
import Data.Time                          (getCurrentTime)

-- | Handle one iteration of the game
step :: Float -> GameState -> IO GameState
step elapsed gstate = do
  if paused gstate then return gstate -- Do nothing if paused
  else do
    -- Add elapsed time to the gamestate
    let gstateNew = gstate { elapsedTime = elapsedTime gstate + elapsed}

    -- Try to spawn an enemy
    gstateN2 <- trySpawnEnemy gstateNew

    -- Step animations
    let as = stepAnimations $ animations gstateN2

    -- Handle projectile collision with enemies. Filters projectiles and enemies.
    (fps, es, nas) <- handleProjectileCollision (filter (isFriendly . friendly) $ projectiles gstate) $ enemies gstateN2
    let hit = length $ enemies gstateN2 \\ es -- Number of enemies that were hit

    -- Filter out projectiles that collide with the player
    let nfps  = filter (not . isFriendly . friendly) $ projectiles gstate -- Non-friendly projectiles
    let cnfps = filter (collidesWith (player gstate)) nfps                -- Colliding non-friendly projectiles

    -- Check if any enemies collide with the player
    let eCollision = any (collidesWith (player gstate)) es

    if null cnfps && not eCollision
      then do -- No collision, return new gamestate
        (es', eps) <- enemyFire es

        time <- msTime
        return gstateN2
          { player      = stepPlayer $ player gstateN2
          , enemies     = stepEnemies es' -- Step left-over enemies
          -- Friendly projectiles, hostile projectiles that didn't collide with the player, and new hostile projectiles
          , projectiles = stepProjectiles $ fps ++ (nfps \\ cnfps) ++ eps -- Step projectiles
          , animations  = as ++ nas
          , score       = score gstateN2 + hit
          , lastStep    = time -- To lerp positions when rendering
          , steps       = steps gstateN2 + 1
          }
      else -- Collision! Exit game if the player has 1 health left, otherwise reset and subtract 1 health
        if health (player gstateN2) == 1
          then onGameOver gstateNew
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
    stepProjectiles = map stepProjectile . filter (onScreen gstate . curPosition)
      where
        stepProjectile p@(RegularProjectile {}) = applyMovement (windowSize gstate) p $ speed p * projectileSpeed
        stepProjectile p@(MissileProjectile { projPos = (x, y), mslRotation = r }) =
          let dr     = degToRad r
              (h, v) = normalizeMotion (sin dr, cos dr)
              m      = speed p * projectileSpeed
          in p
            { projPos = (x + v * m , y + h * m)
            , prevProjPos = projPos p
            }

    -- Handles projectile collision. For each projectile, check if it collides with any enemy.
    -- If it does, remove the projectile and the enemy it collided with.
    handleProjectileCollision :: [Projectile] -> [Enemy] -> IO ([Projectile], [Enemy], [PositionedAnimation])
    handleProjectileCollision [] es     = return ([], es, []) -- No projectiles, no collisions
    handleProjectileCollision (p:ps) es = do
      (ps', es', as) <- handleProjectileCollision ps es
      case getCollisions p es' of
        Nothing   -> return (p:ps', es', as) -- No collision, keep projectile and all enemies
        -- Collision, remove projectile and all enemies that collided. Add an explosion animation for each collided enemy.
        -- It is also possible es'' is empty. I.e. a missile has reached its target, but didn't hit anything.
        Just es'' -> do
          playExplosionSound

          let nas = case es'' of
                [] -> [PositionedAnimation explosionAnimation (projPos p)] -- No enemies, add an explosion at the projectile's position
                _  -> map (PositionedAnimation explosionAnimation . curPosition) es'' -- Add an explosion at each enemy's position
          return (ps', es' \\ es'', nas ++ as)
      where
        getCollisions :: Projectile -> [Enemy] -> Maybe [Enemy]
        -- For regular projectiles, check if the projectile collides with any enemy
        getCollisions p'@(RegularProjectile {}) es' = case getColliding p' es' of
          []   -> Nothing   -- No collision
          es'' -> Just es'' -- Collision

        -- For missiles, if the missile hit anything, it explodes and damages enemies within a radius of 40 pixels.
        -- If it doesn't hit anything, it will explode once it reaches its target.
        getCollisions p'@(MissileProjectile {}) es' =
          let c  = getColliding p' es' -- Colliding enemies
              d  = distanceSq (mslStartPos p') (projPos p')   -- Distance between start and current position
              td = distanceSq (mslStartPos p') (mslTarget p') -- Distance between start and target
          in if d >= td || not (null c)
            then         -- Either the missile collided or it has reached its target
              let b = growBox (head $ createBoxes p') 69 69 -- The missile's box, 80x80 centered around its head
              in Just $ filter (`collidesWithBox` b) es'    -- Enemies within a radius of 40 pixels around the missile's head
            else Nothing -- No collision, but the missile hasn't reached its target yet

        getColliding :: Projectile -> [Enemy] -> [Enemy]
        getColliding p' = filter (collidesWith p')

    -- Decreases the player's cooldown and applies movement
    stepPlayer :: Player -> Player
    stepPlayer p = (applyMovement (subtractMargin $ windowSize gstate) p 10) 
      { cooldown    = max 0 $ cooldown p - 1 
      , mslCooldown = max 0 $ mslCooldown p - 1
      }

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

    -- Attempts to fire a projectile from each enemy. If an enemy fires, it will have a cooldown of 15 steps (0.5 seconds)
    enemyFire :: [Enemy] -> IO ([Enemy], [Projectile])
    enemyFire [] = return ([], [])
    enemyFire (e:es) = do
      r <- randomIO :: IO Float -- Random value between 0 and 1
      if r < 0.01 && enemyCooldown e == 0
        then do
          let (x, y) = curPosition e
          let proj = createProjectile (x - (enemySize / 2) - 2.5, y) Hostile -- Create a new projectile
          fps <- enemyFire es                                                -- Attempt fire on rest of enemies
          return $ bimap (e {enemyCooldown = 15} :) (proj :) fps             -- Set cooldown to 15 steps (0.5 seconds)
        else do
          (es', ps) <- enemyFire es
          return (e : es', ps)

    -- Will spawn an enemy if the last one was spawned long enough ago.
    -- Has an 2% chance of spawning an enemy every step after 2 seconds.
    -- This means that on average, an enemy will spawn every
    -- 2 + 1 / 0.02 / 30 = 2 + 1 / 0.6 = 2 + 1.67 = 3.67 seconds.
    trySpawnEnemy :: GameState -> IO GameState
    trySpawnEnemy gstateNew = do
      let e  = enemies gstateNew
      let sr = spawnRate $ settings gstateNew

      r <- randomIO :: IO Float -- Random value between 0 and 1
      if elapsedTime gstateNew - lastSpawn gstate > (2 / sr) && (r < (0.02 * sr))
        then do
          let (wx, wy)    = windowSize gstate -- The window size
          let (hwx, hwy)  = (fromIntegral wx / 2, fromIntegral wy / 2)
          let maxY        = hwy - fromIntegral (snd margin) -- The maximum y value for an enemy to spawn at
          yd <- randomIO :: IO Float -- Random value between 0 and 1
          let (x, y) = (hwx + (enemySize / 2), maxY - (yd * maxY * 2)) -- y will be between -maxY and maxY
          let enemy = RegularEnemy (x, y) (x, y) (Movement True False False False R2L $ elapsedTime gstateNew) 0

          debugM debugLog $ "Spawning enemy " ++ show enemy
          return $ gstateNew
            { lastSpawn = elapsedTime gstateNew
            , enemies = enemy : e
            }
        else return gstateNew

    onGameOver :: GameState -> IO GameState
    onGameOver gstateNew = do
      t <- getCurrentTime                                            -- Current time in UTCTime
      let s' = Score "Player" (score gstateNew) t : scores gstateNew -- Add the current score to the list of scores
      writeScores s'                                                 -- Write the scores to scores.json
      exitSuccess                                                    -- Exit the game, temporary until we have a UI.
      --return gstateNew { scores = s' }                               -- Return the new gamestate

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

-- | Stage 2 of input handling. Handles key listeners and configurable keys.
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

-- | Stage 3 of input handling. Handles mouse move and window resize.
input'' :: Event -> GameState -> IO GameState
-- Mouse move event
input'' e@(EventMotion {}) gstate = inputMouseMove e gstate
-- Resize event
input'' (EventResize ns) gstate = return gstate { windowSize = ns }
-- Fallback
input'' _ gstate = return gstate

-- | Handle mouse input. Propagates to UI if there's an active UI.
inputMouse :: Event -> GameState -> IO GameState
-- Mouse button pressed, handle UI click.
inputMouse (EventKey (MouseButton btn) state _ (x, y)) gstate = do
  let (ww, wh) = windowSize gstate
  let s = (fromIntegral ww / 1280, fromIntegral wh / 720)
  if btn == LeftButton && isJust (activeUI gstate)
    then do
      (_, ui') <- handleMouse (fromJust $ activeUI gstate) state (x, y) s
      return gstate { activeUI = Just ui' }
    else do
      -- No UI active, fire missile
      if cooldown (player gstate) == 0
        then fireMissile gstate (x, y)
        else return gstate
inputMouse _ gstate = return gstate

-- | Handle mouse motion. Propagates to UI if there's an active UI.
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

-- | Handle pause input
--
--   Only does something if the given event is the escape key being down.
inputPause :: Event -> GameState -> IO (Bool, GameState) -- ^ (Consumed, new GameState)
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

-- | Fires a projectile if the player is not on cooldown.
fireProjectile :: GameState -> IO GameState
fireProjectile gstate = do
  if cooldown (player gstate) == 0 && not (paused gstate)
    then do
      let (px, py) = playerPos $ player gstate
      let proj = createProjectile (px + (24 * assetScale / 2) + 2.5, py) Friendly
      playLaserSound
      return $ gstate { projectiles = proj : projectiles gstate, player = (player gstate) { cooldown = 8 } }
    else do
      return gstate

-- | Fires a missile projectile if the player is not on cooldown.
fireMissile :: GameState -> Position -> IO GameState
fireMissile gstate target = do
  if cooldown (player gstate) == 0 && mslCooldown (player gstate) == 0 && not (paused gstate)
    then do
      let (px, py) = playerPos $ player gstate
      let proj = createMissile (px + (24 * assetScale / 2) + (36 * 0.3), py) target Friendly
      playMissileSound
      return $ gstate { projectiles = proj : projectiles gstate, player = (player gstate) 
        { cooldown    = 8 
        , mslCooldown = 120 -- One missile every 4 seconds
        } }
    else do
      return gstate

-- | Sets the player's forward movement.
moveForward  :: GameState -> Bool -> GameState
moveForward  gstate isDown = movePlayer gstate (\m -> m { forward = isDown })

-- | Sets the player's backward movement.
moveBackward :: GameState -> Bool -> GameState
moveBackward gstate isDown = movePlayer gstate (\m -> m { backward = isDown })

-- | Sets the player's left movement.
moveLeft     :: GameState -> Bool -> GameState
moveLeft     gstate isDown = movePlayer gstate (\m -> m { left = isDown })

-- | Sets the player's right movement.
moveRight    :: GameState -> Bool -> GameState
moveRight    gstate isDown = movePlayer gstate (\m -> m { right = isDown })

-- | Sets the player's movement. Does not directly modify the position, that's done in the next step.
movePlayer :: GameState -> (Movement -> Movement) -> GameState
movePlayer gstate modifier = if paused gstate then gstate else
  gstate { player = (player gstate) { playerMovement = (modifier $ playerMovement (player gstate)) { lastChange = elapsedTime gstate } } }

-- | Plays a random laser sound
playLaserSound :: IO ()
playLaserSound = randomElem [laser1, laser2] >>= playSound

-- | Plays a random missile sound
playMissileSound :: IO ()
playMissileSound = randomElem [missile1, missile2] >>= playSound

-- | Plays a random explosion sound
playExplosionSound :: IO ()
playExplosionSound = randomElem [explosion1, explosion2] >>= playSound
