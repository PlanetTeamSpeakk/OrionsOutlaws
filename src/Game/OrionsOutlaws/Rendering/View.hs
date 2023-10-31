-- | This module defines how to turn the game state into a picture
module Game.OrionsOutlaws.Rendering.View (module Game.OrionsOutlaws.Rendering.View) where

import Graphics.Gloss (Picture, green, orange, red, blank, color, pictures, rectangleWire, rotate, scale, translate)
import Game.OrionsOutlaws.Model
import Game.OrionsOutlaws.Assets (fromPlayerFacing, pixeboyFont, shadows, stars, bigStars, blueStar, redStar, blackHole, smallRotaryStar, rotaryStar, missile, fighter, bullet, enemyProjectile)
import Game.OrionsOutlaws.Util.Util (msTime, lerp)
import Game.OrionsOutlaws.Rendering.UI (renderUI)
import Game.OrionsOutlaws.Rendering.Font (TextAlignment (..), renderString)

-- | Renders the gamestate into a picture
--   Simply calls viewPure with the current stepdelta and the given gamestate
view :: GameState -> IO Picture
view gstate = do
  time <- msTime
  let sd = min 1 $ stepDelta (lastStep gstate) time
  return $ viewPure sd gstate

-- | Renders the gamestate into a picture
viewPure :: Float -> GameState -> Picture
viewPure sd gstate@GameState { windowSize = (ww, wh) } = let (hs, vs) = (fromIntegral ww / 1280, fromIntegral wh / 720) in
  pictures
    [ renderBackground                                                    -- render background                                                    
    , renderPlayer $ player gstate                                        -- render player
    , ifDebug $ renderBoxesFor [player gstate]                            -- render player boxes (debugging only)
    , renderProjectiles $ projectiles gstate                              -- render projectiles
    , ifDebug $ renderBoxesFor $ projectiles gstate                       -- render projectile boxes (debugging only)
    , renderEnemies $ enemies gstate                                      -- render enemies
    , ifDebug $ renderBoxesFor $ enemies gstate                           -- render enemy boxes (debugging only)
    , renderAnimations $ animations gstate                                -- render animations
    , renderScore (windowSize gstate) $ score gstate                      -- render score
    , maybe blank (renderUI (mousePos gstate) (hs, vs)) $ activeUI gstate -- render active UI (if any)
    ]
  where
    -- | Returns the given picture if debug-mode is enabled, otherwise returns 'blank'.
    ifDebug :: Picture -> Picture
    ifDebug p = if debug gstate then p else blank

    -- | Renders the background and all its components.
    renderBackground :: Picture
    renderBackground = pictures
      [ renderShadows                       -- render shadows
      , renderAnimatedLayer stars           -- render animated stars
      , renderAnimatedLayer bigStars        -- render animated big stars
      , renderAnimatedLayer blueStar        -- render animated blue star
      , renderAnimatedLayer redStar         -- render animated red star
      , renderAnimatedLayer blackHole       -- render animated black hole
      , renderAnimatedLayer smallRotaryStar -- render animated small rotary star
      , renderAnimatedLayer rotaryStar      -- render animated rotary star
      ]

    -- | Renders the background shadows.
    renderShadows :: Picture
    renderShadows = translateP (-fromInteger (steps gstate `mod` 256) * 10, 0) $ scale factor factor shadows
      where factor = fromIntegral wh / 360

    -- | Renders an animated layer of the background.
    renderAnimatedLayer :: (Int -> Picture) -> Picture
    renderAnimatedLayer layer = scale factor factor $ layer f
      where factor = fromIntegral wh / 360
            f = fromIntegral $ steps gstate `mod` 16 `div` 2

    -- | Renders the player.
    renderPlayer :: Player -> Picture
    renderPlayer p = color green $ translateP (position sd p) $ fromPlayerFacing (facing gstate (movement $ player gstate)) getShipFrame

    -- | Renders the boxes for the given collidables.
    renderBoxesFor :: (Collidable a, Positionable a) => [a] -> Picture
    renderBoxesFor cs = pictures $ map (\c -> renderBoxes (createBoxes c) (createBoxesAt (prevPosition c) c)) cs

    -- | Renders a list of boxes into rectangles picture.
    --   Used for debugging purposes.
    renderBoxes :: [Box] -> [Box] -> Picture
    renderBoxes bs obs = pictures $ zipWith renderBox bs obs

    -- | Renders a single box into a rectangle picture.
    renderBox :: Box -> Box -> Picture
    renderBox ((minXN, minYN), (maxXN, maxYN)) ((minXO, minYO), (maxXO, maxYO)) = 
      let minX = lerp minXO minXN sd
          minY = lerp minYO minYN sd
          maxX = lerp maxXO maxXN sd
          maxY = lerp maxYO maxYN sd
      in color red $ translateP ((minX + maxX) / 2, (minY + maxY) / 2) $ rectangleWire (maxX - minX) (maxY - minY)

    -- | Renders the projectiles, currently just red/cyan circles
    renderProjectiles :: [Projectile] -> Picture
    renderProjectiles = pictures . map renderProjectile
      where
        renderProjectile p@(RegularProjectile {}) =
          let s = if isFriendly $ friendly p then bullet else enemyProjectile
              r = if isFriendly $ friendly p then 90 else -90
          in translateP (position sd p) $ rotate r $ s $ fromInteger $ steps gstate
        renderProjectile p@(MissileProjectile { mslRotation = r }) =
          let s  = missile $ fromInteger $ steps gstate
              dr = if isFriendly $ friendly p then 90 else -90
          in translateP (position sd p) $ rotate (-r + dr) s

    -- | Renders the enemies
    renderEnemies :: [Enemy] -> Picture
    renderEnemies = pictures . map renderEnemy
      where
        renderEnemy e@(RegularEnemy {}) = translateP (position sd e) $ rotate (-90) fighter

    -- | Renders all animations currently playing.
    renderAnimations :: [PositionedAnimation] -> Picture
    renderAnimations = pictures . map renderAnimation
      where
        renderAnimation a = color orange $ translateP (animationPos a) $ frame $ animation a

    -- | Renders the player's score.
    renderScore :: Bounds ->  Int -> Picture
    renderScore (w, h) s = let x = fromIntegral $ w `div` (-2) + 120
                               y = fromIntegral $ h `div` (-2) + 45
                               in translate x y $ scale 0.75 0.75 $ renderString RightToLeft pixeboyFont $ show s

    -- | Returns the ship frame to use based on the current step
    getShipFrame :: ShipFrame
    getShipFrame = if steps gstate `mod` 4 < 2 then First else Second

-- | Translates a picture by a position 
translateP :: Position -> Picture -> Picture
translateP = uncurry translate

-- | Checks whether the given position is within the given bounds.
inBounds :: Bounds -> Position -> Bool
inBounds (maxX, maxY) (x, y) = let (hwx, hwy) = (fromIntegral maxX / 2, fromIntegral maxY / 2) in
  x > -hwx && x < hwx && y > -hwy && y < hwy

-- | Checks whether the given position is on the screen.
onScreen :: GameState -> Position -> Bool
onScreen gstate = inBounds $ windowSize gstate
