{-# OPTIONS_GHC -Wno-missing-export-lists #-} -- I do not care, just export everything.
-- | This module defines how to turn
--   the game state into a picture
module View where

import Graphics.Gloss
import Model
import Assets
import Util (msTime)
import Data.Bifunctor (Bifunctor(bimap))

-- | Simply calls viewPure with the current stepdelta and the given gamestate
view :: GameState -> IO Picture
view gstate = do
    time <- msTime
    let sd = stepDelta (lastStep gstate) time
    return $ viewPure sd gstate

-- | Renders the gamestate into a picture
viewPure :: Float -> GameState -> Picture
viewPure sd gstate = pictures [
        renderPlayer $ player gstate,           -- render player
        renderPlayerBoxes $ player gstate,      -- render player boxes
        renderProjectiles $ projectiles gstate, -- render projectiles
        renderEnemies $ enemies gstate,         -- render enemies
        renderAnimations $ animations gstate,   -- render animations
        renderPauseOverlay                      -- render pause overlay
    ]
    where
        -- Renders the player, curently just a green circle
        renderPlayer :: Player -> Picture
        renderPlayer p = color green $ translateP (position sd p) $ fromPlayerFacing (facing gstate (movement $ player gstate)) getShipFrame

        renderPlayerBoxes :: Player -> Picture
        renderPlayerBoxes = renderBoxes . createBoxes
        
        renderBoxes :: [Box] -> Picture
        renderBoxes bs = pictures $ map renderBox bs
            where
                renderBox ((minX, minY), (maxX, maxY)) = color red $ translateP ((minX + maxX) / 2, (minY + maxY) / 2) $ 
                    rectangleWire (maxX - minX) (maxY - minY)

        -- Renders the projectiles, currently just red circles
        renderProjectiles :: [Projectile] -> Picture
        renderProjectiles = pictures . map renderProjectile
            where
                renderProjectile p = color (if friendly p then cyan else red) $
                    translateP (position sd p) $ circleSolid 5

        renderEnemies :: [Enemy] -> Picture
        renderEnemies = pictures . map renderEnemy
            where
                renderEnemy e = color orange $ translateP (position sd e) $ circleSolid (enemySize / 2)

        renderAnimations :: [PositionedAnimation] -> Picture
        renderAnimations = pictures . map renderAnimation
            where
                renderAnimation a = color orange $ translateP (animationPos a) $ frame $ animation a

        renderPauseOverlay :: Picture
        renderPauseOverlay
            | paused gstate = let s = fromIntegral (fst $ windowSize gstate) / 1280 in 
                pictures [
                        -- Background
                        color (withAlpha 0.5 black) $ uncurry rectangleSolid $ bimap fromIntegral fromIntegral $ windowSize gstate,
                        scale s s pauseOverlay -- Text
                    ]
            | otherwise = blank
        
        getShipFrame :: ShipFrame
        getShipFrame = if steps gstate `mod` 4 < 2 then First else Second

translateP :: Position -> Picture -> Picture
translateP (x, y) = translate x y

inBounds :: Bounds -> Position -> Bool
inBounds (maxX, maxY) (x, y) =
    let (hwx, hwy) = (fromIntegral maxX / 2, fromIntegral maxY / 2) in
    x > -hwx && x < hwx && y > -hwy && y < hwy

onScreen :: GameState -> Position -> Bool
onScreen gstate = inBounds $ windowSize gstate
