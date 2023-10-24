{-# OPTIONS_GHC -Wno-unused-local-binds #-}   -- We have some functions that are only used for debugging that we leave in.
-- | This module defines how to turn
--   the game state into a picture
module Game.OrionsOutlaws.View (module Game.OrionsOutlaws.View) where

import Graphics.Gloss
import Game.OrionsOutlaws.Model
import Game.OrionsOutlaws.Assets (fromPlayerFacing, pixeboyFont)
import Game.OrionsOutlaws.Util (msTime)
import Game.OrionsOutlaws.UI.Base (uiToPicture)
import Data.Bifunctor (Bifunctor(bimap))
import Game.OrionsOutlaws.Font (renderStringCentered, TextAlignment (..), renderString)

-- | Simply calls viewPure with the current stepdelta and the given gamestate
view :: GameState -> IO Picture
view gstate = do
    time <- msTime
    let sd = stepDelta (lastStep gstate) time
    return $ viewPure sd gstate

-- | Renders the gamestate into a picture
viewPure :: Float -> GameState -> Picture
viewPure sd gstate = pictures [
        renderPlayer $ player gstate,                                 -- render player
        -- renderPlayerBoxes $ player gstate,                         -- render player boxes (debugging only)
        renderProjectiles $ projectiles gstate,                       -- render projectiles
        renderEnemies $ enemies gstate,                               -- render enemies
        renderAnimations $ animations gstate,                         -- render animations
        renderScore (windowSize gstate) $ score gstate,               -- render score
        -- renderPauseOverlay,                                        -- render pause overlay
        maybe blank (uiToPicture (mousePos gstate)) $ activeUI gstate -- render active UI
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

        -- | Renders all animations currently playing.
        renderAnimations :: [PositionedAnimation] -> Picture
        renderAnimations = pictures . map renderAnimation
            where
                renderAnimation a = color orange $ translateP (animationPos a) $ frame $ animation a

        renderScore :: Bounds ->  Int -> Picture
        renderScore (w, h) s = let x = fromIntegral $ w `div` (-2) + 120
                                   y = fromIntegral $ h `div` (-2) + 45
                                   in translate x y $ scale 0.75 0.75 $ renderString RightToLeft pixeboyFont $ show s

        -- | Renders the pause overlay if the game is paused
        renderPauseOverlay :: Picture
        renderPauseOverlay
            | paused gstate = let s = fromIntegral (fst $ windowSize gstate) / 1280 in
                pictures [
                        -- Background
                        color (withAlpha 0.5 black) $ uncurry rectangleSolid $ bimap fromIntegral fromIntegral $ windowSize gstate,
                        scale s s $ translate 0 200 $ scale 2 2 $ renderStringCentered pixeboyFont "PAUSED"
                    ]
            | otherwise = blank

        -- | Returns the ship frame to use based on the current step
        getShipFrame :: ShipFrame
        getShipFrame = if steps gstate `mod` 4 < 2 then First else Second

-- | Turns an integer into a list of its digits.
digs :: Integral x => x -> [x]
digs 0 = [0]
digs x = digs' x
    where
        digs' 0 = []
        digs' y = digs' (y `div` 10) ++ [y `mod` 10]

-- | Translates a picture by a position 
translateP :: Position -> Picture -> Picture
translateP = uncurry translate

-- | Checks whether the given position is within the given bounds.
inBounds :: Bounds -> Position -> Bool
inBounds (maxX, maxY) (x, y) =
    let (hwx, hwy) = (fromIntegral maxX / 2, fromIntegral maxY / 2) in
    x > -hwx && x < hwx && y > -hwy && y < hwy

-- | Checks whether the given position is on the screen.
onScreen :: GameState -> Position -> Bool
onScreen gstate = inBounds $ windowSize gstate
