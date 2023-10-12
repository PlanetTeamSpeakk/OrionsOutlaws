-- | This module defines how to turn
--   the game state into a picture
module View where

import Graphics.Gloss
import Model
import Util (msTime)

view :: GameState -> IO Picture
view gstate = do
    time <- msTime
    let sd = stepDelta (lastStep gstate) time
    return $ viewPure sd gstate

viewPure :: Float -> GameState -> Picture
viewPure sd gstate = pictures [renderPlayer $ player gstate, renderProjectiles $ projectiles gstate]
    where
        renderPlayer :: Player -> Picture
        renderPlayer p = color green $ translateP (position sd p) $ circleSolid (playerSize / 2)

        renderProjectiles :: [Projectile] -> Picture
        renderProjectiles = pictures . map renderProjectile
            where
                renderProjectile p = color (if friendly p then cyan else red) $ 
                    translateP (position sd p) $ circleSolid 5

translateP :: Position -> Picture -> Picture
translateP (x, y) = translate x y

onScreen :: GameState -> Position -> Bool
onScreen gstate (x, y) = 
    let (wx, wy)   = windowSize gstate
        (hwx, hwy) = (fromIntegral wx / 2, fromIntegral wy / 2) in
    x > -hwx && x < hwx && y > -hwy && y < hwy
