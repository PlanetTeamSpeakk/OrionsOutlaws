-- | This module defines how to turn
--   the game state into a picture
module View where

import Graphics.Gloss
import Model
import Util (lerp, msTime)

view :: GameState -> IO Picture
view gstate = do
    time <- msTime
    return $ viewPure time gstate

viewPure :: Integer -> GameState -> Picture
viewPure time gstate = pictures [renderPlayer $ player gstate, renderProjectiles $ projectiles gstate]
    where
        renderPlayer :: Player -> Picture
        renderPlayer (Player p _ _) = color green $ translateP p $ circleSolid (playerSize / 2)

        renderProjectiles :: [Projectile] -> Picture
        renderProjectiles = pictures . map renderProjectile
            where
                renderProjectile p = color (if friendly p then cyan else red) $ 
                    translateP (lerpPos (prevProjPos p) (projPos p) (stepDelta (lastStep gstate) time)) $ circleSolid 5

translateP :: Position -> Picture -> Picture
translateP (x, y) = translate x y

onScreen :: GameState -> Position -> Bool
onScreen gstate (x, y) = 
    let (wx, wy)   = windowSize gstate
        (hwx, hwy) = (fromIntegral wx / 2, fromIntegral wy / 2) in
    x > -hwx && x < hwx && y > -hwy && y < hwy

lerpPos :: Position -> Position -> Float -> Position
lerpPos (x1, y1) (x2, y2) t = (lerp x1 x2 t, lerp y1 y2 t)
