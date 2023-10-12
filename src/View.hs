-- | This module defines how to turn
--   the game state into a picture
module View where

import Graphics.Gloss
import Model

view :: GameState -> IO Picture
view = return . viewPure

viewPure :: GameState -> Picture
viewPure gstate = pictures [renderPlayer $ player gstate, renderProjectiles $ projectiles gstate]

renderPlayer :: Player -> Picture
renderPlayer (Player p _ _) = color green $ translateP p $ circleSolid (playerSize / 2)

renderProjectiles :: [Projectile] -> Picture
renderProjectiles = pictures . map renderProjectile
    where
        renderProjectile p = color (if friendly p then cyan else red) $ translateP (projPos p) $ circleSolid 5

translateP :: Position -> Picture -> Picture
translateP (x, y) = translate x y

onScreen :: GameState -> Position -> Bool
onScreen gstate (x, y) = 
    let (wx, wy)   = windowSize gstate
        (hwx, hwy) = (fromIntegral wx / 2, fromIntegral wy / 2) in
    x > -hwx && x < hwx && y > -hwy && y < hwy
