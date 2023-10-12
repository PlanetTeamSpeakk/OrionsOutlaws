-- | This module defines how the state changes
--   in response to time and user input
module Controller where

import Model
import Util
import Graphics.Gloss.Interface.IO.Game
import Control.Monad (unless)
import System.Log.Logger (debugM)
import Graphics.UI.GLUT.Window
import Graphics.UI.GLUT (Size (Size), ($=))

-- | Handle one iteration of the game
step :: Float -> GameState -> IO GameState
step secs gstate = do
  let (wx, wy) = getWindowSizeFor gstate
  windowSize $= Size (fromIntegral wx) (fromIntegral wy)
  -- Increment time if the game has started
  return gstate { elapsedTime = if started gstate then elapsedTime gstate + secs else 0 }

-- | Handle user input
input :: Event -> GameState -> IO GameState
-- Mouse events
input e@(EventKey (MouseButton _) Down _ _) gstate = do
  ret <- inputMouse e gstate
  unless (ret == gstate) (debugM debugLog "New state ")
  return ret
input _ gstate = return gstate

inputMouse :: Event -> GameState -> IO GameState
inputMouse (EventKey (MouseButton btn) Down _ p@(x, y)) gstate = do
  let (cx, cy) = toCell gstate p
  let (nx, ny) = toNaturalCoord gstate p
  debugM debugLog $ "Mouse pressed " ++ unwords [show btn, show x, show y, show cx, show cy, show nx, show ny] 
  return gstate
inputMouse _ gstate = return gstate