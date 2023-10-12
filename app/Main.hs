module Main where

import Controller
import Model
import View

import Graphics.Gloss.Interface.IO.Game
import System.Log.Logger
import System.Log.Handler (setFormatter)
import System.Log.Handler.Simple
import System.IO
import Graphics.Gloss.Interface.Environment (getScreenSize)
import Util

main :: IO ()
main =
  do
    -- Get rid of default, lame logging
    updateGlobalLogger rootLoggerName removeHandler
    updateGlobalLogger debugLog $ setLevel DEBUG
    updateGlobalLogger defLog $ setLevel INFO

    debugHandler <- streamHandler stdout DEBUG >>= \lh -> return $ setFormatter lh logFormatter
    updateGlobalLogger debugLog $ addHandler debugHandler

    size <- getScreenSize
    board <- createBoard 16 16 40
    let (screenWidth, screenHeight) = size
        (windowWidth, windowHeight) = getWindowSizeFor board
    
    debugM debugLog $ "Initial state: " ++ show board
    debugM debugLog $ "Board: \n" ++ printBoard board

    -- Center screen
    playIO (InWindow "Minesweeper" (windowWidth, windowHeight) 
        ((screenWidth `div` 2) - (windowWidth `div` 2), (screenHeight `div` 2) - (windowHeight `div` 2)))
      (greyN 0.8)             -- Background color
      10                      -- Frames per second
      board                   -- Initial state
      view                    -- View function
      input                   -- Event function
      step                    -- Step function
