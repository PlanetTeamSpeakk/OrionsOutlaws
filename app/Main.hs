module Main (main) where

import Controller
import Model
import View

import Graphics.Gloss.Interface.IO.Game
import System.Log.Logger
import System.Log.Handler (setFormatter)
import System.Log.Handler.Simple
import System.IO
import Graphics.Gloss.Interface.Environment (getScreenSize)

main :: IO ()
main =
  do
    -- Get rid of default, lame logging
    updateGlobalLogger rootLoggerName removeHandler
    updateGlobalLogger debugLog $ setLevel DEBUG
    updateGlobalLogger defLog $ setLevel INFO

    debugHandler <- streamHandler stdout DEBUG >>= \lh -> return $ setFormatter lh logFormatter
    updateGlobalLogger debugLog $ addHandler debugHandler

    state <- initialState
    size <- getScreenSize
    let (screenWidth, screenHeight) = size
        (windowWidth, windowHeight) = Model.windowSize state

    -- Center screen
    playIO (InWindow "Orion's Outlaws" (windowWidth, windowHeight) 
        -- Ensure that the window is centered
        ((screenWidth `div` 2) - (windowWidth `div` 2), (screenHeight `div` 2) - (windowHeight `div` 2)))
      white       -- Background color
      stepsPerSec -- Frames per second
      state       -- Initial state
      view        -- View function
      input       -- Event function
      step        -- Step function
