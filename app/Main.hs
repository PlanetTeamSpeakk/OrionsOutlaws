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
import Audio (initAudio, finishAudio, loopBgMusic)
import Data
import Data.Bifunctor (Bifunctor(bimap))

main :: IO ()
main = do
  -- Get rid of default, lame logging
  updateGlobalLogger rootLoggerName removeHandler
  updateGlobalLogger debugLog $ setLevel DEBUG
  updateGlobalLogger defLog $ setLevel INFO

  debugHandler <- streamHandler stdout DEBUG >>= \lh -> return $ setFormatter lh logFormatter
  updateGlobalLogger debugLog $ addHandler debugHandler

  -- Init audio related stuff
  scs <- initAudio
  debugM debugLog $ "Audio init " ++ if scs then "successful" else "unsuccessful"
  loopBgMusic

  -- Load settings
  settings <- loadSettings
  debugM debugLog $ "Loaded settings: " ++ show settings

  state <- initialState settings
  size <- getScreenSize
  let (screenWidth, screenHeight) = bimap (`div` 2) (`div` 2) size
      (windowWidth, windowHeight) = bimap (`div` 2) (`div` 2) $ Model.windowSize state

  -- Center screen
  playIO (InWindow "Orion's Outlaws" (windowWidth * 2, windowHeight * 2)
      -- Ensure that the window is centered
      (screenWidth - windowWidth, screenHeight - windowHeight))
    white       -- Background color
    stepsPerSec -- Steps (ticks) per second
    state       -- Initial state
    view        -- View function
    input       -- Event function
    step        -- Step function
  
  debugM debugLog "Exiting"
  finishAudio -- Shutdown audio system
