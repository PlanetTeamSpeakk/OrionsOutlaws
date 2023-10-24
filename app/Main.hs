module Main (main) where

import Game.OrionsOutlaws.Controller
import Game.OrionsOutlaws.Model
import Game.OrionsOutlaws.View
import Game.OrionsOutlaws.Audio (initAudio, finishAudio, loopBgMusic)
import Game.OrionsOutlaws.Data
import Game.OrionsOutlaws.Assets (pixeboyFont)

import Graphics.Gloss.Interface.IO.Game
import System.Log.Logger
import System.Log.Handler (setFormatter)
import System.Log.Handler.Simple
import System.IO
import Graphics.Gloss.Interface.Environment (getScreenSize)
import Data.Bifunctor (Bifunctor(bimap))
import Game.OrionsOutlaws.Font (getGlyph)

main :: IO ()
main = do
  -- Get rid of default, lame logging
  updateGlobalLogger rootLoggerName removeHandler
  updateGlobalLogger debugLog $ setLevel DEBUG
  updateGlobalLogger defLog $ setLevel INFO

  -- Set up logging for the DEBUG log.
  debugHandler <- streamHandler stdout DEBUG >>= \lh -> return $ setFormatter lh $ logFormatter True
  updateGlobalLogger debugLog $ addHandler debugHandler

  -- Set up logging for the regular log.
  defHandler <- streamHandler stdout INFO >>= \lh -> return $ setFormatter lh $ logFormatter False
  updateGlobalLogger defLog $ addHandler defHandler

  -- Load settings
  s <- loadSettings
  debugM debugLog $ "Loaded settings: " ++ show s

  -- Init audio related stuff
  scs <- initAudio
  debugM debugLog $ "Audio init " ++ if scs then "successful" else "unsuccessful"
  loopBgMusic $ volume s

  debugM debugLog $ "Pixeboy A: " ++ show (getGlyph pixeboyFont 'A')

  state <- initialState s
  size <- getScreenSize
  let (screenWidth, screenHeight) = bimap (`div` 2) (`div` 2) size
      (windowWidth, windowHeight) = bimap (`div` 2) (`div` 2) $ Game.OrionsOutlaws.Model.windowSize state

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
