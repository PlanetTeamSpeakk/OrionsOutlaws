module Main (main) where

import Game.OrionsOutlaws.Controller        (input, step)
import Game.OrionsOutlaws.Model             (GameState(mousePos, windowSize, steps), debugLog, defLog, initialState, logFormatter, stepsPerSec, Settings(volume))
import Game.OrionsOutlaws.Assets            (freeglutDll)
import Game.OrionsOutlaws.Rendering.View    (view)
import Game.OrionsOutlaws.Util.Audio        (initAudio, finishAudio, loopBgMusic, setVolume)
import Game.OrionsOutlaws.Util.Data         (loadSettings, loadScores)
import Game.OrionsOutlaws.Util.Tasks        (runAndClearTasks)

import Graphics.Gloss.Interface.IO.Game     (playIO, Display(InWindow))
import System.Log.Logger                    (Priority(INFO, DEBUG), addHandler, debugM, removeHandler, rootLoggerName, setLevel, updateGlobalLogger)
import System.Log.Handler                   (setFormatter)
import System.Log.Handler.Simple            (streamHandler)
import System.IO                            (stdout)
import Graphics.Gloss.Interface.Environment (getScreenSize)
import Data.Bifunctor                       (Bifunctor(bimap))
import Graphics.UI.GLUT                     (($=), crossingCallback, Crossing (WindowEntered))
import Control.Monad                        (when)
import Data.IORef                           (IORef, newIORef, writeIORef, readIORef)
import System.IO.Unsafe                     (unsafePerformIO)
import System.Directory                     (setCurrentDirectory, getAppUserDataDirectory, createDirectoryIfMissing)
import qualified Data.ByteString as B       (writeFile)
import System.Environment                   (getArgs)
import Graphics.Gloss.Data.Color (makeColorI)

main :: IO ()
main = do
  args <- getArgs
  let debug = "--debug" `elem` args

  -- Get rid of default, lame logging
  updateGlobalLogger rootLoggerName removeHandler
  updateGlobalLogger debugLog $ setLevel DEBUG
  updateGlobalLogger defLog $ setLevel INFO

  -- Set up logging for the DEBUG log.
  debugHandler <- streamHandler stdout DEBUG >>= \lh -> return $ setFormatter lh $ logFormatter True
  when debug $ updateGlobalLogger debugLog $ addHandler debugHandler

  debugM debugLog "Debug mode enabled" -- Won't show if it's disabled

  -- Set up logging for the regular log.
  defHandler <- streamHandler stdout INFO >>= \lh -> return $ setFormatter lh $ logFormatter False
  updateGlobalLogger defLog $ addHandler defHandler

  -- Get app directory and move there
  appDir <- getAppUserDataDirectory "OrionsOutlaws" -- Get app directory
  createDirectoryIfMissing False appDir             -- Create directory if it doesn't exist
  setCurrentDirectory appDir                        -- Move to app directory

  -- Extract freeglut.dll to the current directory if it doesn't exist
  B.writeFile "freeglut.dll" freeglutDll -- Write freeglut.dll to current directory

  -- Load settings
  s <- loadSettings
  debugM debugLog $ "Loaded settings: " ++ show s

  -- Init audio related stuff
  scs <- initAudio
  setVolume $ volume s
  debugM debugLog $ "Audio init " ++ if scs then "successful" else "unsuccessful"
  loopBgMusic

  state <- loadScores >>= \sc -> initialState s sc debug
  size <- getScreenSize
  let (screenWidth, screenHeight) = bimap (`div` 2) (`div` 2) size
      (windowWidth, windowHeight) = bimap (`div` 2) (`div` 2) $ Game.OrionsOutlaws.Model.windowSize state

  -- Center screen
  playIO (InWindow "Orion's Outlaws" (windowWidth * 2, windowHeight * 2)
      -- Ensure that the window is centered
      (screenWidth - windowWidth, screenHeight - windowHeight))
    (makeColorI 46 34 47 255) -- Background color
    stepsPerSec               -- Steps (ticks) per second
    state                     -- Initial state
    view                      -- View function
    input                     -- Event function
    runTasksAndStep           -- Step function

  debugM debugLog "Exiting"
  finishAudio -- Shutdown audio system

runTasksAndStep :: Float -> GameState -> IO GameState
runTasksAndStep sd gstate = do
  -- First step, has to be called after playIO to ensure that a window is made.
  when (steps gstate == 0) registerCrossingCallback

  -- If the mouse is no longer inside the window, we reset the mouse position to Nothing.
  c <- readIORef crossingState
  let gstate' = gstate { mousePos = if c == WindowEntered then mousePos gstate else Nothing }

  gstate'' <- runAndClearTasks gstate' -- Run tasks
  step sd gstate''                     -- Execute step

-- | The current crossing state of the mouse.
--   Gloss does not have an event for this, so we make our own using GLUT.
crossingState :: IORef Crossing
crossingState = unsafePerformIO $ newIORef WindowEntered
{-# NOINLINE crossingState #-}

-- | Registers a callback that's fired when the mouse enters or leaves the window.
registerCrossingCallback :: IO ()
registerCrossingCallback = do
  debugM debugLog "Registering crossing callback"
  crossingCallback $= Just (writeIORef crossingState)
