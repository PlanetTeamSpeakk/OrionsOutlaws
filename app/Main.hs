module Main (main) where

import Game.OrionsOutlaws.Controller        (input, step)
import Game.OrionsOutlaws.Model             (GameState(..), debugLog, defLog, initialState, logFormatter, stepsPerSec, Settings(volume))
import Game.OrionsOutlaws.Assets            (freeglutDll)
import Game.OrionsOutlaws.Rendering.View    (view)
import Game.OrionsOutlaws.Util.Audio        (initAudio, finishAudio, loopBgMusic, setVolume)
import Game.OrionsOutlaws.Util.Data         (loadSettings, loadScores, writeScores, writeGameState, loadGameState)
import Game.OrionsOutlaws.Util.Tasks        (runAndClearTasks)

import Graphics.Gloss.Interface.IO.Game     (playIO, Display(InWindow))
import Graphics.Gloss.Data.Color            (makeColorI)
import Graphics.Gloss.Interface.Environment (getScreenSize)
import Graphics.UI.GLUT                     (($=), crossingCallback, Crossing (WindowEntered), closeCallback)
import System.Log.Logger                    (Priority(INFO, DEBUG), addHandler, debugM, removeHandler, rootLoggerName, setLevel, updateGlobalLogger)
import System.Log.Handler                   (setFormatter)
import System.Log.Handler.Simple            (streamHandler)
import System.IO                            (stdout)
import System.IO.Unsafe                     (unsafePerformIO)
import System.Directory                     (setCurrentDirectory, getAppUserDataDirectory, createDirectoryIfMissing)
import System.Environment                   (getArgs)
import Data.Bifunctor                       (Bifunctor(bimap))
import Data.IORef                           (IORef, newIORef, writeIORef, readIORef)
import qualified Data.ByteString as B       (writeFile)
import Control.Monad                        (when)

main :: IO ()
main = do
  -- Run with 'stack run -- --debug' to enable debug mode
  args <- getArgs
  let debugMode = "--debug" `elem` args

  -- Get rid of default, lame logging
  updateGlobalLogger rootLoggerName removeHandler
  updateGlobalLogger debugLog $ setLevel DEBUG
  updateGlobalLogger defLog $ setLevel INFO

  -- Set up logging for the DEBUG log.
  debugHandler <- streamHandler stdout DEBUG >>= \lh -> return $ setFormatter lh $ logFormatter True
  when debugMode $ updateGlobalLogger debugLog $ addHandler debugHandler

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

  -- Load gamestate (if one is available)
  savedState <- loadGameState
  debugM debugLog $ "Loaded gamestate: " ++ show savedState

  state <- loadScores >>= \sc -> initialState s sc debugMode
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

runTasksAndStep :: Float -> GameState -> IO GameState
runTasksAndStep sd gstate = do
  -- First step, has to be called after playIO to ensure that a window is made.
  when (steps gstate == 0) $ do
    registerCrossingCallback
    registerCloseCallback

  -- If the mouse is no longer inside the window, we reset the mouse position to Nothing.
  c <- readIORef crossingState
  let gstate' = gstate { mousePos = if c == WindowEntered then mousePos gstate else Nothing }

  gstate''  <- runAndClearTasks gstate' -- Run tasks
  gstate''' <- step sd gstate''         -- Execute step

  writeIORef gameStateRef gstate'''     -- Update the global gamestate

  -- Ensure that we have at least one step.
  -- So that we don't register the callbacks every step.
  return gstate''' { steps = max 1 $ steps gstate''' }

-- | The current GameState. Used in some event handlers.
gameStateRef :: IORef GameState
gameStateRef = unsafePerformIO $ newIORef undefined
{-# NOINLINE gameStateRef #-}

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

-- | Registers a callback that's fired when the window is closed.
registerCloseCallback :: IO ()
registerCloseCallback = do
  debugM debugLog "Registering close callback"
  closeCallback $= Just onClose

-- | Ensures the GameState is saved when the window is closed
--   and that we cleanup the audio system.
onClose :: IO ()
onClose = do
  debugM debugLog "Exiting"
  finishAudio -- Shutdown audio system

  -- Save the scores and gamestate
  gstate <- readIORef gameStateRef
  writeScores $ scores gstate
  writeGameState gstate
