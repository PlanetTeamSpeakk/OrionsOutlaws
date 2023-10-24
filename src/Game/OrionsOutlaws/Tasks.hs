-- | A module that allows for queuing tasks to be executed in the next step.
module Game.OrionsOutlaws.Tasks
  ( getQueuedTasks
  , queueTask
  , clearTasks
  , runAndClearTasks
  ) where

import Game.OrionsOutlaws.Model (GameState)
import GHC.IO (unsafePerformIO)
import Data.IORef (newIORef, IORef, readIORef, writeIORef)

-- | Clears all queued tasks.
clearTasks :: IO ()
clearTasks = writeIORef queuedTasks []

-- | A reference to the list of queued tasks.
queuedTasks :: IORef [GameState -> IO GameState]
queuedTasks = unsafePerformIO $ newIORef []
{-# NOINLINE queuedTasks #-}

-- | Returns the list of queued tasks.
getQueuedTasks :: IO [GameState -> IO GameState]
getQueuedTasks = readIORef queuedTasks

-- | Runs all queued tasks and clears the queue.
runAndClearTasks :: GameState -> IO GameState
runAndClearTasks gstate = do
  tasks <- getQueuedTasks
  writeIORef queuedTasks []
  foldl (>>=) (return gstate) tasks

-- | Queues a task to be executed in the next step.
queueTask :: (GameState -> IO GameState) -> IO ()
queueTask task = do
  tasks <- getQueuedTasks
  writeIORef queuedTasks (task : tasks)
