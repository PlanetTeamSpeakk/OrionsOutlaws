{-# LANGUAGE TypeFamilies #-}
-- | Very simple audio manager.
module Game.OrionsOutlaws.Audio 
  ( Game.OrionsOutlaws.Audio.initAudio
  , finishAudio
  , setVolume
  , playSound
  , loopSound
  , getPlaying
  , stopAllSounds
  , pauseAllSounds
  , resumeAllSounds
  , loopBgMusic
  ) where

import Sound.ProteaAudio (Sound, Sample, finishAudio, initAudio, soundActive, soundLoop, soundPlay, soundStop, soundUpdate, volume)
import Game.OrionsOutlaws.Assets (bgMusic)
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import GHC.IO (unsafePerformIO)
import Control.Monad (filterM)

initAudio :: IO Bool
initAudio = Sound.ProteaAudio.initAudio 32 44100 512

-- | All sounds currently playing, internal use only.
playing :: IORef [Sound]
playing = unsafePerformIO $ newIORef []
{-# NOINLINE playing #-}

-- | Sets the global volume.
setVolume :: Float -> IO ()
setVolume v = volume v v

-- | Adds the given sound to the playing sounds list. Internal use only.
addSound :: Sound -> IO ()
addSound sound = do
    sounds <- readIORef playing
    writeIORef playing $ sound : sounds

-- | Returns all sounds that are still playing.
--   Also removes any sounds that are no longer playing from the list.
getPlaying :: IO [Sound]
getPlaying = do
    sounds <- readIORef playing
    stillActive <- filterM soundActive sounds
    writeIORef playing stillActive
    return stillActive

stopAllSounds :: IO ()
stopAllSounds = do
    sounds <- getPlaying
    mapM_ soundStop sounds

pauseAllSounds :: IO ()
pauseAllSounds = do
    sounds <- getPlaying
    mapM_ pauseSound sounds

pauseSound :: Sound -> IO ()
pauseSound sound = do
    _ <- soundUpdate sound True 1 1 0 1
    return ()

resumeAllSounds :: IO ()
resumeAllSounds = do
    sounds <- getPlaying
    mapM_ resumeSound sounds

resumeSound :: Sound -> IO ()
resumeSound sound = do
    _ <- soundUpdate sound False 1 1 0 1
    return ()

playSound :: Sample -> IO ()
playSound sample = do
    sound <- soundPlay sample 1 1 0 1 -- Numbers are: left volume, right volume, time difference and pitch
    addSound sound

loopSound :: Sample -> IO ()
loopSound sample = do
    sound <- soundLoop sample 1 1 0 1
    addSound sound

loopBgMusic :: IO ()
loopBgMusic = loopSound bgMusic
