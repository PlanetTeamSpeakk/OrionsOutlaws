{-# LANGUAGE TypeFamilies #-}
-- | Very simple audio manager.
module Game.OrionsOutlaws.Util.Audio 
  ( Game.OrionsOutlaws.Util.Audio.initAudio
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

import Sound.ProteaAudio          (Sound, Sample, finishAudio, initAudio, soundActive, soundLoop, soundPlay, soundStop, soundUpdate, volume)
import Game.OrionsOutlaws.Assets  (bgMusic)
import Data.IORef                 (IORef, newIORef, readIORef, writeIORef)
import Control.Monad              (filterM)
import System.IO.Unsafe           (unsafePerformIO)

-- | Initializes the audio system.
--
--   Returns whether the audio system was successfully initialized.
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

-- | Stops all currently playing sounds.
stopAllSounds :: IO ()
stopAllSounds = getPlaying >>= mapM_ soundStop

-- | Pauses all currently playing sounds.
pauseAllSounds :: IO ()
pauseAllSounds = getPlaying >>= mapM_ pauseSound

-- | Pauses the given sound.
pauseSound :: Sound -> IO ()
pauseSound sound = do
  _ <- soundUpdate sound True 1 1 0 1
  return ()

-- | Resumes all paused, but active sounds.
resumeAllSounds :: IO ()
resumeAllSounds = getPlaying >>= mapM_ resumeSound

-- | Resumes the given sound.
resumeSound :: Sound -> IO ()
resumeSound sound = do
  _ <- soundUpdate sound False 1 1 0 1
  return ()

-- | Plays the given sound once.
playSound :: Sample -> IO ()
-- Numbers are: left volume, right volume, time difference and pitch
playSound sample = soundPlay sample 1 1 0 1 >>= addSound

-- | Loops the given sound.
loopSound :: Sample -> IO ()
loopSound sample = soundLoop sample 1 1 0 1 >>= addSound

-- | Loops the background music.
loopBgMusic :: IO ()
loopBgMusic = loopSound bgMusic
