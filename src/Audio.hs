{-# LANGUAGE TypeFamilies #-}
module Audio (Audio.initAudio, finishAudio, playSound, loopSound, getPlaying, stopAllSounds, loopBgMusic) where

import Sound.ProteaAudio
import Assets (bgMusic)
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import GHC.IO (unsafePerformIO)

initAudio :: IO Bool
initAudio = Sound.ProteaAudio.initAudio 32 44100 512

-- | All sounds currently playing
playing :: IORef [Sound]
playing = unsafePerformIO $ newIORef []
{-# NOINLINE playing #-}

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
    let stillActive = filter (unsafePerformIO . soundActive) sounds
    writeIORef playing stillActive
    return stillActive

stopAllSounds :: IO ()
stopAllSounds = do
    sounds <- getPlaying
    mapM_ soundStop sounds

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
