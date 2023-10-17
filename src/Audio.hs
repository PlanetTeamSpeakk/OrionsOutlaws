{-# LANGUAGE TypeFamilies #-}
module Audio (Audio.initAudio, finishAudio) where

import Assets (sampleOgg)
import Sound.ProteaAudio

initAudio :: IO Bool
initAudio = do
    success <- Sound.ProteaAudio.initAudio 32 48000 512

    smp <- sampleFromMemoryOgg sampleOgg 1
    _ <- soundPlay smp 1 1 0 1

    return success
