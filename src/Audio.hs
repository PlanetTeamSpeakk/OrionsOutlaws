{-# LANGUAGE TypeFamilies #-}
module Audio (Audio.initAudio, finishAudio) where

import Assets (sampleOgg)
import Sound.ProteaAudio

initAudio :: IO Bool
initAudio = do
    success <- Sound.ProteaAudio.initAudio 32 44100 512

    _ <- soundPlay sampleOgg 1 1 0 1

    return success
