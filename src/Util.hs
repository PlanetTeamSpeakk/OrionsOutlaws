module Util where
    
import Data.Time.Clock.POSIX (getPOSIXTime)

lerp :: Num a => a -> a -> a -> a
lerp a b t = a + (b - a) * t

msTime :: IO Integer
msTime = round . (* 1000) <$> getPOSIXTime
