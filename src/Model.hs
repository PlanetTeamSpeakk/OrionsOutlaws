-- | This module contains the data types
--   which represent the state of the game
module Model where

import System.Log.Formatter (tfLogFormatter, LogFormatter)
import System.Log.Logger (rootLoggerName)
import Data.BitVector
import Text.Printf (printf)

secondsBetweenCycles :: Float
secondsBetweenCycles = 5

-- https://hackage.haskell.org/package/time-1.12.2/docs/Data-Time-Format.html
logFormatter :: LogFormatter a
logFormatter = tfLogFormatter "%X" "[$time : $loggername : $prio] $msg"

defLog :: String
defLog = rootLoggerName

debugLog :: String
debugLog = "Debug"

data GameState = GameState {
                    boardSize   :: (Int, Int),
                    bombCount   :: Int,
                    bombs       :: BitVector,
                    opened      :: BitVector,
                    started     :: Bool,
                    elapsedTime :: Float
                 }

instance Eq GameState where
  GameState s1 _ b1 o1 st1 t1 == GameState s2 _ b2 o2 st2 t2 = s1 == s2 && b1 == b2 && o1 == o2 && st1 == st2 && t1 == t2

instance Show GameState where
  show (GameState (bw, bh) bc b o s e) = printf "GameState{boardSize=(%d, %d), bombCount=%d, bombs=%s, opened=%s, started=%s, elapsedTime=%f}" 
    bw bh bc (showHex b) (showHex o) (show s) e