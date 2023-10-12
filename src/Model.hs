{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}

-- | This module contains the data types
--   which represent the state of the game
module Model (module Model) where

import System.Log.Formatter (tfLogFormatter, LogFormatter)
import System.Log.Logger (rootLoggerName)

secondsBetweenCycles :: Float
secondsBetweenCycles = 5

-- https://hackage.haskell.org/package/time-1.12.2/docs/Data-Time-Format.html
logFormatter :: LogFormatter a
logFormatter = tfLogFormatter "%X" "[$time : $loggername : $prio] $msg"

defLog :: String
defLog = rootLoggerName

debugLog :: String
debugLog = "Debug"

initialState :: GameState
initialState = GameState (Player (-540, 0) 3 0) [] [] 0 0 False (1280, 720)

playerSize :: Float
playerSize = 40

data GameState = GameState {
    player      :: Player,          -- The player
    enemies     :: [Enemy],         -- A list of enemies
    projectiles :: [Projectile],    -- A list of projectiles currently on the field
    lastSpawn   :: Float,           -- The time at which the last enemy was spawned
    elapsedTime :: Float,           -- The time elapsed since the game started
    started     :: Bool,            -- Whether the game has started or not
    windowSize  :: (Int, Int)       -- The size of the window
} deriving (Show, Eq)

data Player = Player {
    playerPos   :: Position,   -- Player's position on the field. Whether this is a pair of doubles or ints depends on how Gloss handles coordinates
    health      :: Int,        -- Player's health. Will be between 0 and 3
    cooldown    :: Int         -- How many steps until the player can shoot again. Will probably be between 0 and 5 if we go with 10 steps per second.
} deriving (Show, Eq)

data Enemy =
    RegularEnemy { -- Regular enemy, will move in a straight line and shoot at the player
        enemyPos        :: Position,    -- Enemy's position on the field
        enemyCooldown   :: Int          -- How many steps until the enemy can shoot again. Will probably be between 0 and 5 if we go with 10 steps per second.
    } |
    BossEnemy { -- Boss enemy, will not move in a straight line, but rather anywhere on the field and shoot at the player.
        enemyPos        :: Position,    -- Boss' position on the field
        enemyCooldown   :: Int,         -- How many steps until the boss can shoot again. Will probably be between 0 and 5 if we go with 10 steps per second.
        bossHealth      :: Int          -- Boss' health. Will be between 0 and 10
    } deriving (Show, Eq)

data Projectile = RegularProjectile {
    projPos     :: Position,    -- Projectile's position on the field
    friendly    :: Bool,        -- Whether the projectile is friendly or not. If it is, it will hurt enemies, otherwise it will hurt the player
    speed       :: Float        -- How fast the projectile moves. Will probably be between 0.5 and 2
} deriving (Show, Eq)

-- Make a class that has an instance for each data type so that we can use the same function for all of them.
class Positionable a where
    position :: a -> Position

-- Make an instance of Positionable for each data type
instance Positionable Player where position = playerPos
instance Positionable Enemy where position = enemyPos
instance Positionable Projectile where position = projPos

-- Allows for creation of bounding boxes for entities in the game.
class Boxable a where
    createBoxes :: a -> [Box]

-- Sample implementation for Player which assumes that the player is a 20x20 square and that the position is the center of the square.
instance Boxable Player where
    createBoxes (Player (x, y) _ _) = [((x - 10, y - 10), (x + 10, y + 10))]

instance Boxable Enemy where
    createBoxes (RegularEnemy (x, y) _) = undefined -- TODO - Implement this
    createBoxes (BossEnemy (x, y) _ _)  = undefined

instance Boxable Projectile where
    createBoxes (RegularProjectile (x, y) _ _) = undefined

-- Class for things that can collide with other things.
class Collidable a where
    collidesWith :: Box -> a -> Bool

instance Collidable Box where
    collidesWith = intersects

instance Boxable a => Collidable a where
    collidesWith b = any (intersects b) . createBoxes


-- Types and helper functions
-- The Position type, a tuple of two ints representing the x and y coordinates of a point.
type Position = (Float, Float)

-- A box goes from one position to another, representing a rectangle.
type Box = (Position, Position)

-- Produces a list of all corners of the given box.
corners :: Box -> [Position]
corners ((x1, y1), (x2, y2)) = [(x1, y1), (x1, y2), (x2, y1), (x2, y2)]

-- Checks whether the given position is inside the given box.
isInBox :: Box -> Position -> Bool
isInBox ((x1, y1), (x2, y2)) (x, y) = x1 <= x && x <= x2 && y1 <= y && y <= y2

-- Checks whether the two given boxes intersect. (I.e., whether box 2 has a corner inside of box 1)
intersects :: Box -> Box -> Bool
intersects b1 b2 = not $ null [c | c <- corners b2, isInBox b1 c]
