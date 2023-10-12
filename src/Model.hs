{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}

-- | This module contains the data types
--   which represent the state of the game
module Model (module Model) where

import System.Log.Formatter (tfLogFormatter, LogFormatter)
import System.Log.Logger (rootLoggerName)
import Util (msTime, lerp)

-- Some logging-related constants
-- https://hackage.haskell.org/package/time-1.12.2/docs/Data-Time-Format.html
logFormatter :: LogFormatter a
logFormatter = tfLogFormatter "%X" "[$time : $loggername : $prio] $msg"

defLog :: String
defLog = rootLoggerName

debugLog :: String
debugLog = "Debug"

-- Constants
stepsPerSec :: Int
stepsPerSec = 20

stepLengthMs :: Int
stepLengthMs = 1000 `div` stepsPerSec

initialState :: IO GameState
initialState = GameState (Player (-540, 0) (-540, 0) (Movement False False False False) 3 0) [] [] 0 0 False (1280, 720) <$> msTime

playerSize :: Float
playerSize = 40

projectileSpeed :: Float
projectileSpeed = 30

rt2 :: Float
rt2 = 0.5 * sqrt 2

stepDelta :: Integer -> Integer -> Float
stepDelta prev current = fromIntegral (current - prev) / fromIntegral stepLengthMs


-- Data types
data GameState = GameState {
    player      :: Player,          -- The player
    enemies     :: [Enemy],         -- A list of enemies
    projectiles :: [Projectile],    -- A list of projectiles currently on the field
    lastSpawn   :: Float,           -- The time at which the last enemy was spawned
    elapsedTime :: Float,           -- The time elapsed since the game started
    started     :: Bool,            -- Whether the game has started or not
    windowSize  :: (Int, Int),      -- The size of the window
    lastStep    :: Integer          -- The time in milliseconds at which the last step was taken
} deriving (Show, Eq)

data Player = Player {
    playerPos       :: Position,    -- Player's position on the field.
    prevPlayerPos   :: Position,    -- Player's previous position on the field. Used for rendering the player
    movement        :: Movement,    -- Player's movement
    health          :: Int,         -- Player's health. Will be between 0 and 3
    cooldown        :: Int          -- How many steps until the player can shoot again. Will probably be between 0 and 5 if we go with 10 steps per second.
} deriving (Show, Eq)

data Movement = Movement {
    forward     :: Bool,    -- Whether the player is moving forwards
    backward    :: Bool,    -- Whether the player is moving backwards
    left        :: Bool,    -- Whether the player is moving left
    right       :: Bool     -- Whether the player is moving right
} deriving (Show, Eq)

data Enemy =
    RegularEnemy { -- Regular enemy, will move in a straight line and shoot at the player
        enemyPos        :: Position,    -- Enemy's position on the field
        prevEnemyPos    :: Position,    -- Enemy's previous position on the field. Used for rendering the enemy
        enemyCooldown   :: Int          -- How many steps until the enemy can shoot again. Will probably be between 0 and 5 if we go with 10 steps per second.
    } |
    BossEnemy { -- Boss enemy, will not move in a straight line, but rather anywhere on the field and shoot at the player.
        enemyPos        :: Position,    -- Boss' position on the field
        prevEnemyPos    :: Position,    -- Enemy's previous position on the field. Used for rendering the boss
        enemyCooldown   :: Int,         -- How many steps until the boss can shoot again. Will probably be between 0 and 5 if we go with 10 steps per second.
        bossHealth      :: Int          -- Boss' health. Will be between 0 and 10
    } deriving (Show, Eq)

data Projectile = RegularProjectile {
    projPos     :: Position,    -- Projectile's position on the field
    prevProjPos :: Position,    -- Projectile's previous position on the field. Used for rendering the projectile
    friendly    :: Bool,        -- Whether the projectile is friendly or not. If it is, it will hurt enemies, otherwise it will hurt the player
    speed       :: Float        -- How fast the projectile moves. Will probably be between 0.5 and 2
} deriving (Show, Eq)

-- Make a class that has an instance for each data type so that we can use the same function for all of them.
class Positionable a where
    curPosition  :: a -> Position
    prevPosition :: a -> Position

-- Calculates the interpolated position using the previous position, the current position and the step delta.
position :: Positionable a => Float -> a -> Position
position sd a = if sd >= 1 then curPosition a else lerpPos (prevPosition a) (curPosition a) sd

-- Make an instance of Positionable for each data type
instance Positionable Player where
    curPosition = playerPos
    prevPosition = prevPlayerPos

instance Positionable Enemy where
    curPosition = enemyPos
    prevPosition = prevEnemyPos

instance Positionable Projectile where
    curPosition = projPos
    prevPosition = prevProjPos


-- Allows for creation of bounding boxes for entities in the game.
class Boxable a where
    createBoxes :: a -> [Box]

-- Sample implementation for Player which assumes that the player is a 20x20 square and that the position is the center of the square.
instance Boxable Player where
    createBoxes p = let (x, y) = playerPos p in 
        [((x - 10, y - 10), (x + 10, y + 10))]

instance Boxable Enemy where
    createBoxes (RegularEnemy (x, y) _ _) = undefined -- TODO - Implement this
    createBoxes (BossEnemy (x, y) _ _ _)  = undefined

instance Boxable Projectile where
    createBoxes (RegularProjectile (x, y) _ _ _) = undefined

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


-- Helper functions

-- Produces a list of all corners of the given box.
corners :: Box -> [Position]
corners ((x1, y1), (x2, y2)) = [(x1, y1), (x1, y2), (x2, y1), (x2, y2)]

-- Checks whether the given position is inside the given box.
isInBox :: Box -> Position -> Bool
isInBox ((x1, y1), (x2, y2)) (x, y) = x1 <= x && x <= x2 && y1 <= y && y <= y2

-- Checks whether the two given boxes intersect. (I.e., whether box 2 has a corner inside of box 1)
intersects :: Box -> Box -> Bool
intersects b1 b2 = not $ null [c | c <- corners b2, isInBox b1 c]

createProjectile :: Position -> Bool -> Projectile
createProjectile pos f = RegularProjectile pos pos f 1

-- | Moves a position by the given difference
move :: Position -> (Float, Float) -> Position
move (x, y) (dx, dy) = (x + dx, y + dy)

-- | Linearly interpolate between two positions
lerpPos :: Position -> Position -> Float -> Position
lerpPos (x1, y1) (x2, y2) t = (lerp x1 x2 t, lerp y1 y2 t)

-- | Calculates movement based on the given Movement.
-- | Ensures that the player moves at the same speed in all directions.
calcMovement :: Movement -> (Float, Float)
calcMovement m = (if y /= 0 then x * rt2 else x, if x /= 0 then y * rt2 else y)
    where
        x = (if forward m then 1 else 0) + (if backward m then -1 else 0)
        y = (if left m    then 1 else 0) + (if right m    then -1 else 0)

multiplyMovement :: (Float, Float) -> Float -> (Float, Float)
multiplyMovement (x, y) mult = (x * mult, y * mult)

-- | Applies the given movement to the given position.
applyMovement :: Position -> Movement -> Float -> Position
applyMovement p m mult = move p $ multiplyMovement (calcMovement m) mult
