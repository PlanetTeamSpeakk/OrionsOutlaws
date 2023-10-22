-- | This module contains the data types
--   which represent the state of the game
module Model (module Model) where

import System.Log.Formatter (tfLogFormatter, LogFormatter)
import System.Log.Logger (rootLoggerName)
import Util (msTime, lerp)
import Data.Ord (clamp)
import Data.Bifunctor (bimap)
import Graphics.Gloss.Data.Picture (Picture)
import Graphics.Gloss.Interface.IO.Game (Key (..))

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

initialPlayer :: Player
initialPlayer = Player (-540, 0) (-540, 0) (emptyMovement L2R) 3 0

-- | The initial game state
initialState :: Settings -> IO GameState
initialState s = msTime >>= (\time -> return $ GameState Menu initialPlayer [] [] [] 0 0 0 False False (1280, 720) 0 time s)

enemySize :: Float
enemySize = 48

projectileSpeed :: Float
projectileSpeed = 30

-- | The margin between the player and the edge of the screen
margin :: (Int, Int)
margin = (140, 50)

-- Used for movement calculations
halfRt2 :: Float
halfRt2 = 0.5 * sqrt 2

stepDelta :: Integer -> Integer -> Float
stepDelta prev current = fromIntegral (current - prev) / fromIntegral stepLengthMs


-- Data types
data GameState = GameState {
    stateType   :: GameStateType,           -- The type of the game state
    player      :: Player,                  -- The player
    enemies     :: [Enemy],                 -- A list of enemies
    projectiles :: [Projectile],            -- A list of projectiles currently on the field
    animations  :: [PositionedAnimation],   -- A list of animations currently on the field
    score       :: Int,                     -- The player's score
    lastSpawn   :: Float,                   -- The time at which the last enemy was spawned
    elapsedTime :: Float,                   -- The time elapsed since the game started
    started     :: Bool,                    -- Whether the game has started or not
    paused      :: Bool,                    -- Whether the game is paused or not
    windowSize  :: Bounds,                  -- The size of the window
    steps       :: Integer,                 -- The number of steps that have been taken since the game started
    lastStep    :: Integer,                 -- The time in milliseconds at which the last step was taken. Used to calculate step delta and nothing else
    settings    :: Settings                 -- The game settings
} deriving (Show, Eq)

data GameStateType = Menu | Playing | Paused | GameOver deriving (Show, Eq)

data Player = Player {
    playerPos       :: Position,    -- Player's position on the field.
    prevPlayerPos   :: Position,    -- Player's previous position on the field. Used for rendering the player
    playerMovement  :: Movement,    -- Player's movement
    health          :: Int,         -- Player's health. Will be between 0 and 3
    cooldown        :: Int          -- How many steps until the player can shoot again. Will probably be between 0 and 5 if we go with 10 steps per second.
} deriving (Show, Eq)

data MovementDirection = L2R | R2L deriving (Show, Eq)

data Movement = Movement {
    forward     :: Bool,    -- Whether the player is moving forwards
    backward    :: Bool,    -- Whether the player is moving backwards
    left        :: Bool,    -- Whether the player is moving left
    right       :: Bool,    -- Whether the player is moving right
    direction   :: MovementDirection, -- The direction this movement goes in.
    lastChange  :: Float    -- The time at which the last change in direction happened
} deriving (Show, Eq)

data Enemy =
    RegularEnemy { -- Regular enemy, will move in a straight line and shoot at the player
        enemyPos        :: Position,    -- Enemy's position on the field
        prevEnemyPos    :: Position,    -- Enemy's previous position on the field. Used for rendering the enemy
        enemyMovement   :: Movement,    -- Enemy's movement
        enemyCooldown   :: Int          -- How many steps until the enemy can shoot again. Will probably be between 0 and 5 if we go with 10 steps per second.
    } |
    BossEnemy { -- Boss enemy, will not move in a straight line, but rather anywhere on the field and shoot at the player.
        enemyPos        :: Position,    -- Boss' position on the field
        prevEnemyPos    :: Position,    -- Enemy's previous position on the field. Used for rendering the boss
        enemyCooldown   :: Int,         -- How many steps until the boss can shoot again. Will probably be between 0 and 5 if we go with 10 steps per second.
        enemyMovement   :: Movement,    -- Enemy's movement
        bossHealth      :: Int          -- Boss' health. Will be between 0 and 10
    } deriving (Show, Eq)

data Projectile = RegularProjectile {
    projPos         :: Position,    -- Projectile's position on the field
    prevProjPos     :: Position,    -- Projectile's previous position on the field. Used for rendering the projectile
    projMovement    :: Movement,    -- Projectile's movement
    friendly        :: Bool,        -- Whether the projectile is friendly or not. If it is, it will hurt enemies, otherwise it will hurt the player
    speed           :: Float        -- How fast the projectile moves. Will probably be between 0.5 and 2
} deriving (Show, Eq)

-- Make a class that has an instance for each data type so that we can use the same function for all of them.
class Positionable a where
    curPosition  :: a -> Position
    prevPosition :: a -> Position
    withPosition :: a -> Position -> a
    movement :: a -> Movement

-- Calculates the interpolated position using the previous position, the current position and the step delta.
position :: Positionable a => Float -> a -> Position
position sd a = if sd >= 1 then curPosition a else lerpPos (prevPosition a) (curPosition a) sd

-- Make an instance of Positionable for each data type
instance Positionable Player where
    curPosition = playerPos
    prevPosition = prevPlayerPos
    withPosition p pos = p { prevPlayerPos = playerPos p, playerPos = pos }
    movement = playerMovement

instance Positionable Enemy where
    curPosition = enemyPos
    prevPosition = prevEnemyPos
    withPosition e pos = e { prevEnemyPos = enemyPos e, enemyPos = pos }
    movement = enemyMovement

instance Positionable Projectile where
    curPosition = projPos
    prevPosition = prevProjPos
    withPosition p pos = p { prevProjPos = projPos p, projPos = pos }
    movement = projMovement


-- Class for things that can collide with other things.
class Collidable a where
    createBoxes  :: a -> [Box]

-- Sample implementation for Player which assumes that the player is a 20x20 square and that the position is the center of the square.
instance Collidable Player where
    createBoxes p = let (x, y) = playerPos p in [
            ((x - 12, y - 32), (x + 28, y + 32)), -- Body
            ((x + 28, y - 24), (x + 40, y + 24)), -- Head
            ((x + 40, y - 16), (x + 48, y + 16))  -- Tip
        ]

instance Collidable Enemy where
    createBoxes (RegularEnemy (x, y) _ _ _) = [((x - 20, y - 20), (x + 20, y + 20))]
    createBoxes (BossEnemy (_, _) _ _ _ _)  = undefined -- TODO - Implement this

instance Collidable Projectile where
    createBoxes (RegularProjectile (x, y) _ _ _ _) = [((x - 5, y - 5), (x + 5, y + 5))]


-- | An animation is a set of frames that are shown in order.
data Animation = Animation {
    frameCount      :: Int,             -- How many frames the animation has
    frameDuration   :: Int,             -- How long each frame lasts in steps
    curFrame        :: Int,             -- The current frame of the animation
    animationStep   :: Int,             -- The current step of the animation
    frameGetter     :: Int -> Picture   -- A function that gets the picture for the given frame
}

-- | An animation with a position.
data PositionedAnimation = PositionedAnimation {
    animation    :: Animation, -- The animation
    animationPos :: Position   -- The position of the animation
} deriving (Show, Eq)

instance Eq Animation where
    a1 == a2 = frameCount a1 == frameCount a2 && frameDuration a1 == frameDuration a2 &&
        curFrame a1 == curFrame a2 && animationStep a1 == animationStep a2

instance Show Animation where
    show a = "Animation { frameCount = " ++ show (frameCount a) ++ ", frameDuration = " ++ show (frameDuration a) ++
        ", curFrame = " ++ show (curFrame a) ++ ", animationStep = " ++ show (animationStep a) ++ " }"

data PlayerFacing = LeftLeft | Left | Normal | Right | RightRight deriving (Show, Eq)

data ShipFrame = First | Second deriving (Show, Eq)

data Alignment = LeftToRight | RightToLeft deriving (Show, Eq)

-- Game settings that stores anything the user can change.
data Settings = Settings {
    forwardKey  :: Key,
    backwardKey :: Key,
    leftKey     :: Key,
    rightKey    :: Key
} deriving (Show, Eq)

-- Default settings
defaultSettings :: Settings
defaultSettings = Settings {
    forwardKey  = Char 'w',
    backwardKey = Char 's',
    leftKey     = Char 'a',
    rightKey    = Char 'd'
}

-- | A score that someone achieved once.
--   For use in saving and loading high scores.
data Score = Score {
    name       :: String,
    scoreValue :: Int
} deriving (Show, Eq)

instance Ord Score where
    compare s1 s2 = compare (scoreValue s1) (scoreValue s2)

-- Types and helper functions
-- The Position type, a tuple of two ints representing the x and y coordinates of a point.
type Position = (Float, Float)

-- A box goes from one position to another, representing a rectangle.
type Box = (Position, Position)

-- | Represents width and height of an area.
type Bounds = (Int, Int)


-- Helper functions

emptyMovement :: MovementDirection -> Movement
emptyMovement d = Movement False False False False d 0

-- Produces a list of all corners of the given box.
corners :: Box -> [Position]
corners ((x1, y1), (x2, y2)) = [(x1, y1), (x1, y2), (x2, y1), (x2, y2)]

-- Checks whether the given position is inside the given box.
isInBox :: Box -> Position -> Bool
isInBox ((x1, y1), (x2, y2)) (x, y) = x1 <= x && x <= x2 && y1 <= y && y <= y2

-- Checks whether the two given boxes intersect.
intersects :: Box -> Box -> Bool
intersects ((minx1, miny1), (maxx1, maxy1)) ((minx2, miny2), (maxx2, maxy2)) =
    minx1 <= maxx2 && maxx1 >= minx2 && miny1 <= maxy2 && maxy1 >= miny2

createProjectile :: Position -> Bool -> Projectile
createProjectile pos f = RegularProjectile pos pos (Movement True False False False (if f then L2R else R2L) 0) f 1

-- | Moves a position by the given difference
move :: Bounds -> Position -> (Float, Float) -> Position
move (width, height) (x, y) (dx, dy) = let halfWidth = fromIntegral width / 2
                                           halfHeight = fromIntegral height / 2 in
    (clamp (-halfWidth, halfWidth) (x + dx), clamp (-halfHeight, halfHeight) (y + dy))

-- | Linearly interpolate between two positions
lerpPos :: Position -> Position -> Float -> Position
lerpPos (x1, y1) (x2, y2) t = (lerp x1 x2 t, lerp y1 y2 t)

-- | Calculates movement based on the given Movement.
-- | Ensures that the player moves at the same speed in all directions.
calcMovement :: Movement -> (Float, Float)
calcMovement m = invertIfR2L (if y /= 0 then x * halfRt2 else x, if x /= 0 then y * halfRt2 else y)
    where
        x = (if forward m then 1 else 0) + (if backward m then -1 else 0)
        y = (if left m    then 1 else 0) + (if right m    then -1 else 0)
        invertIfR2L (h, v) = if direction m == R2L then (-h, -v) else (h, v)

multiplyMovement :: (Float, Float) -> Float -> (Float, Float)
multiplyMovement (x, y) mult = (x * mult, y * mult)

-- | Applies the given movement to the given position.
applyPositionMovement :: Bounds -> Position -> Movement -> Float -> Position
applyPositionMovement b p m mult = move b p $ multiplyMovement (calcMovement m) mult

-- | Applies the given movement to the given positionable.
applyMovement :: (Positionable a) => Bounds -> a -> Float -> a
applyMovement b a mult = withPosition a $ applyPositionMovement b (curPosition a) (movement a) mult

-- | Subtracts the margin from the given bounds.
subtractMargin :: Bounds -> Bounds
subtractMargin (width, height) = bimap (width -) (height -) margin

-- | Checks whether the given collidable collides with the given box.
collidesWithBox :: (Collidable a) => a -> Box -> Bool
collidesWithBox a b = any (intersects b) $ createBoxes a

-- | Checks whether the given collidable collides with any of the given boxes.
collidesWith :: (Collidable a, Collidable b) => a -> b -> Bool
collidesWith a b = collidesWithBox a `any` createBoxes b

frame :: Animation -> Picture
frame a = frameGetter a $ curFrame a

positionAnimation :: Animation -> Position -> PositionedAnimation
positionAnimation = PositionedAnimation

facing :: GameState -> Movement -> PlayerFacing
facing gstate m =
    let (v, h) = calcMovement m
        c = elapsedTime gstate - lastChange m in
            if c > 0.3 && v == 0 -- If the player hasn't changed direction in the last 0.3 seconds and is not moving vertically
                then if h > 0 then LeftLeft   else if h < 0 then RightRight  else Normal
                else if h > 0 then Model.Left else if h < 0 then Model.Right else Normal
