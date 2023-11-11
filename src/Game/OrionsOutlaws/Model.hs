{-# LANGUAGE InstanceSigs #-}
{-# OPTIONS_GHC -Wno-partial-fields #-}
-- | This module contains the data types
--   which represent the state of the game
module Game.OrionsOutlaws.Model (module Game.OrionsOutlaws.Model) where

import System.Log.Formatter             (tfLogFormatter, LogFormatter)
import System.Log.Logger                (rootLoggerName)
import Game.OrionsOutlaws.Util.Util     (msTime, lerp, rotatePointAround, pointAngle)
import Game.OrionsOutlaws.Rendering.UI  (UI, withParent)
import Data.Ord                         (clamp)
import Data.Bifunctor                   (bimap)
import Data.Maybe                       (isJust)
import Data.Time                        (UTCTime)
import Graphics.Gloss.Data.Picture      (Picture)
import Graphics.Gloss.Interface.IO.Game (Key (..), SpecialKey (KeySpace))
import Graphics.Gloss.Geometry.Angle    (radToDeg, degToRad)
import Data.Functor                     ((<&>))
import Game.OrionsOutlaws.Util.Registry (RegistryEntry (..))
import {-# SOURCE #-} Game.OrionsOutlaws.UI.MenuUI (menuUI)

-- | Some logging-related constants
--
--   [Formats here](https://hackage.haskell.org/package/time-1.12.2/docs/Data-Time-Format.html)
logFormatter :: Bool -> LogFormatter a
logFormatter includeName = tfLogFormatter "%X" $ "[$time : " ++ (if includeName then "$loggername : " else "") ++ "$prio] $msg"

-- | The default log name, rarely used.
defLog :: String
defLog = rootLoggerName

-- | The debug log name, used for debugging.
debugLog :: String
debugLog = "Debug"

-- CONSTANTS
-- | The value assets are scaled by when displayed in the game.
assetScale :: Float
assetScale = 4

-- | The number of steps per second
stepsPerSec :: Int
stepsPerSec = 30

-- | The length of a step in milliseconds
stepLengthMs :: Int
stepLengthMs = 1000 `div` stepsPerSec

-- | The initial state of the player
initialPlayer :: Player
initialPlayer = Player (-540, 0) (-540, 0) (emptyMovement L2R) 3 0 0

-- | The initial game state
initialState :: Settings -> [Score] -> Bool -> IO GameState
initialState s ss d = msTime >>= (\time -> return $ GameState
  { player       = initialPlayer
  , enemies      = []
  , projectiles  = []
  , animations   = []
  , activeUI     = Just menuUI 
  , score        = 0
  , lastSpawn    = 0
  , elapsedTime  = 0
  , windowSize   = (1280, 720)
  , mousePos     = Nothing
  , steps        = 0
  , lastStep     = time
  , settings     = s
  , scores       = ss
  , keyListeners = []
  , debug        = d
  })

-- | The size of enemies in pixels
enemySize :: Float
enemySize = 48

-- | The speed at which projectiles travel by default.
projectileSpeed :: Float
projectileSpeed = 30

-- | The margin between the player and the edge of the screen
margin :: (Int, Int)
margin = (140, 50)

-- | Half of the square root of 2
--
--   Used for movement calculations
halfRt2 :: Float
halfRt2 = 0.5 * sqrt 2

-- | Calculates the progress between the previous step and the current step.
--
--   Used to linearly interpolate positions when rendering.
stepDelta :: Integer -> Integer -> Float
stepDelta prev current = fromIntegral (current - prev) / fromIntegral stepLengthMs


-- Data types
-- | The game state. Contains everything that is needed to render and simulate the game.
data GameState = GameState
  { player       :: Player                -- ^ The player
  , enemies      :: [Enemy]               -- ^ A list of enemies
  , projectiles  :: [Projectile]          -- ^ A list of projectiles currently on the field
  , animations   :: [PositionedAnimation] -- ^ A list of animations currently on the field
  , activeUI     :: Maybe (RegistryEntry UI)              -- ^ The currently active UI, if any
  , score        :: Int                   -- ^ The player's score
  , lastSpawn    :: Float                 -- ^ The time at which the last enemy was spawned
  , elapsedTime  :: Float                 -- ^ The time elapsed since the game started
  , windowSize   :: Bounds                -- ^ The size of the window
  , mousePos     :: Maybe Position        -- ^ The position of the mouse. 'Nothing' if the mouse is outside the window
  , steps        :: Integer               -- ^ The number of steps that have been taken since the game started
  , lastStep     :: Integer               -- ^ The time in milliseconds at which the last step was taken. Used to calculate step delta and nothing else
  , settings     :: Settings              -- ^ The game settings
  , scores       :: [Score]               -- ^ The high scores
  , keyListeners :: [Key -> IO ()]        -- ^ A list of key listeners that will be called when a key is pressed
  , debug        :: Bool                  -- ^ Whether debug mode is enabled
  }

instance Show GameState where
  show :: GameState -> String
  show g = "GameState { player = " ++ show (player g) ++ ", enemies = " ++ show (enemies g) ++ ", projectiles = " ++ show (projectiles g) ++
    ", animations = " ++ show (animations g) ++ ", activeUI = " ++ show (activeUI g) ++ ", score = " ++ show (score g) ++ ", lastSpawn = " ++ show (lastSpawn g) ++
    ", elapsedTime = " ++ show (elapsedTime g) ++ ", windowSize = " ++ show (windowSize g) ++ ", mousePos = " ++ show (mousePos g) ++ ", steps = " ++ show (steps g) ++
    ", lastStep = " ++ show (lastStep g) ++ ", settings = " ++ show (settings g) ++ " }"

instance Eq GameState where
  (==) :: GameState -> GameState -> Bool
  g1 == g2 = player g1 == player g2 && enemies g1 == enemies g2 && projectiles g1 == projectiles g2 && animations g1 == animations g2 &&
    activeUI g1 == activeUI g2 && score g1 == score g2 && lastSpawn g1 == lastSpawn g2 && elapsedTime g1 == elapsedTime g2 &&
    windowSize g1 == windowSize g2 && mousePos g1 == mousePos g2 && steps g1 == steps g2 && lastStep g1 == lastStep g2 && settings g1 == settings g2

-- | The player. Can move and shoot and there'll only ever be one.
data Player = Player
  { playerPos       :: Position -- ^ Player's position on the field.
  , prevPlayerPos   :: Position -- ^ Player's previous position on the field. Used for rendering the player
  , playerMovement  :: Movement -- ^ Player's movement
  , health          :: Int      -- ^ Player's health. Between 0 and 3
  , cooldown        :: Int      -- ^ How many steps until the player can shoot again.
  , mslCooldown     :: Int      -- ^ How many steps until the player can shoot a missile again.
  } deriving (Show, Eq)

-- | What directions an object is moving in.
--   Also determines whether the object is moving from left to right or right to left.
data Movement = Movement
  { forward     :: Bool  -- ^ Whether the player is moving forwards
  , backward    :: Bool  -- ^ Whether the player is moving backwards
  , left        :: Bool  -- ^ Whether the player is moving left
  , right       :: Bool  -- ^ Whether the player is moving right
  , direction   :: MovementDirection -- ^ The direction this movement goes in.
  , lastChange  :: Float -- ^ The time at which the last change in direction happened
  } deriving (Show, Eq)

-- | The direction in which something is moving. Either left to right or right to left.
data MovementDirection = L2R | R2L deriving (Show, Eq)

data Enemy =
    -- | Regular enemy, moves in a straight line and shoots at the player
    RegularEnemy {
        enemyPos        :: Position, -- ^ Enemy's position on the field
        prevEnemyPos    :: Position, -- ^ Enemy's previous position on the field. Used for rendering the enemy
        enemyMovement   :: Movement, -- ^ Enemy's movement
        enemyCooldown   :: Int       -- ^ How many steps until the enemy can shoot again. Between 0 and 5.
    }
    -- 
    -- BossEnemy { -- Boss enemy, will not move in a straight line, but rather anywhere on the field and shoot at the player.
    --     enemyPos        :: Position,    -- Boss' position on the field
    --     prevEnemyPos    :: Position,    -- Enemy's previous position on the field. Used for rendering the boss
    --     enemyCooldown   :: Int,         -- How many steps until the boss can shoot again.
    --     enemyMovement   :: Movement,    -- Enemy's movement
    --     bossHealth      :: Int          -- Boss' health. Will be between 0 and 10
    -- } 
    deriving (Show, Eq)

-- | A projectile fired by either the player or an enemy.
data Projectile =
  RegularProjectile
    { projPos         :: Position     -- ^ Projectile's position on the field
    , prevProjPos     :: Position     -- ^ Projectile's previous position on the field. Used for rendering the projectile
    , projMovement    :: Movement     -- ^ Projectile's movement
    , friendly        :: Friendliness -- ^ Whether the projectile is friendly or not. If it is, it will hurt enemies, otherwise it will hurt the player
    , speed           :: Float        -- ^ How fast the projectile moves. Should be between 0.5 and 2
    } |
  MissileProjectile
    { projPos         :: Position
    , prevProjPos     :: Position
    , friendly        :: Friendliness
    , speed           :: Float
    , mslTarget       :: Position     -- ^ The position the missile is going towards
    , mslStartPos     :: Position     -- ^ The position the missile started at
    , mslRotation     :: Float        -- ^ The rotation of the missile
    }
    deriving (Show, Eq)

-- | Objects that can have a position and move.
class Positionable a where
  -- | The current position of the object.
  curPosition  :: a -> Position
  -- | The previous position of the object.
  prevPosition :: a -> Position
  -- | Returns a version of this object with the given position.
  withPosition :: a -> Position -> a
  -- | The movement of the object.
  movement     :: a -> Movement

-- | Calculates the interpolated position using the previous position, the current position and the step delta.
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
  movement p@(RegularProjectile {}) = projMovement p
  movement   (MissileProjectile {}) = emptyMovement L2R -- Missiles always move towards their target.


-- Class for things that can collide with other things.
class Collidable a where
  -- | Creates a list of boxes that wrap this object.
  createBoxes :: a -> [Box]

  -- | Creates a list of boxes that wrap this object at the given position.
  createBoxesAt :: Position -> a -> [Box]

  -- | Checks whether the given collidable collides with the given box.
  collidesWithBox :: a -> Box -> Bool
  collidesWithBox a b = intersects b `any` createBoxes a

  -- | Checks whether the given collidable collides with any of the given boxes.
  collidesWith :: Collidable b => a -> b -> Bool
  collidesWith a b = collidesWithBox a `any` createBoxes b

instance Collidable Player where
  createBoxes p = createBoxesAt (curPosition p) p
  createBoxesAt (x, y) _ =
    [ ((x - 12, y - 32), (x + 28, y + 32)) -- Body
    , ((x + 28, y - 24), (x + 40, y + 24)) -- Head
    , ((x + 40, y - 16), (x + 48, y + 16)) -- Tip
    ]

instance Collidable Enemy where
  createBoxes e          = createBoxesAt (curPosition e) e
  createBoxesAt (x, y) _ =
    [ ((x - 24, y - 16), (x + 24, y + 16)) -- Main body
    , ((x - 40, y - 12), (x - 24, y + 12)) -- 'Cockpit'
    , ((x + 24, y - 24), (x + 40, y + 24)) -- Engine
    , ((x - 36, y + 32), (x + 28, y + 48)) -- Right wing
    , ((x - 12, y + 16), (x +  8, y + 32)) -- Right bridge
    , ((x - 36, y - 48), (x + 28, y - 32)) -- Left wing
    , ((x - 12, y - 32), (x +  8, y - 16)) -- Left bridge
    ]

instance Collidable Projectile where
  createBoxes p = createBoxesAt (curPosition p) p
  createBoxesAt (x, y) (RegularProjectile { friendly = Friendly } ) = [((x - 5,  y - 5), (x + 8, y + 5))]
  createBoxesAt (x, y) (RegularProjectile { friendly = Hostile  } ) = [((x - 12, y - 4), (x + 5, y + 4))]
  createBoxesAt (x, y) (MissileProjectile { mslRotation = r } )     =
    let (cx, cy) = rotatePointAround (x + (5 * assetScale), y) (x, y) $ degToRad r
    in [((cx - 5, cy - 5), (cx + 5, cy + 5))]


-- | An animation is a set of frames that are shown in order.
data Animation = Animation
  { frameCount      :: Int            -- ^ How many frames the animation has
  , frameDuration   :: Int            -- ^ How long each frame lasts in steps
  , frameGetter     :: Int -> Picture -- ^ A function that returns the picture for the given frame
  }

-- | An animation with a position.
data PositionedAnimation = PositionedAnimation
  { animation     :: RegistryEntry Animation -- ^ The animation
  , curFrame      :: Int                     -- ^ The current frame of the animation
  , animationStep :: Int                     -- ^ The current step of the animation
  , animationPos  :: Position                -- ^ The position of the animation
  } deriving (Show, Eq)

instance Eq Animation where
  a1 == a2 = frameCount a1 == frameCount a2 && frameDuration a1 == frameDuration a2

instance Show Animation where
  show a = "Animation { frameCount = " ++ show (frameCount a) ++ ", frameDuration = " ++ show (frameDuration a) ++ " }"

-- | A direction the player is facing.
--
--   Determines what sprite to use when rendering the player.
data PlayerFacing = FacingLeftLeft | FacingLeft | FacingNormal | FacingRight | FacingRightRight deriving (Show, Eq)

-- | What frame to use when rendering a ship. (Such as the player or an enemy)
data ShipFrame = First | Second deriving (Show, Eq)

-- | Game settings that stores anything the user can change.
data Settings = Settings
  { fireKey     :: Key    -- ^ The key used to fire
  , forwardKey  :: Key    -- ^ The key used to move forward
  , backwardKey :: Key    -- ^ The key used to move backward
  , leftKey     :: Key    -- ^ The key used to move left
  , rightKey    :: Key    -- ^ The key used to move right
  , volume      :: Float  -- ^ The volume of sounds played, between 0 and 1
  , spawnRate   :: Float  -- ^ The rate at which enemies spawn, between 0 and 4
  } deriving (Show, Eq)

-- | Default settings
defaultSettings :: Settings
defaultSettings = Settings
  { fireKey     = SpecialKey KeySpace
  , forwardKey  = Char 'w'
  , backwardKey = Char 's'
  , leftKey     = Char 'a'
  , rightKey    = Char 'd'
  , volume      = 1
  , spawnRate   = 1
  }

-- | A score that someone achieved once.
--   For use in saving and loading high scores.
data Score = Score
  { scoreName  :: String  -- ^ The name of the player
  , scoreValue :: Int     -- ^ The score value
  , scoreDate  :: UTCTime -- ^ The date at which the score was achieved
  } deriving (Show, Eq)

instance Ord Score where
  compare s1 s2 = compare (scoreValue s1) (scoreValue s2)

-- | Whether something is friendly or hostile.
data Friendliness = Friendly | Hostile deriving (Show, Eq)

-- Types and helper functions
-- | The Position type, a tuple of two ints representing the x and y coordinates of a point.
type Position = (Float, Float)

-- | A box goes from one position to another, representing a rectangle.
type Box = (Position, Position)

-- | Represents width and height of an area.
type Bounds = (Int, Int)


-- Helper functions

-- | Checks whether the game is paused.
--   True if a UI is open.
paused :: GameState -> Bool
paused gstate = isJust $ activeUI gstate

-- | A Movement with no movement.
emptyMovement :: MovementDirection -> Movement
emptyMovement d = Movement False False False False d 0

-- | Produces a list of all corners of the given box.
corners :: Box -> [Position]
corners ((x1, y1), (x2, y2)) = [(x1, y1), (x1, y2), (x2, y1), (x2, y2)]

-- | Checks whether the given position is inside the given box.
isInBox :: Box -> Position -> Bool
isInBox ((x1, y1), (x2, y2)) (x, y) = x1 <= x && x <= x2 && y1 <= y && y <= y2

-- | Checks whether the two given boxes intersect.
intersects :: Box -> Box -> Bool
intersects ((minx1, miny1), (maxx1, maxy1)) ((minx2, miny2), (maxx2, maxy2)) =
  minx1 <= maxx2 && maxx1 >= minx2 && miny1 <= maxy2 && maxy1 >= miny2

-- | Creates a new projectile with the given position and friendliness.
createProjectile :: Position -> Friendliness -> Projectile
createProjectile pos f = RegularProjectile pos pos (Movement True False False False (friendlinessToMovementDirection f) 0) f 1

createMissile :: Position -> Position -> Friendliness -> Projectile
createMissile pos target f = MissileProjectile pos pos f 1 target pos $ radToDeg $ pointAngle pos target

-- | Converts a friendliness to a movement direction.
friendlinessToMovementDirection :: Friendliness -> MovementDirection
friendlinessToMovementDirection Friendly = L2R
friendlinessToMovementDirection Hostile  = R2L

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

-- | Multiplies the given movement by the given multiplier.
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

-- | Returns the current frame of the given animation.
frame :: PositionedAnimation -> Picture
frame pa@(PositionedAnimation { animation = RegistryEntry { entryValue = a } }) = frameGetter a $ curFrame pa

-- | Positions the given animation at the given position.
positionAnimation :: RegistryEntry Animation -> Position -> PositionedAnimation
positionAnimation a = PositionedAnimation a 0 0

-- | Calculates a facing based on the given movement.
facing :: GameState -> Movement -> PlayerFacing
facing gstate m =
  let (v, h) = calcMovement m
      c = elapsedTime gstate - lastChange m in
        if c > 0.3 && v == 0 -- If the player hasn't changed direction in the last 0.3 seconds and is not moving vertically
          then if h > 0 then FacingLeftLeft else if h < 0 then FacingRightRight else FacingNormal
          else if h > 0 then FacingLeft     else if h < 0 then FacingRight      else FacingNormal

-- | Adds the given score to the given list of scores at the appropriate position.
addScore :: [Score] -> Score -> [Score]
addScore [] s = [s]
addScore (s:ss) s'
  | scoreValue s' > scoreValue s = s' : s : ss
  | otherwise                    = s : addScore ss s'

-- | Normalizes the given motion vector.
normalizeMotion :: (Float, Float) -> (Float, Float)
normalizeMotion (x, y) = let l = sqrt $ x * x + y * y in (x / l, y / l)

-- | Returns whether the given friendliness is friendly.
isFriendly :: Friendliness -> Bool
isFriendly Friendly = True
isFriendly Hostile  = False

-- | Creates a box centered at the given position with the given width and height.
createBox :: Position -> Float -> Float -> Box
createBox (x, y) w h = ((x - w / 2, y - h / 2), (x + w / 2, y + h / 2))

-- | Grows a box by the given width and height.
growBox :: Box -> Float -> Float -> Box
growBox ((x1, y1), (x2, y2)) w h = ((x1 - w / 2, y1 - h / 2), (x2 + w / 2, y2 + h / 2))

-- | Opens a UI while keeping track of the parent.
openUI :: GameState -> Maybe (RegistryEntry UI) -> GameState
openUI gstate Nothing = gstate { activeUI = Nothing }
openUI gstate (Just e@(RegistryEntry _ ui)) = gstate { activeUI =
  Just ui <&> (\ui' -> e { entryValue = ui'}) . withParent (activeUI gstate) }
