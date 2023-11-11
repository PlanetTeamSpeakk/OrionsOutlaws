-- Boot file for the Model module.
module Game.OrionsOutlaws.Model 
  ( -- * Constants
    logFormatter
  , defLog
  , debugLog
  , assetScale
  , stepsPerSec
  , stepLengthMs
  , initialPlayer
  , initialState
  , enemySize
  , projectileSpeed
  , margin
  , halfRt2
  , stepDelta

    -- * Types
  , GameState(..)
  , Player
  , Movement
  , MovementDirection
  , Enemy
  , Projectile
  , Positionable
  , Collidable
  , Animation
  , PositionedAnimation
  , PlayerFacing
  , ShipFrame
  , Settings
  , Score
  , Friendliness
  , Position
  , Box
  , Bounds

    -- * Functions
  , paused
  , emptyMovement
  , corners
  , isInBox
  , intersects
  , createProjectile
  , createMissile
  , friendlinessToMovementDirection
  , move
  , lerpPos
  , calcMovement
  , multiplyMovement
  , applyPositionMovement
  , applyMovement
  , subtractMargin
  , frame
  , positionAnimation
  , facing
  , addScore
  , normalizeMotion
  , isFriendly
  , createBox
  , growBox
  , openUI
  ) where

import Graphics.Gloss.Data.Picture (Picture)
import {-# SOURCE #-} Game.OrionsOutlaws.Rendering.UI (UI)
import System.Log.Formatter (LogFormatter)
import Graphics.Gloss.Interface.IO.Game (Key)
import Game.OrionsOutlaws.Util.Registry (RegistryEntry)

logFormatter    :: Bool -> LogFormatter a
defLog          :: String
debugLog        :: String
assetScale      :: Float
stepsPerSec     :: Int
stepLengthMs    :: Int
initialPlayer   :: Player
initialState    :: Settings -> [Score] -> Bool -> IO GameState
enemySize       :: Float
projectileSpeed :: Float
margin          :: (Int, Int)
halfRt2         :: Float
stepDelta       :: Integer -> Integer -> Float

data GameState = GameState
  { player       :: Player
  , enemies      :: [Enemy]
  , projectiles  :: [Projectile]
  , animations   :: [PositionedAnimation]
  , activeUI     :: Maybe (RegistryEntry UI)
  , score        :: Int
  , lastSpawn    :: Float
  , elapsedTime  :: Float
  , windowSize   :: Bounds
  , mousePos     :: Maybe Position
  , steps        :: Integer
  , lastStep     :: Integer
  , settings     :: Settings
  , scores       :: [Score]
  , keyListeners :: [Key -> IO ()]
  , debug        :: Bool
  }
instance Show GameState
instance Eq GameState

data Player
instance Show Player
instance Eq Player

data Movement
instance Show Movement
instance Eq Movement

data MovementDirection
instance Show MovementDirection
instance Eq MovementDirection

data Enemy
instance Show Enemy
instance Eq Enemy

data Projectile
instance Show Projectile
instance Eq Projectile

class Positionable a where
  curPosition  :: a -> Position
  prevPosition :: a -> Position
  withPosition :: a -> Position -> a
  movement     :: a -> Movement


class Collidable a where
  createBoxes :: a -> [Box]
  createBoxesAt :: Position -> a -> [Box]
  collidesWithBox :: a -> Box -> Bool
  collidesWithBox _ _ = undefined
  collidesWith :: Collidable b => a -> b -> Bool
  collidesWith _ _ = undefined

instance Collidable Player
instance Collidable Enemy
instance Collidable Projectile

data Animation
instance Show Animation
instance Eq Animation

data PositionedAnimation
instance Show PositionedAnimation
instance Eq PositionedAnimation

data PlayerFacing
data ShipFrame

data Settings

data Score
instance Ord Score

data Friendliness

type Position = (Float, Float)
type Box = (Position, Position)
type Bounds = (Int, Int)

paused :: GameState -> Bool
emptyMovement :: MovementDirection -> Movement
corners :: Box -> [Position]
isInBox :: Box -> Position -> Bool
intersects :: Box -> Box -> Bool
createProjectile :: Position -> Friendliness -> Projectile
createMissile :: Position -> Position -> Friendliness -> Projectile
friendlinessToMovementDirection :: Friendliness -> MovementDirection
move :: Bounds -> Position -> (Float, Float) -> Position
lerpPos :: Position -> Position -> Float -> Position
calcMovement :: Movement -> (Float, Float)
multiplyMovement :: (Float, Float) -> Float -> (Float, Float)
applyPositionMovement :: Bounds -> Position -> Movement -> Float -> Position
applyMovement :: (Positionable a) => Bounds -> a -> Float -> a
subtractMargin :: Bounds -> Bounds
frame :: PositionedAnimation -> Picture
positionAnimation :: RegistryEntry Animation -> Position -> PositionedAnimation
facing :: GameState -> Movement -> PlayerFacing
addScore :: [Score] -> Score -> [Score]
normalizeMotion :: (Float, Float) -> (Float, Float)
isFriendly :: Friendliness -> Bool
createBox :: Position -> Float -> Float -> Box
growBox :: Box -> Float -> Float -> Box
openUI :: GameState -> Maybe (RegistryEntry UI) -> GameState
