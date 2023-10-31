{-# LANGUAGE TemplateHaskell #-}

-- | This module contains all the assets used in the game.
-- | Assets are packed using file-embed
module Game.OrionsOutlaws.Assets
  ( 
  -- * Images
    explosionAnimation
  , fromPlayerFacing
  , missile1, missile2

  -- ** Background
  , shadows, stars, bigStars, blueStar, redStar, blackHole, smallRotaryStar, rotaryStar

  -- * Sounds
  , bgMusic
  -- ** Laser sounds
  , laserOld, laser1, laser2
  -- ** Explosion sounds
  , explosion1, explosion2

  -- * Misc
  , freeglutDll
  , pixeboyFont
  ) where

import Graphics.Gloss
import Data.ByteString (ByteString)
import Data.FileEmbed (embedFile, embedStringFile) -- Uses the magic of TemplateHaskell to turn files into bytestrings at compile time
import Game.OrionsOutlaws.Model (Animation(Animation), PlayerFacing(..), ShipFrame(..), assetScale)
import Codec.Picture.Png (decodePng)
import Graphics.Gloss.Juicy (fromDynamicImage)
import Sound.ProteaAudio (Sample, sampleFromMemoryOgg, sampleFromMemoryMp3)
import System.IO.Unsafe (unsafePerformIO)
import Game.OrionsOutlaws.Rendering.Font (Font, loadFont)

-- | Attempts to load a ByteString representing a PNG into a BitmapData.
--   If the bytestring is not a valid PNG, returns Nothing.
loadPNG :: ByteString -> Maybe BitmapData
loadPNG bstr = case decodePng bstr of
  Left _    -> Nothing
  Right pic -> case fromDynamicImage pic of
    Nothing -> Nothing
    Just p  -> Just $ extractBitmapData p

-- | Extracts the BitmapData from a Picture.
--   PNGs can only be loaded into a Picture and not a BitmapData.
--   We need a BitmapData, however, so we can use bitmapSection.
--   Behind the scenes, they're still converted to BitmapData, so this should be safe.
extractBitmapData :: Picture -> BitmapData
extractBitmapData (Bitmap bdata) = bdata
extractBitmapData _              = error "extractBitmapData: Not a bitmap"

-- | The freeglut dll, required for the game to run.
freeglutDll :: ByteString
freeglutDll = $(embedFile "assets/freeglut.dll")

-- | Pixeboy font. Used for all text.
pixeboyFont :: Font
pixeboyFont = case loadPNG $(embedFile "assets/fonts/pixeboy.png") of
  Nothing    -> error "pixeboyFont: Could not load font"
  Just bdata -> loadFont $(embedStringFile "assets/fonts/pixeboy.fnt") bdata

-- IMAGES
--- Spritesheets

-- | Explosion animation
--
--   Spritesheets have to be bitmaps so that we can use bitmapSection.
explosionSheet :: Maybe BitmapData
explosionSheet = loadPNG $(embedFile "assets/images/spritesheets/explosion.png")

-- | Returns the nth frame of the explosion animation.
explosionFrame :: Int -> Picture
explosionFrame 0 = explosionFrame' 0
explosionFrame 1 = explosionFrame' 1
explosionFrame 2 = explosionFrame' 2
explosionFrame 3 = explosionFrame' 3
explosionFrame 4 = explosionFrame' 4
explosionFrame n = explosionFrame $ n `mod` 5

-- | Returns the nth frame of the explosion animation.
--
--   Does not check if the frame is valid.
explosionFrame' :: Int -> Picture
explosionFrame' n = maybe blank (scale assetScale assetScale . bitmapSection (Rectangle (n * 16, 0) (16, 16))) explosionSheet

explosionAnimation :: Animation
explosionAnimation = Animation 5 2 0 0 explosionFrame


-- | Ship spritesheet (player)
shipSheet :: BitmapData
shipSheet = case loadPNG $(embedFile "assets/images/spritesheets/ship.png") of
  Just bdata -> bdata
  Nothing    -> error "shipSheet: Could not load spritesheet"

-- | Returns the ship sprite at the given column and row.
ship :: Int -> Int -> Picture
ship c r = rotate 90 $ scale assetScale assetScale $ bitmapSection (Rectangle (c * 16, r * 24) (16, 24)) shipSheet

-- | Returns the ship sprite for the given player facing.
fromPlayerFacing :: PlayerFacing -> ShipFrame -> Picture
fromPlayerFacing FacingLeftLeft    First  = ship 0 0
fromPlayerFacing FacingLeftLeft    Second = ship 0 1
fromPlayerFacing FacingLeft        First  = ship 1 0
fromPlayerFacing FacingLeft        Second = ship 1 1
fromPlayerFacing FacingNormal      First  = ship 2 0
fromPlayerFacing FacingNormal      Second = ship 2 1
fromPlayerFacing FacingRight       First  = ship 3 0
fromPlayerFacing FacingRight       Second = ship 3 1
fromPlayerFacing FacingRightRight  First  = ship 4 0
fromPlayerFacing FacingRightRight  Second = ship 4 1

-- | Missile spritesheet. Contains two sprites.
missileSheet :: BitmapData
missileSheet = case loadPNG $(embedFile "assets/images/spritesheets/missile.png") of
  Just bdata -> bdata
  Nothing    -> error "missileSheet: Could not load spritesheet"

-- | First frame of the missile projectile.
missile1 :: Picture
missile1 = scale assetScale assetScale $ bitmapSection (Rectangle (0, 0) (72, 19)) missileSheet

-- | Second frame of the missile projectile.
missile2 :: Picture
missile2 = scale assetScale assetScale $ bitmapSection (Rectangle (0, 19) (72, 19)) missileSheet

-- Background
shadows :: Picture
shadows = case loadPNG $(embedFile "assets/images/background/shadows.png") of
  Just bdata -> bitmap bdata
  Nothing    -> error "shadows: Could not load background"

stars :: Int -> Picture
stars f = case loadPNG $(embedFile "assets/images/background/stars.png") of
  Just bdata -> bitmapSection (Rectangle (f * 640, 0) (640, 360)) bdata
  Nothing    -> error "stars: Could not load background"

bigStars :: Int -> Picture
bigStars f = case loadPNG $(embedFile "assets/images/background/bigStars.png") of
  Just bdata -> bitmapSection (Rectangle (f * 640, 0) (640, 360)) bdata
  Nothing    -> error "stars: Could not load background"

blueStar :: Int -> Picture
blueStar f = case loadPNG $(embedFile "assets/images/background/blueStar.png") of
  Just bdata -> bitmapSection (Rectangle (f * 640, 0) (640, 360)) bdata
  Nothing    -> error "blueStar: Could not load background"

redStar :: Int -> Picture
redStar f = case loadPNG $(embedFile "assets/images/background/redStar.png") of
  Just bdata -> bitmapSection (Rectangle (f * 640, 0) (640, 360)) bdata
  Nothing    -> error "redStar: Could not load background"

blackHole :: Int -> Picture
blackHole f = case loadPNG $(embedFile "assets/images/background/blackHole.png") of
  Just bdata -> bitmapSection (Rectangle (f * 640, 0) (640, 360)) bdata
  Nothing    -> error "blackHole: Could not load background"

smallRotaryStar :: Int -> Picture
smallRotaryStar f = case loadPNG $(embedFile "assets/images/background/smallRotaryStar.png") of
  Just bdata -> bitmapSection (Rectangle (f * 640, 0) (640, 360)) bdata
  Nothing    -> error "smallRotaryStar: Could not load background"

rotaryStar :: Int -> Picture
rotaryStar f = case loadPNG $(embedFile "assets/images/background/rotaryStar.png") of
  Just bdata -> bitmapSection (Rectangle (f * 640, 0) (640, 360)) bdata
  Nothing    -> error "rotaryStar: Could not load background"

-- SOUNDS

-- Samples can be unpacked with unsafePerformIO just fine.
-- They are only packed in IOs because the code that loads them is foreign.
-- | Background music
bgMusic :: Sample
bgMusic = unsafePerformIO $ sampleFromMemoryOgg $(embedFile "assets/sounds/bg.ogg") 1
{-# NOINLINE bgMusic #-}

-- | Laser sound
--
--   Not used anymore.
--
--   Source: https://pixabay.com/sound-effects/blaster-2-81267/
laserOld :: Sample
laserOld = unsafePerformIO $ sampleFromMemoryMp3 $(embedFile "assets/sounds/laserOld.mp3") 1
{-# NOINLINE laserOld #-}

-- | Laser sound 1
--
--   Voiced by Tygo
laser1 :: Sample
laser1 = unsafePerformIO $ sampleFromMemoryOgg $(embedFile "assets/sounds/laser1.ogg") 1
{-# NOINLINE laser1 #-}

-- | Laser sound 2
--
--   Voiced by Tygo
laser2 :: Sample
laser2 = unsafePerformIO $ sampleFromMemoryOgg $(embedFile "assets/sounds/laser2.ogg") 1
{-# NOINLINE laser2 #-}

-- | Explosion sound 1
--
--   Voiced by Tygo
explosion1 :: Sample
explosion1 = unsafePerformIO $ sampleFromMemoryOgg $(embedFile "assets/sounds/explosion1.ogg") 1
{-# NOINLINE explosion1 #-}

-- | Explosion sound 2
--
--   Voiced by Tygo
explosion2 :: Sample
explosion2 = unsafePerformIO $ sampleFromMemoryOgg $(embedFile "assets/sounds/explosion2.ogg") 1
{-# NOINLINE explosion2 #-}
