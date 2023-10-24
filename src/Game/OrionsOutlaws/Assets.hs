{-# LANGUAGE TemplateHaskell #-}

-- | This module contains all the assets used in the game.
-- | Assets are packed using file-embed
module Game.OrionsOutlaws.Assets (
    assetScale,
    pixeboyFont,
    explosionAnimation,
    pauseOverlay,
    fromPlayerFacing,
    digit,
    bgMusic,
    laserOld, laser1, laser2,
    explosion1, explosion2
    ) where

import Graphics.Gloss
import Data.ByteString (ByteString, fromStrict)
import Data.FileEmbed -- Uses the magic of TemplateHaskell to turn files into bytestrings at compile time
import Codec.BMP (parseBMP)
import Game.OrionsOutlaws.Model (Animation(Animation), PlayerFacing(..), ShipFrame(..))
import Codec.Picture.Png (decodePng)
import Graphics.Gloss.Juicy (fromDynamicImage)
import Sound.ProteaAudio (Sample, sampleFromMemoryOgg, sampleFromMemoryMp3)
import System.IO.Unsafe (unsafePerformIO)
import Game.OrionsOutlaws.Font (Font, loadFont)
import Data.Maybe (fromMaybe)

-- Helper function for turning bytestrings into bitmapdata
loadBMPData :: ByteString -> BitmapData
loadBMPData bstr = case parseBMP $ fromStrict bstr of
  Left err  -> error $ show err
  Right bmp -> bitmapDataOfBMP bmp

-- | Attempts to load a ByteString representing a PNG into a Picture.
--   If the bytestring is not a valid PNG, returns a blank picture.
loadPNG :: ByteString -> Picture
loadPNG bstr = case decodePng bstr of
  Left _    -> blank
  Right pic -> fromMaybe blank (fromDynamicImage pic)

-- | The value assets are scaled by when displayed in the game.
assetScale :: Float
assetScale = 4

-- Pixeboy font
pixeboyFont :: Font
pixeboyFont = loadFont $(embedStringFile "assets/fonts/pixeboy.fnt") $ loadBMPData $(embedFile "assets/fonts/pixeboy.bmp")

-- IMAGES
--- Spritesheets

-- Explosion animation
-- Spritesheets have to be bitmaps so that we can use bitmapSection
explosionSheet :: BitmapData
explosionSheet = loadBMPData $(embedFile "assets/images/spritesheets/explosion.bmp")

explosionFrame :: Int -> Picture
explosionFrame 0 = explosionFrame' 0
explosionFrame 1 = explosionFrame' 1
explosionFrame 2 = explosionFrame' 2
explosionFrame 3 = explosionFrame' 3
explosionFrame 4 = explosionFrame' 4
explosionFrame n = explosionFrame $ n `mod` 5

explosionFrame' :: Int -> Picture
explosionFrame' n = scale assetScale assetScale $ bitmapSection (Rectangle (n * 16, 0) (16, 16)) explosionSheet

explosionAnimation :: Animation
explosionAnimation = Animation 5 2 0 0 explosionFrame


-- Ship (player)
shipSheet :: BitmapData
shipSheet = loadBMPData $(embedFile "assets/images/spritesheets/ship.bmp")

ship :: Int -> Int -> Picture
ship c r = rotate 90 $ scale assetScale assetScale $ bitmapSection (Rectangle (c * 16, r * 24) (16, 24)) shipSheet

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


-- Digits spritesheet
digitsSheet :: BitmapData
digitsSheet = loadBMPData $(embedFile "assets/images/spritesheets/digits.bmp")

digit :: Int -> Picture
digit d
    -- Each digit is exactly 61x73 pixels
    | d >= 0 && d <= 9 = bitmapSection (Rectangle (d * 61, 0) (61, 73)) digitsSheet
    | otherwise = error "Invalid digit" -- Should be impossible

--- Misc images
-- Pause overlay
pauseOverlay :: Picture
pauseOverlay = loadPNG $(embedFile "assets/images/pauseOverlay.png")


-- SOUNDS

-- Samples can be unpacked with unsafePerformIO just fine.
-- They are only packed in IOs because the code that loads them is foreign.
-- | Background music
bgMusic :: Sample
bgMusic = unsafePerformIO $ sampleFromMemoryOgg $(embedFile "assets/sounds/bg.ogg") 1
{-# NOINLINE bgMusic #-}

-- | Laser sound
--   Source: https://pixabay.com/sound-effects/blaster-2-81267/
laserOld :: Sample
laserOld = unsafePerformIO $ sampleFromMemoryMp3 $(embedFile "assets/sounds/laserOld.mp3") 1
{-# NOINLINE laserOld #-}

laser1 :: Sample
laser1 = unsafePerformIO $ sampleFromMemoryOgg $(embedFile "assets/sounds/laser1.ogg") 1
{-# NOINLINE laser1 #-}

laser2 :: Sample
laser2 = unsafePerformIO $ sampleFromMemoryOgg $(embedFile "assets/sounds/laser2.ogg") 1
{-# NOINLINE laser2 #-}

explosion1 :: Sample
explosion1 = unsafePerformIO $ sampleFromMemoryOgg $(embedFile "assets/sounds/explosion1.ogg") 1
{-# NOINLINE explosion1 #-}

explosion2 :: Sample
explosion2 = unsafePerformIO $ sampleFromMemoryOgg $(embedFile "assets/sounds/explosion2.ogg") 1
{-# NOINLINE explosion2 #-}
