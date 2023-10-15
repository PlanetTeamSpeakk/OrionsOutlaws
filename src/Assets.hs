{-# LANGUAGE TemplateHaskell #-}

-- | This module contains all the assets used in the game.
-- | Assets are packed using file-embed
module Assets (smiley, explosionAnimation, pauseOverlay) where

import Graphics.Gloss
import Data.ByteString (ByteString, fromStrict)
import Data.FileEmbed -- Uses the magic of TemplateHaskell to turn files into bytestrings at compile time
import Codec.BMP (parseBMP)
import Model (Animation (Animation))
import Codec.Picture.Png (decodePng)
import Graphics.Gloss.Juicy (fromDynamicImage)

-- Helper function for turning bytestrings into bitmapdata
loadBMPData :: ByteString -> BitmapData
loadBMPData bstr =
    case parseBMP $ fromStrict bstr of
        Left err -> error $ show err
        Right bmp -> bitmapDataOfBMP bmp

assetScale :: Float
assetScale = 5

smiley :: BitmapData
smiley = loadBMPData $(embedFile "assets/smiley.bmp")

-- Explosion animation
explosionSheet :: BitmapData
explosionSheet = loadBMPData $(embedFile "assets/explosion.bmp")

explosionFrame :: Int -> Picture
explosionFrame 0 = explosionFrame' 0
explosionFrame 1 = explosionFrame' 1
explosionFrame 2 = explosionFrame' 2
explosionFrame 3 = explosionFrame' 3
explosionFrame 4 = explosionFrame' 4
explosionFrame _ = explosionFrame 0

explosionFrame' :: Int -> Picture
explosionFrame' n = scale assetScale assetScale $ bitmapSection (Rectangle (n * 16, 0) (16, 16)) explosionSheet

explosionAnimation :: Animation
explosionAnimation = Animation 5 2 0 0 explosionFrame

pauseOverlay :: Picture
pauseOverlay = case decodePng $(embedFile "assets/pauseOverlay.png") of
    Left err -> error err
    Right pic -> case fromDynamicImage pic of
        Nothing -> error "Could not load pause overlay"
        Just p -> p
