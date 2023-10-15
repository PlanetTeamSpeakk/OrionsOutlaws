{-# LANGUAGE TemplateHaskell #-}

-- | This module contains all the assets used in the game.
-- | Assets are packed using file-embed
module Assets (smiley) where

import Graphics.Gloss
import Data.ByteString (ByteString, fromStrict)
import Data.FileEmbed -- Uses the magic of TemplateHaskell to turn files into bytestrings at compile time
import Codec.BMP (parseBMP)

-- Helper function for turning bytestrings into bitmapdata
loadBMPData :: ByteString -> BitmapData
loadBMPData bstr =
    case parseBMP $ fromStrict bstr of
        Left err -> error $ show err
        Right bmp -> bitmapDataOfBMP bmp

smiley :: BitmapData
smiley = loadBMPData $(embedFile "assets/smiley.bmp")
