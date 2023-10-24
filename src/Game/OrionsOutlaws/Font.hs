-- | Utility module for loading fonts created with Sprite Font Builder (https://www.johnwordsworth.com/projects/sprite-font-builder/)
module Game.OrionsOutlaws.Font 
  ( Font                 -- | The main font type, contains the glyphs and some metadata.
  , Glyph                -- | A single glyph, contains necessary information for rendering.
  , TextAlignment(..)    -- | The alignment of the text.
  , loadFont             -- | Load a font from a spritesheet and metadata file.
  , getGlyph             -- | Get the glyph for a given character.
  , renderString         -- | Render a string into a picture.
  , renderChar           -- | Render a single character into a picture.
  , stringWidth          -- | Get the width of a string in pixels when rendered with the given font.
  , charWidth            -- | Get the width of a character in pixels when rendered with the given font.
  , renderStringCentered -- | Renders a string into a picture, centered around the center of the text.
  ) where

import Data.Map (Map, fromList, (!), lookup, member)
import Data.ByteString (ByteString, fromStrict)
import Data.List (isPrefixOf)
import Graphics.Gloss.Data.Bitmap (BitmapData, bitmapDataOfBMP)
import Codec.BMP (parseBMP)
import Data.List.Split (splitOn)
import Graphics.Gloss.Data.Picture (Picture, translate, bitmapSection)
import Graphics.Gloss (pictures, Rectangle (Rectangle))

data Font = Font
  { fontSheet  :: BitmapData
  , fontGlyphs :: Map Char Glyph
  } deriving (Eq, Show)

data Glyph = Glyph
  { glyphWidth   :: Int
  , glyphHeight  :: Int
  , glyphPos     :: (Int, Int)
  , glyphOffset  :: (Int, Int)
  , glyphAdvance :: Int
  } deriving (Eq, Show)

data TextAlignment = LeftToRight | RightToLeft deriving (Show, Eq)

loadFont :: String -> ByteString -> Font
loadFont metadata spritesheet = case parseBMP $ fromStrict spritesheet of
  Left err -> error $ show err -- Couldn't parse font spritesheet
  Right bmp -> Font (bitmapDataOfBMP bmp) $ ensureHasSpace parseMetadata
  where
    -- | Ensure that the font has a space glyph, otherwise throw an error.
    --   When creating a font map using Sprite Font Builder, this should always be included.
    ensureHasSpace :: Map Char Glyph -> Map Char Glyph
    ensureHasSpace glyphs = if member ' ' glyphs then glyphs else error "Font does not contain space glyph"

    -- | Parse the font metadata into a map of chars to glyphs.
    parseMetadata :: Map Char Glyph
    parseMetadata = fromList $ map parseGlyph $ filter isCharLine $ lines metadata

    -- | Whether the given line is a character information line.
    isCharLine :: String -> Bool
    isCharLine line = "char " `isPrefixOf` line

    -- | Parse a line of metadata into a Glyph
    parseGlyph :: String -> (Char, Glyph)
    parseGlyph line =
      -- The letter is encapsulated in double quotes, use init $ tail to remove em.
      (let l = init $ tail $ props ! "letter" in if l == "space" then ' ' else head l, 
        Glyph 
          (intProp "width")                  -- Glyph width
          (intProp "height")                 -- Glyph height
          (intProp "x", intProp "y")  -- Glyph position in sheet
          (intProp "xoffset", intProp "yoffset") -- Glyph offset for rendering`
          (intProp "xadvance"))              -- Glyph "width" when rendering
      where
        props = parseLine line

        intProp :: String -> Int
        intProp p = read $ props ! p
    
    -- | Parse a line of metadata into a map of properties.
    parseLine :: String -> Map String String
    parseLine line = fromList $ map (\s -> let l = splitOn "=" s in (head l, head $ tail l)) $ filter (elem '=') $ words line

-- | Get the glyph for the given character.
getGlyph :: Font -> Char -> Glyph
getGlyph font c = case Data.Map.lookup c $ fontGlyphs font of
  Just g  -> g
  Nothing -> getGlyph font ' ' -- Default to space if the character is not found

-- | Get the width of a string in pixels when rendered with the given font.
stringWidth :: Font -> String -> Int
stringWidth font str = sum $ map (glyphAdvance . getGlyph font) str

charWidth :: Font -> Char -> Int
charWidth font c = glyphAdvance $ getGlyph font c

-- | Renders a string into a picture.
--   The string can be rendered either from left to right or from right to left.
--   The characters themselves will always be rendered in the same order, but the
--   direction in which the rendering is done and which character is considered
--   the origin are different.
renderString :: TextAlignment -> Font -> String -> Picture
renderString LeftToRight f s = renderString' f   1  s id
renderString RightToLeft f s = renderString' f (-1) s reverse

renderString' :: Font -> Float -> String -> ([Glyph] -> [Glyph])  -> Picture
renderString' font m s f = let glyphs = map (getGlyph font) s in 
  pictures $ snd $ foldr renderGlyph' (0, []) (f glyphs)
  where
    renderGlyph' :: Glyph -> (Float, [Picture]) -> (Float, [Picture])
    renderGlyph' g (offset, ps) = let glyphOffset = m * fromIntegral (glyphAdvance g) in 
      (offset + glyphOffset, translate offset 0 (renderGlyph (fontSheet font) g) : ps)

-- | Renders a single character into a picture.
--   Converts the character to a Glyph and renders it with renderGlyph.
renderChar :: Font -> Char -> Picture
renderChar f c = renderGlyph (fontSheet f) $ getGlyph f c

-- | Renders a Glyph into a picture using a spritesheet.
renderGlyph :: BitmapData -> Glyph -> Picture
renderGlyph sheet g = bitmapSection (Rectangle (glyphPos g) (glyphWidth g, glyphHeight g)) sheet

-- | Renders a string into a picture, centered around the center of the text.
renderStringCentered :: Font -> String -> Picture
-- Text is centered around the center of the first character, 
-- so we have to offset it by half the width of the first character
renderStringCentered f s = translate ((-fromIntegral (stringWidth f s) + fromIntegral (charWidth f $ head s)) / 2) 0 $ 
  renderString LeftToRight f s
