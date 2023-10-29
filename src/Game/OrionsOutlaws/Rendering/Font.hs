-- | Utility module for loading fonts created with Sprite Font Builder (https://www.johnwordsworth.com/projects/sprite-font-builder/)
module Game.OrionsOutlaws.Rendering.Font
  ( Font (..)            -- | The main font type, contains the glyphs and some metadata.
  , Glyph (..)           -- | A single glyph, contains necessary information for rendering.
  , TextAlignment (..)   -- | The alignment of the text.
  , loadFont             -- | Load a font from a spritesheet and metadata file.
  , getGlyph             -- | Get the glyph for a given character.
  , renderString         -- | Render a string into a picture.
  , renderChar           -- | Render a single character into a picture.
  , stringWidth          -- | Get the width of a string in pixels when rendered with the given font.
  , charWidth            -- | Get the width of a character in pixels when rendered with the given font.
  , renderStringCentered -- | Renders a string into a picture, centered around the center of the text.
  ) where

import Data.Map (Map, fromList, (!), lookup, member)
import Data.List (isPrefixOf, intercalate)
import Graphics.Gloss.Data.Bitmap (BitmapData)
import Data.List.Split (splitOn)
import Graphics.Gloss.Data.Picture (Picture, translate, bitmapSection, blank)
import Graphics.Gloss (pictures, Rectangle (Rectangle))
import Data.Bifunctor (first)

data Font = Font
  { fontSheet      :: BitmapData     -- ^ The spritesheet containing all glyphs.
  , fontGlyphs     :: Map Char Glyph -- ^ A map of characters to glyphs.
  , fontLineHeight :: Int            -- ^ The height of a line of text in pixels.
  } deriving (Eq, Show)

data Glyph = Glyph
  { glyphWidth   :: Int        -- ^ The width of the glyph in the spritesheet in pixels.
  , glyphHeight  :: Int        -- ^ The height of the glyph in the spritesheet in pixels.
  , glyphPos     :: (Int, Int) -- ^ The position of the glyph in the spritesheet in pixels.
  , glyphOffset  :: (Int, Int) -- ^ The offset of the glyph when rendered in pixels.
  , glyphAdvance :: Int        -- ^ The width of the glyph when rendered in pixels. Determines the position of the next glyph.
  } deriving (Eq, Show)

data TextAlignment = LeftToRight | RightToLeft deriving (Show, Eq)

-- | Loads a font from metadata and a spritesheet.
loadFont :: String -> BitmapData -> Font
loadFont metadata spritesheet = uncurry (Font spritesheet) $ first ensureHasSpace parseMetadata
  where
    -- | Ensure that the font has a space glyph, otherwise throw an error.
    --   When creating a font map using Sprite Font Builder, this should always be included.
    ensureHasSpace :: Map Char Glyph -> Map Char Glyph
    ensureHasSpace glyphs = if member ' ' glyphs then glyphs else error "Font does not contain space glyph"

    -- | Parse the font metadata into a map of chars to glyphs.
    parseMetadata :: (Map Char Glyph, Int)
    parseMetadata = let l = lines metadata in
      (fromList $ map parseGlyph $ filter isCharLine l, read $ parseLine (head $ filter isCommonLine l) ! "lineHeight")

    -- | Whether the given line is a character information line.
    isCharLine :: String -> Bool
    isCharLine line = "char " `isPrefixOf` line

    -- | Whether the given line is a common information line.
    --   Contains information such as the line height.
    isCommonLine :: String -> Bool
    isCommonLine line = "common " `isPrefixOf` line

    -- | Parse a line of metadata into a Glyph
    parseGlyph :: String -> (Char, Glyph)
    parseGlyph line =
      -- The letter is encapsulated in double quotes, use init $ tail to remove em.
      (let l = init $ tail $ props ! "letter" in if l == "space" then ' ' else head l,
        Glyph
          (intProp "width")                      -- Glyph width
          (intProp "height")                     -- Glyph height
          (intProp "x", intProp "y")             -- Glyph position in sheet
          (intProp "xoffset", intProp "yoffset") -- Glyph offset when rendering
          (intProp "xadvance"))                  -- Glyph "width" when rendering
      where
        props = parseLine line

        intProp :: String -> Int
        intProp p = read $ props ! p

    -- | Parse a line of metadata into a map of properties.
    parseLine :: String -> Map String String
    parseLine line = fromList $ map parseProp $ filter (elem '=') $ words line
      where
        -- | Parse a property into a key-value pair.
        --   Ensures that values with an '=' in them are not split.
        parseProp :: String -> (String, String)
        parseProp prop = let l = splitOn "=" prop in (head l, intercalate "=" $ tail l)

-- | Get the glyph for the given character.
getGlyph :: Font -> Char -> Glyph
getGlyph font c = case Data.Map.lookup c $ fontGlyphs font of
  Just g  -> g
  Nothing -> getGlyph font ' ' -- Default to space if the character is not found

-- | Get the width of a string in pixels when rendered with the given font.
stringWidth :: Font -> String -> Int
stringWidth font str = sum $ map (glyphAdvance . getGlyph font) str

-- | Get the height of a string in pixels when rendered with the given font.
--   Not the same as 'fontLineHeight' as this is just the height of the tallest glyph.
stringHeight :: Font -> String -> Int
stringHeight font str = maximum $ map (glyphHeight . getGlyph font) str

-- | Get the width of a character in pixels when rendered with the given font.
charWidth :: Font -> Char -> Int
charWidth font c = glyphAdvance $ getGlyph font c

-- | Renders a string into a picture.
--   The string can be rendered either from left to right or from right to left.
--   The characters themselves will always be rendered in the same order, but the
--   direction in which the rendering is done and which character is considered
--   the origin are different.
--   Does not take line-breaks into account.
renderString :: TextAlignment -> Font -> String -> Picture
renderString LeftToRight f s = renderString' f   1  s reverse
renderString RightToLeft f s = renderString' f (-1) s id

-- | Actually renders a string into a picture.
--   Given a font, a horizontal multiplier a string and a function to transform the glyphs,
--   outputs a picture representing the given text.
renderString' :: Font -> Float -> String -> ([Glyph] -> [Glyph])  -> Picture
renderString' _    _ "" _ = blank
renderString' font m s  f = 
  let glyphs    = map (getGlyph font) s -- Translate all chars to glyphs
      fstGlyph  = head glyphs
      fstOffset = glyphOffset fstGlyph
  -- Render all glyphs and combine them into one picture
  in translate 
    -- We offset horizontally by the offset of the first glyph which effectively ignores the offset of the first glyph
    (-fromIntegral (fst fstOffset))
    (-halfHeight + (fromIntegral (stringHeight font s) / 2) + fromIntegral (snd fstOffset)) $ pictures $ snd $ foldr renderGlyph' 
    (0, []) $ f glyphs 
  where
    -- | Half the line height of the used font.
    --   Needed cuz everything in Gloss is centered around the origin.
    halfHeight = fromIntegral (fontLineHeight font) / 2

    -- | Renders a single glyph while keeping track of the horizontal offset.
    --   Also offsets a glyph by half its width so that fonts do not break when rendering
    --   characters that don't all have the same widths.
    renderGlyph' :: Glyph -> (Float, [Picture]) -> (Float, [Picture])
    renderGlyph' g (offset, ps) = (offset + (m * fromIntegral (glyphAdvance g)),
      translate
        (offset + (0.5 * m * fromIntegral (glyphWidth g)) + fromIntegral (fst $ glyphOffset g))  -- Horizontal offset
        (halfHeight - fromIntegral (snd $ glyphOffset g) - (0.5 * fromIntegral (glyphHeight g))) -- Vertical offset
        (renderGlyph (fontSheet font) g) : ps)

-- | Renders a single character into a picture.
--   Converts the character to a Glyph and renders it with renderGlyph.
renderChar :: Font -> Char -> Picture
renderChar f c = renderGlyph (fontSheet f) $ getGlyph f c

-- | Renders a Glyph into a picture using a spritesheet.
renderGlyph :: BitmapData -> Glyph -> Picture
renderGlyph sheet Glyph { glyphWidth = gw, glyphHeight = gh, glyphPos = (gx, gy) } =
  bitmapSection (Rectangle (gx, gy) (gw, gh)) sheet

-- | Renders a string into a picture, centered around the center of the text.
renderStringCentered :: Font -> String -> Picture
renderStringCentered _ "" = blank
renderStringCentered f s  = translate (-fromIntegral (stringWidth f s) / 2) 0 $ renderString LeftToRight f s
