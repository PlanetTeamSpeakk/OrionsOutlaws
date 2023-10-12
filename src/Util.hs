module Util where

import Model (GameState (GameState), boardSize, bombs)
import Data.BitVector (BitVector, zeros, (@.), setBit)
import System.Random (randomIO)
import Data.Ix (range)

-- | Converts center-based coordinates to top-left-based coordinates.
toNaturalCoord :: GameState -> (Float, Float) -> (Int, Int)
toNaturalCoord gstate (x, y) = let (width, height) = getWindowSizeFor gstate in
  (floor x + (width `div` 2), negate (floor y) + (height `div` 2))

-- | Converts top-left-based coordinates to center-based coordinates
toGlossCoord :: GameState -> (Int, Int) -> (Float, Float)
toGlossCoord gstate (x, y) = let (width, height) = getWindowSizeFor gstate in
  (fromIntegral (x + width `div` 2), fromIntegral (y - height `div` 2))

-- | Converts center-based coordinates to a cell index
toCell :: GameState -> (Float, Float) -> (Int, Int)
toCell gstate p = let (nx, ny) = toNaturalCoord gstate p in (nx `div` cellSize, ny `div` cellSize)

-- | Calculates the index of the BitVectors where the information regarding the given cell is stored
cellIndex :: GameState -> (Int, Int) -> Int
cellIndex gstate (x, y) = let (bWidth, _) = boardSize gstate in y * bWidth + x

-- | Calculates the required window size for the given state
getWindowSizeFor :: GameState -> (Int, Int)
getWindowSizeFor gstate = let (bWidth, bHeight) = boardSize gstate in (bWidth * cellSize, bHeight * cellSize)

-- | The size of a single cell in pixels
cellSize :: Int
cellSize = 16

isBomb :: GameState -> (Int, Int) -> Bool
isBomb gstate cell = bombs gstate @. cellIndex gstate cell

distributeBombs :: Int -> Int -> Int -> IO BitVector
distributeBombs width height bombCount = do
  let size = width * height
  distributeBombs' size (zeros size) bombCount
  where
    distributeBombs' :: Int -> BitVector -> Int -> IO BitVector
    distributeBombs' _ bv 0 = return bv
    distributeBombs' size bv bombsLeft = do
      r <- randomIO
      let i = abs r `mod` size
      let skip = bv @. i
      distributeBombs' size (if skip then bv else setBit bv i) (if skip then bombsLeft else bombsLeft - 1)

createBoard :: Int -> Int -> Int -> IO GameState
createBoard width height bombCount = do
  b <- distributeBombs width height bombCount
  return $ GameState (width, height) bombCount b (zeros (width * height)) False 0

printBoard :: GameState -> String
printBoard (GameState (width, height) _ b _ _ _) = unlines (map printLine (range (0, height - 1)))
  where
    printLine row = printCells (row * width) width
    printCells _ 0 = ""
    printCells i count = (if b @. i then '0' else '.') : printCells (i + 1) (count - 1)