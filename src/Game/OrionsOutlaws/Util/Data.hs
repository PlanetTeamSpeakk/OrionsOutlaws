{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-orphans #-} -- Gloss didn't define them, so we do it.
-- | Handles loading and writing of settings, scores and any other data.
module Game.OrionsOutlaws.Util.Data (loadSettings, writeSettings, loadScores, writeScores) where

import Game.OrionsOutlaws.Model (Settings (..), defaultSettings, Score, defLog)
import Data.Aeson (FromJSON, ToJSON, Options (fieldLabelModifier))
import Data.Aeson.TH (deriveJSON, defaultOptions)
import Data.Aeson.Decoding (eitherDecodeStrict)
import Data.Aeson.Encode.Pretty (encodePretty)
import Graphics.Gloss.Interface.IO.Game (Key, MouseButton, SpecialKey)
import Control.Exception (try)
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString as B
import System.Log.Logger (warningM)
import Data.List (sortBy)
import Data.Functor ((<&>))
import Data.Ord (comparing, Down (Down))

-- Define JSON instances for the types used by Settings and Settings itself.
$(deriveJSON defaultOptions ''SpecialKey)
$(deriveJSON defaultOptions ''MouseButton)
$(deriveJSON defaultOptions ''Key)
$(deriveJSON defaultOptions ''Settings)
$(deriveJSON defaultOptions { fieldLabelModifier = drop 5 } ''Score)

-- | Load the settings from the settings.json file.
loadSettings :: IO Settings
loadSettings = loadSafely "settings.json" defaultSettings

-- | Write the settings to the settings.json file.
writeSettings :: Settings -> IO ()
writeSettings = writeJson "settings.json"

-- | Load the scores from the scores.json file.
loadScores :: IO [Score]
loadScores = loadSafely "scores.json" [] <&> sortBy (comparing Down)

-- | Write the scores to the scores.json file.
writeScores :: [Score] -> IO ()
writeScores = writeJson "scores.json"

-- | Load a file safely. If the file could not be read, write the fallback
loadSafely :: (FromJSON a, ToJSON a) => FilePath -> a -> IO a
loadSafely file fallback = do
  -- Attempt to read the file as a bytestring.
  res <- try (B.readFile file) :: IO (Either IOError B.ByteString)
  case res of
    -- If the file could not be read, write and return fallback.
    Left e -> do
      warningM defLog $ "Could not read " ++ file ++ ": " ++ show e
      writeJson file fallback -- Write fallback
      return fallback

    -- If the file could be read, try to parse it.
    Right content -> case eitherDecodeStrict content of
      -- If the content could not be parsed, return fallback.
      Left e -> do
        warningM defLog $ "Could not parse " ++ file ++ ": " ++ e
        writeJson file fallback -- Write fallback
        return fallback

      -- If the content could be parsed, return it.
      Right s -> return s

-- | Writes a datastructure to a file in JSON format.
writeJson :: (ToJSON a) => FilePath -> a -> IO ()
writeJson file = BL.writeFile file . encodePretty
