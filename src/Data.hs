{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-orphans #-} -- Gloss didn't define them, so we do it.
module Data (loadSettings, writeSettings, loadScores, writeScores) where

import Model (Settings (..), defaultSettings, Score, debugLog)
import Data.Aeson (FromJSON, ToJSON)
import Data.Aeson.TH (deriveJSON, defaultOptions)
import Data.Aeson.Decoding (decodeStrict)
import Data.Aeson.Encode.Pretty (encodePretty)
import Graphics.Gloss.Interface.IO.Game (Key, MouseButton, SpecialKey)
import Data.Maybe (fromMaybe)
import Control.Exception (try)
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString as B
import System.Log.Logger (debugM)

-- Define JSON instances for the types used by Settings and Settings itself.
$(deriveJSON defaultOptions ''SpecialKey)
$(deriveJSON defaultOptions ''MouseButton)
$(deriveJSON defaultOptions ''Key)
$(deriveJSON defaultOptions ''Settings)
$(deriveJSON defaultOptions ''Score)

-- | Load the settings from the settings.json file.
loadSettings :: IO Settings
loadSettings = loadSafely "settings.json" defaultSettings

-- | Write the settings to the settings.json file.
writeSettings :: Settings -> IO ()
writeSettings = writeJson "settings.json"

-- | Load the scores from the scores.json file.
loadScores :: IO [Score]
loadScores = loadSafely "scores.json" []

-- | Write the scores to the scores.json file.
writeScores :: [Score] -> IO ()
writeScores = writeJson "scores.json"

-- | Load a file safely. If the file could not be read, write the fallback
loadSafely :: (FromJSON a, ToJSON a) => FilePath -> a -> IO a
loadSafely file fallback = do
    res <- try (B.readFile file) :: IO (Either IOError B.ByteString)
    case res of
        Left e -> do
            -- If the file could not be read, write and return fallback.
            debugM debugLog $ "Could not read " ++ file ++ ": " ++ show e
            writeJson file fallback -- Write fallback
            return fallback

        -- If the content could not be parsed, return fallback.
        Right content -> return $ fromMaybe fallback $ decodeStrict content

-- | Writes a datastructure to a file in JSON format.
writeJson :: (ToJSON a) => FilePath -> a -> IO ()
writeJson file = BL.writeFile file . encodePretty
