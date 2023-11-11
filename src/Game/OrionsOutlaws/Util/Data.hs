{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-orphans #-} -- Gloss didn't define them, so we do it.
-- | Handles loading and writing of settings, scores and any other data.
module Game.OrionsOutlaws.Util.Data 
  ( loadSettings
  , writeSettings
  , loadScores
  , writeScores
  , loadGameState
  , writeGameState
  ) where

import Game.OrionsOutlaws.Model (Settings(..), defaultSettings, Score, defLog, GameState(..), Player,
  Movement, MovementDirection, Enemy, PositionedAnimation(..), Projectile, Friendliness, Bounds, Position)
import Graphics.Gloss.Interface.IO.Game (Key, MouseButton, SpecialKey)
import Data.Aeson                       (FromJSON, ToJSON, Options (fieldLabelModifier), Value (..), (.:), object, KeyValue ((.=)))
import Data.Aeson.TH                    (deriveJSON, defaultOptions)
import Data.Aeson.Decoding              (eitherDecodeStrict)
import Data.Aeson.Encode.Pretty         (encodePretty)
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString as B
import Data.List                        (sortBy)
import Data.Functor                     ((<&>))
import Data.Ord                         (comparing, Down (Down))
import Control.Exception                (try)
import System.Log.Logger                (warningM)
import Data.Aeson.Types                 (FromJSON (..), ToJSON (..))
import Game.OrionsOutlaws.Util.Registry (getEntry, RegistryEntry (entryKey))
import Data.Maybe                       (fromJust)
import {-# SOURCE #-} Game.OrionsOutlaws.Util.Registries (animationsRegistry)

-- Define JSON instances for the types used by Settings and Settings itself.
$(deriveJSON defaultOptions ''SpecialKey)
$(deriveJSON defaultOptions ''MouseButton)
$(deriveJSON defaultOptions ''Key)
$(deriveJSON defaultOptions ''Settings)
$(deriveJSON defaultOptions { fieldLabelModifier = drop 5 } ''Score)
$(deriveJSON defaultOptions ''MovementDirection)
$(deriveJSON defaultOptions ''Movement)
$(deriveJSON defaultOptions ''Player)
$(deriveJSON defaultOptions ''Enemy)
$(deriveJSON defaultOptions ''Friendliness)
$(deriveJSON defaultOptions ''Projectile)

instance FromJSON PositionedAnimation where
  parseJSON (Object v) = mkPosAnim
    <$> v .: "animation"
    <*> v .: "curFrame"
    <*> v .: "animationStep"
    <*> v .: "animationPos"
    where
      mkPosAnim :: String -> Int -> Int -> Position -> PositionedAnimation
      mkPosAnim a = PositionedAnimation (fromJust (getEntry animationsRegistry a))
  parseJSON _ = error "parseJSON PositionedAnimation: Expected Object, but got something else"

instance ToJSON PositionedAnimation where
  toJSON pa = object
    [ "animation"     .= entryKey (animation pa)
    , "curFrame"      .= curFrame pa
    , "animationStep" .= animationStep pa
    , "animationPos"  .= animationPos pa
    ]

instance FromJSON GameState where
  parseJSON (Object v) = mkGstate
    <$> v .: "player"
    <*> v .: "enemies"
    <*> v .: "projectiles"
    <*> v .: "animations"
    <*> v .: "score"
    <*> v .: "lastSpawn"
    <*> v .: "elapsedTime"
    <*> v .: "windowSize"
    <*> v .: "mousePos"
    <*> v .: "steps"
    <*> v .: "lastStep"
    <*> v .: "settings"
    <*> v .: "scores"
    where
      mkGstate :: Player -> [Enemy] -> [Projectile] -> [PositionedAnimation] -> Int -> Float -> Float ->
        Bounds -> Maybe Position -> Integer -> Integer -> Settings -> [Score] -> GameState
      mkGstate p es ps as s ls et ws mp st lst stt scs = GameState p es ps as Nothing s ls et ws mp st lst stt scs [] False
  parseJSON _ = error "parseJSON GameState: Expected Object, but got something else"

instance ToJSON GameState where
  toJSON gs = object
    [ "player"       .= player gs
    , "enemies"      .= enemies gs
    , "projectiles"  .= projectiles gs
    , "animations"   .= animations gs
    , "score"        .= score gs
    , "lastSpawn"    .= lastSpawn gs
    , "elapsedTime"  .= elapsedTime gs
    , "windowSize"   .= windowSize gs
    , "mousePos"     .= mousePos gs
    , "steps"        .= steps gs
    , "lastStep"     .= lastStep gs
    , "settings"     .= settings gs
    , "scores"       .= scores gs
    ]

newtype Const a = Const a
instance Functor Const where
  fmap f (Const x) = Const $ f x

-- instance ToJSON GameState where
  -- toJSON _ = error "GameState is not a JSON type."

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

loadGameState :: IO (Maybe GameState)
loadGameState = loadSafely "gamestate.json" Nothing

writeGameState :: GameState -> IO ()
writeGameState = writeJson "gamestate.json"

-- | Load a file safely. If the file could not be read, writes and returns the fallback.
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
