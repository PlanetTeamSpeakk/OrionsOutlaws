module Game.OrionsOutlaws.Util.Registry
  ( Registry(..)
  , RegistryEntry(..)
  , getEntry
  , mkRegistry
  ) where
  
import Data.Map (Map, (!?), fromList)

-- | Holds entries with a key and has a name itself.
data Registry a = Registry
  { regKey     :: String                       -- ^ The name of the registry.
  , regEntries :: Map String (RegistryEntry a) -- ^ The entries of the registry.
  } deriving (Show, Eq)

-- | An entry in a registry.
data RegistryEntry a = RegistryEntry
  { entryKey   :: String -- ^ The key of the entry.
  , entryValue :: a      -- ^ The value of the entry.
  } deriving (Show, Eq)

instance Functor RegistryEntry where
  fmap f (RegistryEntry k v) = RegistryEntry k (f v)

-- | Get an entry from a registry by its key.
getEntry :: Registry a -> String -> Maybe (RegistryEntry a)
getEntry r k = regEntries r !? k

-- | Create a registry from a list of key-value pairs.
mkRegistry :: String -> [(String, a)] -> Registry a
mkRegistry rk = Registry rk . fromList . map (\(k, v) -> (k, RegistryEntry k v))
