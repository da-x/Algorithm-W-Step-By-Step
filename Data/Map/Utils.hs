module Data.Map.Utils (deleteKeySet) where

import Data.Map (Map)
import Data.Set (Set)
import qualified Data.Map as Map
import qualified Data.Set as Set

deleteKeySet :: Ord k => Set k -> Map k v -> Map k v
deleteKeySet s m = Set.foldr Map.delete m s
