module Lamdu.Infer.Scope
  ( Scope, empty
  , fromTypeMap
  , insertTypeOf
  , lookupTypeOf
  ) where

import Lamdu.Infer.Internal.FreeTypeVars
import Lamdu.Infer.Scheme (Scheme)
import qualified Data.Map as Map
import qualified Data.Set as Set

newtype Scope = Scope { typeOfVar :: Map.Map String Scheme }
instance FreeTypeVars Scope where
    freeTypeVars (Scope env) =  Set.unions $ map freeTypeVars $ Map.elems env
    applySubst s (Scope env)      =  Scope $ Map.map (applySubst s) env

empty :: Scope
empty = Scope Map.empty

lookupTypeOf :: String -> Scope -> Maybe Scheme
lookupTypeOf key = Map.lookup key . typeOfVar

insertTypeOf :: String -> Scheme -> Scope -> Scope
insertTypeOf key scheme (Scope env) = Scope (Map.insert key scheme env)

fromTypeMap :: Map.Map String Scheme -> Scope
fromTypeMap = Scope
