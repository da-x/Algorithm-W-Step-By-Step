module Scope
  ( Scope, empty
  , fromTypeMap
  , insertTypeOf
  , lookupTypeOf
  ) where

import Expr (Scheme)
import TVs
import qualified Data.Map as Map

newtype Scope = Scope { typeOfVar :: Map.Map String Scheme }
instance TVs Scope where
    ftv (Scope env)      =  ftv (Map.elems env)
    apply s (Scope env)  =  Scope (Map.map (apply s) env)

empty :: Scope
empty = Scope Map.empty

lookupTypeOf :: String -> Scope -> Maybe Scheme
lookupTypeOf key = Map.lookup key . typeOfVar

insertTypeOf :: String -> Scheme -> Scope -> Scope
insertTypeOf key scheme (Scope env) = Scope (Map.insert key scheme env)

fromTypeMap :: Map.Map String Scheme -> Scope
fromTypeMap = Scope
