module Lamdu.Infer.Scope
  ( Scope, empty
  , fromTypeMap
  , insertTypeOf
  , lookupTypeOf
  ) where

import Data.Map (Map)
import Lamdu.Infer.Internal.FreeTypeVars (FreeTypeVars(..))
import Lamdu.Infer.Scheme (Scheme)
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Lamdu.Expr as E

newtype Scope = Scope { typeOfVar :: Map E.ValVar Scheme }
instance FreeTypeVars Scope where
    freeTypeVars (Scope env) =  Set.unions $ map freeTypeVars $ Map.elems env
    applySubst s (Scope env)      =  Scope $ Map.map (applySubst s) env

empty :: Scope
empty = Scope Map.empty

lookupTypeOf :: E.ValVar -> Scope -> Maybe Scheme
lookupTypeOf key = Map.lookup key . typeOfVar

insertTypeOf :: E.ValVar -> Scheme -> Scope -> Scope
insertTypeOf key scheme (Scope env) = Scope (Map.insert key scheme env)

fromTypeMap :: Map E.ValVar Scheme -> Scope
fromTypeMap = Scope
