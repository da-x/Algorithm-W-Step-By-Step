module Lamdu.Infer.Internal.Scope
  ( Scope, empty
  , fromTypeMap
  , insertTypeOf
  , lookupTypeOf
  ) where

import Data.Map (Map)
import Data.Monoid (Monoid(..))
import Lamdu.Infer.Internal.FreeTypeVars (FreeTypeVars(..))
import Lamdu.Infer.Internal.Scheme (Scheme)
import qualified Data.Map as Map
import qualified Lamdu.Expr as E

newtype Scope = Scope { typeOfVar :: Map E.ValVar Scheme }
instance FreeTypeVars Scope where
    freeTypeVars (Scope env) =  mconcat $ map freeTypeVars $ Map.elems env
    applySubst s (Scope env) =  Scope $ Map.map (applySubst s) env

empty :: Scope
empty = Scope Map.empty

lookupTypeOf :: E.ValVar -> Scope -> Maybe Scheme
lookupTypeOf key = Map.lookup key . typeOfVar

insertTypeOf :: E.ValVar -> Scheme -> Scope -> Scope
insertTypeOf key scheme (Scope env) = Scope (Map.insert key scheme env)

fromTypeMap :: Map E.ValVar Scheme -> Scope
fromTypeMap = Scope
