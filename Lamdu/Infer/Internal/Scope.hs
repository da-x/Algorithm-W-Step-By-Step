{-# LANGUAGE DeriveGeneric #-}
module Lamdu.Infer.Internal.Scope
  ( Scope, emptyScope
  , fromTypeMap
  , insertTypeOf
  , lookupTypeOf
  ) where

import Control.DeepSeq (NFData(..))
import Control.DeepSeq.Generics (genericRnf)
import Data.Map (Map)
import Data.Monoid (Monoid(..))
import GHC.Generics (Generic)
import Lamdu.Infer.Internal.TypeVars (FreeTypeVars(..))
import qualified Data.Map as Map
import qualified Lamdu.Expr as E

newtype Scope = Scope { typeOfVar :: Map E.ValVar E.Type }
  deriving (Generic, Show)
instance NFData Scope where rnf = genericRnf
instance FreeTypeVars Scope where
    freeTypeVars (Scope env) =  mconcat $ map freeTypeVars $ Map.elems env
    applySubst s (Scope env) =  Scope $ Map.map (applySubst s) env

emptyScope :: Scope
emptyScope = Scope Map.empty

lookupTypeOf :: E.ValVar -> Scope -> Maybe E.Type
lookupTypeOf key = Map.lookup key . typeOfVar

insertTypeOf :: E.ValVar -> E.Type -> Scope -> Scope
insertTypeOf key scheme (Scope env) = Scope (Map.insert key scheme env)

fromTypeMap :: Map E.ValVar E.Type -> Scope
fromTypeMap = Scope
