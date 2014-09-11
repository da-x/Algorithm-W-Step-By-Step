{-# LANGUAGE DeriveGeneric #-}
module Lamdu.Infer.Internal.Scope
  ( Scope, emptyScope
  , fromTypeMap, scopeToTypeMap
  , insertTypeOf
  , lookupTypeOf
  ) where

import Control.DeepSeq (NFData(..))
import Control.DeepSeq.Generics (genericRnf)
import Data.Binary (Binary)
import Data.Map (Map)
import Data.Monoid (Monoid(..))
import GHC.Generics (Generic)
import Lamdu.Expr.Type (Type)
import Lamdu.Infer.Internal.Subst (CanSubst(..))
import qualified Data.Map as Map
import qualified Lamdu.Expr.TypeVars as TypeVars
import qualified Lamdu.Expr.Val as V
import qualified Lamdu.Infer.Internal.Subst as Subst

newtype Scope = Scope { typeOfVar :: Map V.Var Type }
  deriving (Generic, Show)

instance NFData Scope where rnf = genericRnf
instance Binary Scope

instance TypeVars.Free Scope where
  free (Scope env) = mconcat $ map TypeVars.free $ Map.elems env

instance CanSubst Scope where
  apply s (Scope env) = Scope $ Map.map (Subst.apply s) env

emptyScope :: Scope
emptyScope = Scope Map.empty

lookupTypeOf :: V.Var -> Scope -> Maybe Type
lookupTypeOf key = Map.lookup key . typeOfVar

insertTypeOf :: V.Var -> Type -> Scope -> Scope
insertTypeOf key scheme (Scope env) = Scope (Map.insert key scheme env)

scopeToTypeMap :: Scope -> Map V.Var Type
scopeToTypeMap = typeOfVar

fromTypeMap :: Map V.Var Type -> Scope
fromTypeMap = Scope
