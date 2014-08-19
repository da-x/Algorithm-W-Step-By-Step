{-# LANGUAGE DeriveGeneric #-}
module Lamdu.Expr.TypeVars
  ( TypeVars(..)
  , HasVar(..), CompositeHasVar(..)
  , newVar
  , difference
  ) where

import Control.DeepSeq (NFData(..))
import Control.DeepSeq.Generics (genericRnf)
import Data.Binary (Binary)
import Data.Monoid (Monoid(..))
import Data.Set (Set)
import GHC.Generics (Generic)
import Lamdu.Expr.Type (Type)
import qualified Data.Set as Set
import qualified Lamdu.Expr.Type as T

data TypeVars = TypeVars (Set (T.Var Type)) (Set T.ProductVar)
  deriving (Eq, Generic, Show)
instance NFData TypeVars where
  rnf = genericRnf
instance Monoid TypeVars where
  mempty = TypeVars mempty mempty
  mappend (TypeVars t0 r0) (TypeVars t1 r1) =
    TypeVars (mappend t0 t1) (mappend r0 r1)

instance Binary TypeVars

difference :: TypeVars -> TypeVars -> TypeVars
difference (TypeVars t0 r0) (TypeVars t1 r1) =
  TypeVars (Set.difference t0 t1) (Set.difference r0 r1)

class HasVar t where
  getVars :: TypeVars -> Set (T.Var t)
  newVars :: Set (T.Var t) -> TypeVars

newVar :: HasVar t => T.Var t -> TypeVars
newVar = newVars . Set.singleton

instance HasVar Type where
  getVars (TypeVars vs _) = vs
  newVars vs = TypeVars vs mempty

class CompositeHasVar p where
  compositeGetVars :: TypeVars -> Set (T.Var (T.Composite p))
  compositeNewVars :: Set (T.Var (T.Composite p)) -> TypeVars

instance CompositeHasVar T.Product where
  compositeGetVars (TypeVars _ vs) = vs
  compositeNewVars = TypeVars mempty

instance CompositeHasVar p => HasVar (T.Composite p) where
  getVars = compositeGetVars
  newVars = compositeNewVars
