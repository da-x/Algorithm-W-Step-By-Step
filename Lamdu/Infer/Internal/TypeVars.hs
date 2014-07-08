module Lamdu.Infer.Internal.TypeVars
  ( TypeVars(..)
  , Var(..)
  , difference
  ) where

import Data.Monoid (Monoid(..))
import Data.Set (Set)
import qualified Data.Set as Set
import qualified Lamdu.Expr as E

data TypeVars = TypeVars (Set E.TypeVar) (Set E.RecordTypeVar)
  deriving (Eq)
instance Monoid TypeVars where
  mempty = TypeVars mempty mempty
  mappend (TypeVars t0 r0) (TypeVars t1 r1) =
    TypeVars (mappend t0 t1) (mappend r0 r1)

class Var v where
  getVars :: TypeVars -> Set v

instance Var E.TypeVar where
  getVars (TypeVars vs _) = vs

instance Var E.RecordTypeVar where
  getVars (TypeVars _ vs) = vs

difference :: TypeVars -> TypeVars -> TypeVars
difference (TypeVars t0 r0) (TypeVars t1 r1) =
  TypeVars (Set.difference t0 t1) (Set.difference r0 r1)
