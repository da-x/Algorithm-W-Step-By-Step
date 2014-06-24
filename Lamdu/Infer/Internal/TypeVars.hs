module Lamdu.Infer.Internal.TypeVars
  ( TypeVars(..)
  , Occurs(..)
  , difference
  ) where

import Data.Monoid (Monoid(..))
import Data.Set (Set)
import qualified Data.Set as Set
import qualified Lamdu.Expr as E

data TypeVars = TypeVars (Set E.TypeVar) (Set E.RecordTypeVar)
instance Monoid TypeVars where
  mempty = TypeVars mempty mempty
  mappend (TypeVars t0 r0) (TypeVars t1 r1) =
    TypeVars (mappend t0 t1) (mappend r0 r1)

class Occurs v where
  occurs :: v -> TypeVars -> Bool

instance Occurs E.TypeVar where
  occurs v (TypeVars vs _) = Set.member v vs

instance Occurs E.RecordTypeVar where
  occurs v (TypeVars _ vs) = Set.member v vs

difference :: TypeVars -> TypeVars -> TypeVars
difference (TypeVars t0 r0) (TypeVars t1 r1) =
  TypeVars (Set.difference t0 t1) (Set.difference r0 r1)
