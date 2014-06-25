module Lamdu.Infer.Internal.FreeTypeVars
  ( Subst(..), substDelete
  , FreeTypeVars(..)
  ) where

import Data.Map (Map)
import Data.Maybe (fromMaybe)
import Data.Monoid (Monoid(..))
import Data.Set (Set)
import Lamdu.Infer.TypeVars (TypeVars(..))
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Lamdu.Expr as E

data Subst = Subst
  { substTypes :: Map E.TypeVar E.Type
  , substRecordTypes :: Map E.RecordTypeVar E.RecordType
  }

instance Monoid Subst where
  mempty = Subst Map.empty Map.empty
  mappend (Subst t0 r0) s1@(Subst t1 r1) =
    Subst
    (t1 `Map.union` (Map.map (applySubst s1) t0))
    (r1 `Map.union` (Map.map (applySubst s1) r0))

mapSetDifference :: Ord k => Set k -> Map k v -> Map k v
mapSetDifference s m = Set.foldr Map.delete m s

substDelete :: TypeVars -> Subst -> Subst
substDelete (TypeVars t r) (Subst st sr) =
  Subst (mapSetDifference t st) (mapSetDifference r sr)

class FreeTypeVars a where
  freeTypeVars :: a -> TypeVars
  applySubst   :: Subst -> a -> a

instance FreeTypeVars E.RecordType where
  freeTypeVars E.TRecEmpty          = mempty
  freeTypeVars (E.TRecVar n)        = TypeVars mempty (Set.singleton n)
  freeTypeVars (E.TRecExtend _ t r) = freeTypeVars t `mappend` freeTypeVars r

  applySubst _ E.TRecEmpty          = E.TRecEmpty
  applySubst s (E.TRecVar n)        = fromMaybe (E.TRecVar n) $ Map.lookup n (substRecordTypes s)
  applySubst s (E.TRecExtend n t r) = E.TRecExtend n (applySubst s t) (applySubst s r)

instance FreeTypeVars E.Type where
  freeTypeVars (E.TVar n)      =  TypeVars (Set.singleton n) mempty
  freeTypeVars (E.TCon _)      =  mempty
  freeTypeVars (E.TFun t1 t2)  =  freeTypeVars t1 `mappend` freeTypeVars t2
  freeTypeVars (E.TApp t1 t2)  =  freeTypeVars t1 `mappend` freeTypeVars t2
  freeTypeVars (E.TRecord r)   =  freeTypeVars r

  applySubst s (E.TVar n)      = fromMaybe (E.TVar n) $ Map.lookup n (substTypes s)
  applySubst s (E.TFun t1 t2)  = E.TFun (applySubst s t1) (applySubst s t2)
  applySubst s (E.TApp t1 t2)  = E.TApp (applySubst s t1) (applySubst s t2)
  applySubst _ (E.TCon t)      = E.TCon t
  applySubst s (E.TRecord r)   = E.TRecord $ applySubst s r
