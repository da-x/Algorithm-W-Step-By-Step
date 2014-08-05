module Lamdu.Infer.Internal.Subst
  ( HasVar(..), CompositeHasVar
  , Subst(..), intersect
  , CanSubst(..)
  , difference
  ) where

import Control.Applicative ((<$>))
import Data.Map (Map)
import Data.Maybe (fromMaybe)
import Data.Monoid (Monoid(..))
import Data.Set (Set)
import Lamdu.Expr.TypeVars (TypeVars(..))
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Lamdu.Expr as E
import qualified Lamdu.Expr.TypeVars as TypeVars

type SubSubst t = Map (E.TypeVar t) t

data Subst = Subst
  { substTypes :: SubSubst E.Type
  , substRecordTypes :: SubSubst E.ProductType
  }

instance Monoid Subst where
  mempty = Subst Map.empty Map.empty
  mappend (Subst t0 r0) s1@(Subst t1 r1) =
    Subst
    (t1 `Map.union` Map.map (apply s1) t0)
    (r1 `Map.union` Map.map (apply s1) r0)

intersectMapSet :: Ord k => Set k -> Map k a -> Map k a
intersectMapSet s m = Map.intersection m $ Map.fromSet (const ()) s

intersect :: TypeVars -> Subst -> Subst
intersect (TypeVars tvs rtvs) (Subst ts rs) =
  Subst (intersectMapSet tvs ts) (intersectMapSet rtvs rs)

difference :: TypeVars -> TypeVars -> TypeVars
difference (TypeVars t0 r0) (TypeVars t1 r1) =
  TypeVars (Set.difference t0 t1) (Set.difference r0 r1)

class CanSubst a where
  freeVars :: a -> TypeVars
  apply   :: Subst -> a -> a

class (TypeVars.HasVar t, CanSubst t) => HasVar t where
  new :: E.TypeVar t -> t -> Subst

class TypeVars.CompositeHasVar p => CompositeHasVar p where
  compositeNew :: SubSubst (E.CompositeType p) -> Subst
  compositeGet :: Subst -> SubSubst (E.CompositeType p)

instance CompositeHasVar p => CanSubst (E.CompositeType p) where
  freeVars E.CEmpty          = mempty
  freeVars (E.CVar n)        = TypeVars.newVar n
  freeVars (E.CExtend _ t r) = freeVars t `mappend` freeVars r

  apply _ E.CEmpty          = E.CEmpty
  apply s (E.CVar n)        = fromMaybe (E.CVar n) $ Map.lookup n (compositeGet s)
  apply s (E.CExtend n t r) = E.CExtend n (apply s t) (apply s r)

instance CanSubst E.Type where
  freeVars (E.TVar n)      =  TypeVars.newVar n
  freeVars (E.TInst _ p)   =  mconcat $ map freeVars $ Map.elems p
  freeVars (E.TFun t1 t2)  =  freeVars t1 `mappend` freeVars t2
  freeVars (E.TRecord r)   =  freeVars r

  apply s (E.TVar n)      = fromMaybe (E.TVar n) $ Map.lookup n (substTypes s)
  apply s (E.TInst n p)   = E.TInst n $ apply s <$> p
  apply s (E.TFun t1 t2)  = E.TFun (apply s t1) (apply s t2)
  apply s (E.TRecord r)   = E.TRecord $ apply s r

instance HasVar E.Type where
  new tv t = Subst (Map.singleton tv t) mempty
  {-# INLINE new #-}

instance CompositeHasVar E.Product where
  {-# INLINE compositeNew #-}
  compositeGet = substRecordTypes
  compositeNew = Subst mempty

instance CompositeHasVar p => HasVar (E.CompositeType p) where
  new tv t = compositeNew $ Map.singleton tv t
  {-# INLINE new #-}
