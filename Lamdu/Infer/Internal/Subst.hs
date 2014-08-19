module Lamdu.Infer.Internal.Subst
  ( HasVar(..), CompositeHasVar
  , Subst(..), intersect
  , CanSubst(..)
  ) where

import Control.Applicative ((<$>))
import Data.Map (Map)
import Data.Maybe (fromMaybe)
import Data.Monoid (Monoid(..))
import Data.Set (Set)
import Lamdu.Expr.Type (Type)
import Lamdu.Expr.TypeVars (TypeVars(..))
import qualified Data.Map as Map
import qualified Lamdu.Expr.Type as T
import qualified Lamdu.Expr.TypeVars as TypeVars

type SubSubst t = Map (T.Var t) t

data Subst = Subst
  { substTypes :: SubSubst Type
  , substRecordTypes :: SubSubst (T.Composite T.Product)
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

class CanSubst a where
  freeVars :: a -> TypeVars
  apply   :: Subst -> a -> a

class (TypeVars.HasVar t, CanSubst t) => HasVar t where
  new :: T.Var t -> t -> Subst

class TypeVars.CompositeHasVar p => CompositeHasVar p where
  compositeNew :: SubSubst (T.Composite p) -> Subst
  compositeGet :: Subst -> SubSubst (T.Composite p)

instance CompositeHasVar p => CanSubst (T.Composite p) where
  freeVars T.CEmpty          = mempty
  freeVars (T.CVar n)        = TypeVars.newVar n
  freeVars (T.CExtend _ t r) = freeVars t `mappend` freeVars r

  apply _ T.CEmpty          = T.CEmpty
  apply s (T.CVar n)        = fromMaybe (T.CVar n) $ Map.lookup n (compositeGet s)
  apply s (T.CExtend n t r) = T.CExtend n (apply s t) (apply s r)

instance CanSubst Type where
  freeVars (T.TVar n)      =  TypeVars.newVar n
  freeVars (T.TInst _ p)   =  mconcat $ map freeVars $ Map.elems p
  freeVars (T.TFun t1 t2)  =  freeVars t1 `mappend` freeVars t2
  freeVars (T.TRecord r)   =  freeVars r

  apply s (T.TVar n)      = fromMaybe (T.TVar n) $ Map.lookup n (substTypes s)
  apply s (T.TInst n p)   = T.TInst n $ apply s <$> p
  apply s (T.TFun t1 t2)  = T.TFun (apply s t1) (apply s t2)
  apply s (T.TRecord r)   = T.TRecord $ apply s r

instance HasVar Type where
  new tv t = Subst (Map.singleton tv t) mempty
  {-# INLINE new #-}

instance CompositeHasVar T.Product where
  {-# INLINE compositeNew #-}
  compositeGet = substRecordTypes
  compositeNew = Subst mempty

instance CompositeHasVar p => HasVar (T.Composite p) where
  new tv t = compositeNew $ Map.singleton tv t
  {-# INLINE new #-}
