{-# LANGUAGE DeriveGeneric #-}
module Lamdu.Infer.Internal.TypeVars
  ( TypeVars(..)
  , HasVar(..), CompositeHasVar
  , difference
  , Subst(..), intersectSubst
  , Free(..)
  ) where

import Control.Applicative ((<$>))
import Control.DeepSeq (NFData(..))
import Control.DeepSeq.Generics (genericRnf)
import Data.Map (Map)
import Data.Maybe (fromMaybe)
import Data.Monoid (Monoid(..))
import Data.Set (Set)
import GHC.Generics (Generic)
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Lamdu.Expr as E

data TypeVars = TypeVars (Set (E.TypeVar E.Type)) (Set (E.TypeVar E.ProductType))
  deriving (Eq, Generic)
instance NFData TypeVars where
  rnf = genericRnf
instance Monoid TypeVars where
  mempty = TypeVars mempty mempty
  mappend (TypeVars t0 r0) (TypeVars t1 r1) =
    TypeVars (mappend t0 t1) (mappend r0 r1)

type SubSubst t = Map (E.TypeVar t) t

data Subst = Subst
  { substTypes :: SubSubst E.Type
  , substRecordTypes :: SubSubst E.ProductType
  }

instance Monoid Subst where
  mempty = Subst Map.empty Map.empty
  mappend (Subst t0 r0) s1@(Subst t1 r1) =
    Subst
    (t1 `Map.union` Map.map (applySubst s1) t0)
    (r1 `Map.union` Map.map (applySubst s1) r0)

intersectMapSet :: Ord k => Set k -> Map k a -> Map k a
intersectMapSet s m = Map.intersection m $ Map.fromSet (const ()) s

intersectSubst :: TypeVars -> Subst -> Subst
intersectSubst (TypeVars tvs rtvs) (Subst ts rs) =
  Subst (intersectMapSet tvs ts) (intersectMapSet rtvs rs)

class Free a where
  free :: a -> TypeVars
  applySubst   :: Subst -> a -> a

class CompositeHasVar p where
  compositeGetVars :: TypeVars -> Set (E.TypeVar (E.CompositeType p))
  compositeNewVars :: Set (E.TypeVar (E.CompositeType p)) -> TypeVars
  compositeGetSubst :: Subst -> SubSubst (E.CompositeType p)
  compositeNewSubst :: SubSubst (E.CompositeType p) -> Subst

class Free t => HasVar t where
  getVars :: TypeVars -> Set (E.TypeVar t)
  newVars :: Set (E.TypeVar t) -> TypeVars
  liftVar :: E.TypeVar t -> t
  newSubst :: E.TypeVar t -> t -> Subst

instance CompositeHasVar p => Free (E.CompositeType p) where
  free E.CEmpty          = mempty
  free (E.CVar n)        = newVars (Set.singleton n)
  free (E.CExtend _ t r) = free t `mappend` free r

  applySubst _ E.CEmpty          = E.CEmpty
  applySubst s (E.CVar n)        = fromMaybe (E.CVar n) $ Map.lookup n (compositeGetSubst s)
  applySubst s (E.CExtend n t r) = E.CExtend n (applySubst s t) (applySubst s r)

instance Free E.Type where
  free (E.TVar n)      =  newVars (Set.singleton n)
  free (E.TInst _ p)   =  mconcat $ map free $ Map.elems p
  free (E.TFun t1 t2)  =  free t1 `mappend` free t2
  free (E.TRecord r)   =  free r

  applySubst s (E.TVar n)      = fromMaybe (E.TVar n) $ Map.lookup n (substTypes s)
  applySubst s (E.TInst n p)   = E.TInst n $ applySubst s <$> p
  applySubst s (E.TFun t1 t2)  = E.TFun (applySubst s t1) (applySubst s t2)
  applySubst s (E.TRecord r)   = E.TRecord $ applySubst s r

instance HasVar E.Type where
  getVars (TypeVars vs _) = vs
  newVars vs = TypeVars vs mempty
  liftVar = E.TVar
  newSubst tv t = Subst (Map.singleton tv t) mempty
  {-# INLINE newSubst #-}

instance CompositeHasVar E.Product where
  compositeGetVars (TypeVars _ vs) = vs
  compositeNewVars = TypeVars mempty
  compositeNewSubst = Subst mempty
  {-# INLINE compositeNewSubst #-}
  compositeGetSubst = substRecordTypes

instance CompositeHasVar p => HasVar (E.CompositeType p) where
  getVars = compositeGetVars
  newVars = compositeNewVars
  newSubst tv t = compositeNewSubst $ Map.singleton tv t
  {-# INLINE newSubst #-}
  liftVar = E.CVar

difference :: TypeVars -> TypeVars -> TypeVars
difference (TypeVars t0 r0) (TypeVars t1 r1) =
  TypeVars (Set.difference t0 t1) (Set.difference r0 r1)
