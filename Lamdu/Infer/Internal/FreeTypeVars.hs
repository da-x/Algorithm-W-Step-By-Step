{-# LANGUAGE FlexibleInstances #-}

module Lamdu.Infer.Internal.FreeTypeVars
  ( Subst(..), substDeleteVars
  , FreeTypeVars(..), NewSubst(..)
  ) where

import Control.Applicative ((<$>))
import Data.Map (Map)
import Data.Maybe (fromMaybe)
import Data.Monoid (Monoid(..))
import Lamdu.Infer.Internal.TypeVars (TypeVars(..))
import qualified Data.Map as Map
import qualified Data.Map.Utils as MapU
import qualified Data.Set as Set
import qualified Lamdu.Expr as E
import qualified Lamdu.Infer.Internal.TypeVars as TypeVars

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

substDeleteVars :: TypeVars -> Subst -> Subst
substDeleteVars (TypeVars t r) (Subst st sr) =
  Subst (MapU.deleteKeySet t st) (MapU.deleteKeySet r sr)

class FreeTypeVars a where
  freeTypeVars :: a -> TypeVars
  applySubst   :: Subst -> a -> a

instance FreeTypeVars E.ProductType where
  freeTypeVars E.CEmpty          = mempty
  freeTypeVars (E.CVar n)        = TypeVars.newVars (Set.singleton n)
  freeTypeVars (E.CExtend _ t r) = freeTypeVars t `mappend` freeTypeVars r

  applySubst _ E.CEmpty          = E.CEmpty
  applySubst s (E.CVar n)        = fromMaybe (E.CVar n) $ Map.lookup n (substRecordTypes s)
  applySubst s (E.CExtend n t r) = E.CExtend n (applySubst s t) (applySubst s r)

instance FreeTypeVars E.Type where
  freeTypeVars (E.TVar n)      =  TypeVars.newVars (Set.singleton n)
  freeTypeVars (E.TInst _ p)   =  mconcat $ map freeTypeVars $ Map.elems p
  freeTypeVars (E.TFun t1 t2)  =  freeTypeVars t1 `mappend` freeTypeVars t2
  freeTypeVars (E.TRecord r)   =  freeTypeVars r

  applySubst s (E.TVar n)      = fromMaybe (E.TVar n) $ Map.lookup n (substTypes s)
  applySubst s (E.TInst n p)   = E.TInst n $ applySubst s <$> p
  applySubst s (E.TFun t1 t2)  = E.TFun (applySubst s t1) (applySubst s t2)
  applySubst s (E.TRecord r)   = E.TRecord $ applySubst s r

class (E.TypePart t, FreeTypeVars t) => NewSubst t where
  newSubst :: E.TypeVar t -> t -> Subst

instance NewSubst E.Type          where
  newSubst tv t = Subst (Map.singleton tv t) mempty
  {-# INLINE newSubst #-}

instance NewSubst E.ProductType where
  newSubst tv t = Subst mempty (Map.singleton tv t)
  {-# INLINE newSubst #-}
