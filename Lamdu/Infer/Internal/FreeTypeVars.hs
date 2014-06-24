module Lamdu.Infer.Internal.FreeTypeVars
  ( Subst, substLookup, substDelete, substFromList
  , FreeTypeVars(..)
  ) where

import Data.Monoid (Monoid(..))
import Lamdu.Expr
import qualified Data.Map as Map
import qualified Data.Set as Set

-- TODO: Where should this be defined?
newtype Subst = Subst (Map.Map TypeVar Type)
instance Monoid Subst where
  mempty = Subst Map.empty
  mappend (Subst s1) (Subst s2) = Subst (s2 `Map.union` (Map.map (applySubst (Subst s2)) s1))

substLookup :: TypeVar -> Subst -> Maybe Type
substLookup name (Subst s) = Map.lookup name s

substDelete :: TypeVar -> Subst -> Subst
substDelete name (Subst s) = Subst (Map.delete name s)

substFromList :: [(TypeVar, Type)] -> Subst
substFromList = Subst . Map.fromList

class FreeTypeVars a where
    freeTypeVars    ::  a -> Set.Set TypeVar
    applySubst           ::  Subst -> a -> a

instance FreeTypeVars Type where
    freeTypeVars (TVar n)      =  Set.singleton n
    freeTypeVars (TCon _)      =  Set.empty
    freeTypeVars (TFun t1 t2)  =  freeTypeVars t1 `Set.union` freeTypeVars t2
    freeTypeVars (TApp t1 t2)  =  freeTypeVars t1 `Set.union` freeTypeVars t2
    freeTypeVars TRecEmpty     =  Set.empty
    freeTypeVars (TRecExtend _ t1 t2) = freeTypeVars t1 `Set.union` freeTypeVars t2

    applySubst s (TVar n)      =  case substLookup n s of
                               Nothing  -> TVar n
                               Just t   -> t
    applySubst s (TFun t1 t2)  = TFun (applySubst s t1) (applySubst s t2)
    applySubst s (TApp t1 t2)  = TApp (applySubst s t1) (applySubst s t2)
    applySubst _s (TCon t)     = TCon t
    applySubst _s TRecEmpty = TRecEmpty
    applySubst s (TRecExtend name typ rest) =
      TRecExtend name (applySubst s typ) $ applySubst s rest
