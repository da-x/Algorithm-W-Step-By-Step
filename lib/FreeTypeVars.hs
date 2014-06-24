module FreeTypeVars
  ( Subst, substLookup, substDelete, substFromList
  , FreeTypeVars(..)
  ) where

import Data.Monoid (Monoid(..))
import Expr
import qualified Data.Map as Map
import qualified Data.Set as Set

-- TODO: Where should this be defined?
newtype Subst = Subst (Map.Map TypeVar Type)
instance Monoid Subst where
  mempty = Subst Map.empty
  mappend (Subst s1) (Subst s2) = Subst (s2 `Map.union` (Map.map (apply (Subst s2)) s1))

substLookup :: TypeVar -> Subst -> Maybe Type
substLookup name (Subst s) = Map.lookup name s

substDelete :: TypeVar -> Subst -> Subst
substDelete name (Subst s) = Subst (Map.delete name s)

substFromList :: [(TypeVar, Type)] -> Subst
substFromList = Subst . Map.fromList

class FreeTypeVars a where
    freeTypeVars    ::  a -> Set.Set TypeVar
    apply           ::  Subst -> a -> a

instance FreeTypeVars Type where
    freeTypeVars (TVar n)      =  Set.singleton n
    freeTypeVars (TCon _)      =  Set.empty
    freeTypeVars (TFun t1 t2)  =  freeTypeVars t1 `Set.union` freeTypeVars t2
    freeTypeVars (TApp t1 t2)  =  freeTypeVars t1 `Set.union` freeTypeVars t2
    freeTypeVars TRecEmpty     =  Set.empty
    freeTypeVars (TRecExtend _ t1 t2) = freeTypeVars t1 `Set.union` freeTypeVars t2

    apply s (TVar n)      =  case substLookup n s of
                               Nothing  -> TVar n
                               Just t   -> t
    apply s (TFun t1 t2)  = TFun (apply s t1) (apply s t2)
    apply s (TApp t1 t2)  = TApp (apply s t1) (apply s t2)
    apply _s (TCon t)     = TCon t
    apply _s TRecEmpty = TRecEmpty
    apply s (TRecExtend name typ rest) =
      TRecExtend name (apply s typ) $ apply s rest
