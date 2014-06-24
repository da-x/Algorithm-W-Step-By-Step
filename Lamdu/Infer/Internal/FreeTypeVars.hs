module Lamdu.Infer.Internal.FreeTypeVars
  ( Subst, substLookup, substDelete, substFromList
  , FreeTypeVars(..)
  ) where

import Data.Map (Map)
import Data.Monoid (Monoid(..))
import Data.Set (Set)
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Lamdu.Expr as E

-- TODO: Where should this be defined?
newtype Subst = Subst (Map E.TypeVar E.Type)
instance Monoid Subst where
  mempty = Subst Map.empty
  mappend (Subst s1) (Subst s2) = Subst (s2 `Map.union` (Map.map (applySubst (Subst s2)) s1))

substLookup :: E.TypeVar -> Subst -> Maybe E.Type
substLookup name (Subst s) = Map.lookup name s

substDelete :: E.TypeVar -> Subst -> Subst
substDelete name (Subst s) = Subst (Map.delete name s)

substFromList :: [(E.TypeVar, E.Type)] -> Subst
substFromList = Subst . Map.fromList

class FreeTypeVars a where
    freeTypeVars :: a -> Set E.TypeVar
    applySubst   :: Subst -> a -> a

instance FreeTypeVars E.Type where
    freeTypeVars (E.TVar n)      =  Set.singleton n
    freeTypeVars (E.TCon _)      =  Set.empty
    freeTypeVars (E.TFun t1 t2)  =  freeTypeVars t1 `Set.union` freeTypeVars t2
    freeTypeVars (E.TApp t1 t2)  =  freeTypeVars t1 `Set.union` freeTypeVars t2
    freeTypeVars E.TRecEmpty     =  Set.empty
    freeTypeVars (E.TRecExtend _ t1 t2) = freeTypeVars t1 `Set.union` freeTypeVars t2

    applySubst s (E.TVar n)      =  case substLookup n s of
                                  Nothing  -> E.TVar n
                                  Just t   -> t
    applySubst s (E.TFun t1 t2)  = E.TFun (applySubst s t1) (applySubst s t2)
    applySubst s (E.TApp t1 t2)  = E.TApp (applySubst s t1) (applySubst s t2)
    applySubst _s (E.TCon t)     = E.TCon t
    applySubst _s E.TRecEmpty = E.TRecEmpty
    applySubst s (E.TRecExtend name typ rest) =
      E.TRecExtend name (applySubst s typ) $ applySubst s rest
