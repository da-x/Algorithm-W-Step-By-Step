module TVs
  ( Subst, substLookup, substDelete, substFromList
  , TVs(..)
  ) where

import Data.Monoid (Monoid(..))
import Expr
import qualified Data.Map as Map
import qualified Data.Set as Set

newtype Subst = Subst (Map.Map String Type)
instance Monoid Subst where
  mempty = Subst Map.empty
  mappend (Subst s1) (Subst s2) = Subst (s2 `Map.union` (Map.map (apply (Subst s2)) s1))

substLookup :: String -> Subst -> Maybe Type
substLookup name (Subst s) = Map.lookup name s

substDelete :: String -> Subst -> Subst
substDelete name (Subst s) = Subst (Map.delete name s)

substFromList :: [(String, Type)] -> Subst
substFromList = Subst . Map.fromList

class TVs a where
    ftv    ::  a -> Set.Set String
    apply  ::  Subst -> a -> a

instance TVs Type where
    ftv (TVar n)      =  Set.singleton n
    ftv (TCon _)      =  Set.empty
    ftv (TFun t1 t2)  =  ftv t1 `Set.union` ftv t2
    ftv (TApp t1 t2)  =  ftv t1 `Set.union` ftv t2
    ftv TRecEmpty     =  Set.empty
    ftv (TRecExtend _ t1 t2) = ftv t1 `Set.union` ftv t2

    apply s (TVar n)      =  case substLookup n s of
                               Nothing  -> TVar n
                               Just t   -> t
    apply s (TFun t1 t2)  = TFun (apply s t1) (apply s t2)
    apply s (TApp t1 t2)  = TApp (apply s t1) (apply s t2)
    apply _s (TCon t)     = TCon t
    apply _s TRecEmpty = TRecEmpty
    apply s (TRecExtend name typ rest) =
      TRecExtend name (apply s typ) $ apply s rest
instance TVs Scheme where
    ftv (Scheme vars t)      =  (ftv t) `Set.difference` (Set.fromList vars)

    apply s (Scheme vars t)  =  Scheme vars (apply (foldr substDelete s vars) t)
instance TVs a => TVs [a] where
    apply s  =  map (apply s)
    ftv l    =  foldr Set.union Set.empty (map ftv l)
