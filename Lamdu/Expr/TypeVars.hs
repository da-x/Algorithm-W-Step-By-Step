{-# LANGUAGE DeriveGeneric #-}
module Lamdu.Expr.TypeVars
    ( TypeVars(..)
    , Free(..)
    , VarKind(..), CompositeVarKind(..)
    , difference
    ) where

import           Control.DeepSeq (NFData(..))
import           Control.DeepSeq.Generics (genericRnf)
import           Data.Binary (Binary)
import qualified Data.Map as Map
import           Data.Monoid (Monoid(..), (<>))
import           Data.Set (Set)
import qualified Data.Set as Set
import           GHC.Generics (Generic)
import           Lamdu.Expr.Type (Type)
import qualified Lamdu.Expr.Type as T

data TypeVars = TypeVars
    { typeVars :: Set T.TypeVar
    , productTypeVars :: Set T.ProductVar
    , sumTypeVars :: Set T.SumVar
    }
    deriving (Eq, Generic, Show)
instance NFData TypeVars where
    rnf = genericRnf
instance Monoid TypeVars where
    mempty = TypeVars mempty mempty mempty
    mappend (TypeVars t0 r0 s0) (TypeVars t1 r1 s1) =
        TypeVars (mappend t0 t1) (mappend r0 r1) (mappend s0 s1)

instance Binary TypeVars

difference :: TypeVars -> TypeVars -> TypeVars
difference (TypeVars t0 r0 s0) (TypeVars t1 r1 s1) =
    TypeVars (Set.difference t0 t1) (Set.difference r0 r1) (Set.difference s0 s1)

class Free t where free :: t -> TypeVars

instance Free Type where
    free (T.TVar n)      =  singleton n
    free (T.TInst _ p)   =  mconcat $ map free $ Map.elems p
    free (T.TFun t1 t2)  =  free t1 <> free t2
    free (T.TRecord r)   =  free r
    free (T.TSum s)      =  free s

instance CompositeVarKind p => Free (T.Composite p) where
    free T.CEmpty          = mempty
    free (T.CVar n)        = singleton n
    free (T.CExtend _ t r) = free t <> free r

class VarKind t where
    lift :: T.Var t -> t
    member :: T.Var t -> TypeVars -> Bool
    singleton :: T.Var t -> TypeVars

instance VarKind Type where
    lift = T.TVar
    member v tvs = v `Set.member` typeVars tvs
    singleton v = mempty { typeVars = Set.singleton v }

class CompositeVarKind p where
    compositeMember :: T.Var (T.Composite p) -> TypeVars -> Bool
    compositeSingleton :: T.Var (T.Composite p) -> TypeVars

instance CompositeVarKind T.ProductTag where
    compositeMember v tvs = v `Set.member` productTypeVars tvs
    compositeSingleton v = mempty { productTypeVars = Set.singleton v }

instance CompositeVarKind T.SumTag where
    compositeMember v tvs = v `Set.member` sumTypeVars tvs
    compositeSingleton v = mempty { sumTypeVars = Set.singleton v }

instance CompositeVarKind p => VarKind (T.Composite p) where
    lift = T.CVar
    member = compositeMember
    singleton = compositeSingleton
