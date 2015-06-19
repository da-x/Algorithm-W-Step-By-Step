{-# LANGUAGE DeriveGeneric #-}
module Lamdu.Expr.TypeVars
    ( TypeVars(..)
    , Free(..)
    , VarKind(..), CompositeVarKind(..)
    , difference
    ) where

import Control.DeepSeq (NFData(..))
import Control.DeepSeq.Generics (genericRnf)
import Data.Binary (Binary)
import Data.Monoid (Monoid(..), (<>))
import Data.Set (Set)
import GHC.Generics (Generic)
import Lamdu.Expr.Type (Type)
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Lamdu.Expr.Type as T

data TypeVars = TypeVars (Set (T.Var Type)) (Set T.ProductVar)
    deriving (Eq, Generic, Show)
instance NFData TypeVars where
    rnf = genericRnf
instance Monoid TypeVars where
    mempty = TypeVars mempty mempty
    mappend (TypeVars t0 r0) (TypeVars t1 r1) =
        TypeVars (mappend t0 t1) (mappend r0 r1)

instance Binary TypeVars

difference :: TypeVars -> TypeVars -> TypeVars
difference (TypeVars t0 r0) (TypeVars t1 r1) =
    TypeVars (Set.difference t0 t1) (Set.difference r0 r1)

class Free t where free :: t -> TypeVars

instance Free Type where
    free (T.TVar n)      =  singleton n
    free (T.TInst _ p)   =  mconcat $ map free $ Map.elems p
    free (T.TFun t1 t2)  =  free t1 <> free t2
    free (T.TRecord r)   =  free r

instance CompositeVarKind p => Free (T.Composite p) where
    free T.CEmpty          = mempty
    free (T.CVar n)        = singleton n
    free (T.CExtend _ t r) = free t <> free r

class T.LiftVar t => VarKind t where
    member :: T.Var t -> TypeVars -> Bool
    singleton :: T.Var t -> TypeVars

instance VarKind Type where
    member v (TypeVars vs _) = v `Set.member` vs
    singleton v = TypeVars (Set.singleton v) mempty

class CompositeVarKind p where
    compositeMember :: T.Var (T.Composite p) -> TypeVars -> Bool
    compositeSingleton :: T.Var (T.Composite p) -> TypeVars

instance CompositeVarKind T.Product where
    compositeMember v (TypeVars _ vs) = v `Set.member` vs
    compositeSingleton = TypeVars mempty . Set.singleton

instance CompositeVarKind p => VarKind (T.Composite p) where
    member = compositeMember
    singleton = compositeSingleton
