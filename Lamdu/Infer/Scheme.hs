{-# LANGUAGE FlexibleContexts, TypeFamilies #-}

module Lamdu.Infer.Scheme
  ( Scheme(..)
  , generalize
  , instantiate
  , specific
  ) where

import Control.Applicative ((<$>), (<*>))
import Data.Map (Map)
import Data.Monoid (Monoid(..))
import Data.Set (Set)
import Lamdu.Infer.Internal.FreeTypeVars (FreeTypeVars(..))
import Lamdu.Infer.Internal.Monad (Infer)
import Lamdu.Infer.TypeVars (TypeVars(..))
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Lamdu.Expr as E
import qualified Lamdu.Infer.Internal.FreeTypeVars as FreeTypeVars
import qualified Lamdu.Infer.Internal.Monad as InferMonad
import qualified Lamdu.Infer.TypeVars as TypeVars

data Scheme = Scheme TypeVars E.Type

instance FreeTypeVars Scheme where
    freeTypeVars (Scheme vars t) = freeTypeVars t `TypeVars.difference` vars
    applySubst s (Scheme vars t) =
      Scheme vars $ applySubst (FreeTypeVars.substDelete vars s) t

specific :: E.Type -> Scheme
specific = Scheme mempty

generalize :: TypeVars -> E.Type -> Scheme
generalize outsideTVs t  =   Scheme vars t
  where vars = freeTypeVars t `TypeVars.difference` outsideTVs

type family VarOf a
type instance VarOf E.Type = E.TypeVar
type instance VarOf E.RecordType = E.RecordTypeVar

mkInstantiateSubstPart ::
  (InferMonad.InfersVars t, Ord (VarOf t)) =>
  String -> Set (VarOf t) -> Infer (Map (VarOf t) t)
mkInstantiateSubstPart prefix =
  fmap Map.fromList . mapM f . Set.toList
  where
    f oldVar =
      do
        newVarExpr <- InferMonad.newInferredVar prefix
        return (oldVar, newVarExpr)

instantiate :: Scheme -> Infer E.Type
instantiate (Scheme (TypeVars tv rv) t) =
  do
    subst <-
      FreeTypeVars.Subst
      <$> mkInstantiateSubstPart "i" tv
      <*> mkInstantiateSubstPart "k" rv
    return $ applySubst subst t
