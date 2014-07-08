{-# LANGUAGE FlexibleContexts #-}
module Lamdu.Infer.Internal.Scheme
  ( Scheme(..)
  , generalize
  , instantiate
  , specific
  ) where

import Control.Applicative ((<$>))
import Data.Map (Map)
import Data.Monoid (Monoid(..))
import Data.Set (Set)
import Data.String (IsString(..))
import Lamdu.Infer.Internal.Constraints (Constraints(..))
import Lamdu.Infer.Internal.FreeTypeVars (FreeTypeVars(..))
import Lamdu.Infer.Internal.Monad (Infer)
import Lamdu.Infer.Internal.TypeVars (TypeVars(..))
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Lamdu.Expr as E
import qualified Lamdu.Infer.Internal.Constraints as Constraints
import qualified Lamdu.Infer.Internal.FreeTypeVars as FreeTypeVars
import qualified Lamdu.Infer.Internal.Monad as M
import qualified Lamdu.Infer.Internal.TypeVars as TypeVars

data Scheme = Scheme
  { schemeForAll :: TypeVars
  , schemeConstraints :: Constraints
  , schemeType :: E.Type
  }

instance FreeTypeVars Scheme where
    freeTypeVars (Scheme vars _ t) =
      freeTypeVars t `TypeVars.difference` vars
    applySubst s (Scheme vars constraints t) =
      Scheme vars constraints $ applySubst (FreeTypeVars.substDeleteVars vars s) t

specific :: E.Type -> Scheme
specific = Scheme mempty mempty

-- outside represents all outside types
generalize :: FreeTypeVars o => o -> Infer (E.Type, a) -> Infer (o, Scheme, a)
generalize outside mkType = do
  ((t, pl), results@(M.Results s c)) <- M.listenNoTell mkType
  let outside' = applySubst s outside
  let insideVars = freeTypeVars t `TypeVars.difference` freeTypeVars outside'
  M.tell $ M.deleteResultsVars insideVars results
  return (outside', Scheme insideVars c t, pl)

mkInstantiateSubstPart :: (IsString v, Ord v) => String -> Set v -> Infer (Map v v)
mkInstantiateSubstPart prefix =
  fmap Map.fromList . mapM f . Set.toList
  where
    f oldVar =
      do
        newVarExpr <- M.newInferredVarName prefix
        return (oldVar, newVarExpr)

instantiate :: Scheme -> Infer E.Type
instantiate (Scheme (TypeVars tv rv) constraints t) =
  do
    recordSubsts <- mkInstantiateSubstPart "k" rv
    subst <-
      (`FreeTypeVars.Subst` fmap E.liftVar recordSubsts) .
      fmap E.liftVar
      <$> mkInstantiateSubstPart "i" tv
    M.tellConstraints $ Constraints.applyRenames recordSubsts constraints
    return $ applySubst subst t
