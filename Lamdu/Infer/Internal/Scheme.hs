{-# LANGUAGE DeriveGeneric #-}
module Lamdu.Infer.Internal.Scheme
  ( Scheme(..)
  , make
  , instantiate
  ) where

import Control.Applicative ((<$>))
import Control.DeepSeq (NFData(..))
import Control.DeepSeq.Generics (genericRnf)
import Data.Map (Map)
import Data.Set (Set)
import Data.String (IsString(..))
import GHC.Generics (Generic)
import Lamdu.Infer.Internal.Constraints (Constraints(..))
import Lamdu.Infer.Internal.Monad (Infer)
import Lamdu.Infer.Internal.TypeVars (FreeTypeVars(..), TypeVars(..), HasVar(..))
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Lamdu.Expr as E
import qualified Lamdu.Infer.Internal.Constraints as Constraints
import qualified Lamdu.Infer.Internal.Monad as M
import qualified Lamdu.Infer.Internal.TypeVars as TypeVars

data Scheme = Scheme
  { schemeForAll :: TypeVars
  , schemeConstraints :: Constraints
  , schemeType :: E.Type
  } deriving (Generic)

instance NFData Scheme where
  rnf = genericRnf

make :: E.Type -> Infer Scheme
make t = do
  Constraints c <- M.getConstraints
  let c' = Constraints $ Map.filterWithKey inFreeVars c
  return $ Scheme freeVars c' t
  where
    inFreeVars rtv _ = rtv `Set.member` TypeVars.getVars freeVars
    freeVars = freeTypeVars t

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
      (`TypeVars.Subst` fmap liftVar recordSubsts) .
      fmap liftVar
      <$> mkInstantiateSubstPart "i" tv
    M.tellConstraints $ Constraints.applyRenames recordSubsts constraints
    return $ applySubst subst t
