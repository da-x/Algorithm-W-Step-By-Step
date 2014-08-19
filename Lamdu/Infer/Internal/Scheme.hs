module Lamdu.Infer.Internal.Scheme
  ( makeScheme
  , instantiate
  ) where

import Control.Applicative ((<$>))
import Data.Map (Map)
import Data.Set (Set)
import Data.String (IsString(..))
import Lamdu.Expr.Scheme (Scheme(..))
import Lamdu.Expr.Type (Type)
import Lamdu.Expr.TypeVars (TypeVars(..), HasVar(..))
import Lamdu.Infer.Internal.Monad (Infer)
import Lamdu.Infer.Internal.Subst (Subst(..))
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Lamdu.Expr.Constraints as Constraints
import qualified Lamdu.Infer.Internal.Constraints as Constraints
import qualified Lamdu.Infer.Internal.Monad as M
import qualified Lamdu.Infer.Internal.Subst as Subst

makeScheme :: Type -> Infer Scheme
makeScheme t = do
  c <- M.getConstraints
  return $ Scheme freeVars (Constraints.intersect freeVars c) t
  where
    freeVars = Subst.freeVars t

mkInstantiateSubstPart :: (IsString v, Ord v) => String -> Set v -> Infer (Map v v)
mkInstantiateSubstPart prefix =
  fmap Map.fromList . mapM f . Set.toList
  where
    f oldVar =
      do
        newVarExpr <- M.newInferredVarName prefix
        return (oldVar, newVarExpr)

instantiate :: Scheme -> Infer Type
instantiate (Scheme (TypeVars tv rv) constraints t) =
  do
    recordSubsts <- mkInstantiateSubstPart "k" rv
    subst <-
      (`Subst` fmap liftVar recordSubsts) .
      fmap liftVar
      <$> mkInstantiateSubstPart "i" tv
    M.tellConstraints $ Constraints.applyRenames recordSubsts constraints
    return $ Subst.apply subst t
