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
import Lamdu.Expr.TypeVars (TypeVars(..))
import Lamdu.Infer.Internal.Monad (Infer)
import Lamdu.Infer.Internal.Subst (Subst(..))
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Lamdu.Expr.Constraints as Constraints
import qualified Lamdu.Expr.Scheme as Scheme
import qualified Lamdu.Expr.Type as T
import qualified Lamdu.Infer.Internal.Monad as M
import qualified Lamdu.Infer.Internal.Subst as Subst

makeScheme :: M.Context -> Type -> Scheme
makeScheme = Scheme.make . M.constraints . M.ctxResults

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
      (`Subst` fmap T.liftVar recordSubsts) .
      fmap T.liftVar
      <$> mkInstantiateSubstPart "i" tv
    M.tellConstraints $ Constraints.applyRenames recordSubsts constraints
    return $ Subst.apply subst t
