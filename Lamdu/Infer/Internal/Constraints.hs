module Lamdu.Infer.Internal.Constraints
  ( applySubst, intersect
  ) where

import Control.Applicative ((<$>))
import Lamdu.Expr.Constraints (Constraints(..))
import Lamdu.Expr.TypeVars (TypeVars)
import Lamdu.Infer.Error (Error(FieldForbidden))
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Lamdu.Expr as E
import qualified Lamdu.Expr.TypeVars as TypeVars
import qualified Lamdu.Infer.Internal.TypeVars as TypeVars

applySubst ::
  TypeVars.Subst -> Constraints -> Either Error Constraints
applySubst (TypeVars.Subst _ recordSubsts) (Constraints c) =
  Constraints . Map.fromListWith Set.union . concat <$>
  mapM onConstraint (Map.toList c)
  where
    onConstraint (var, forbidden) =
      case Map.lookup var recordSubsts of
      Nothing -> Right [(var, forbidden)]
      Just recType ->
        go recType
        where
          go E.CEmpty             = Right []
          go (E.CVar newVar)      = Right [(newVar, forbidden)]
          go (E.CExtend f _ rest)
            | Set.member f forbidden = Left $ FieldForbidden f var recType
            | otherwise              = go rest

intersect :: TypeVars -> Constraints -> Constraints
intersect tvs (Constraints c) =
  Constraints (Map.filterWithKey inTVs c)
  where
    inTVs rtv _ = rtv `Set.member` TypeVars.getVars tvs
