module Lamdu.Infer.Internal.Constraints
  ( applySubst
  ) where

import Control.Applicative ((<$>))
import Lamdu.Expr.Constraints (Constraints(..))
import Lamdu.Infer.Error (Error(FieldForbidden))
import Lamdu.Infer.Internal.Subst (Subst(..))
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Lamdu.Expr.Type as T

applySubst ::
  Subst -> Constraints -> Either Error Constraints
applySubst (Subst _ rtvSubsts) (Constraints c) =
  Constraints . Map.fromListWith Set.union . concat <$>
  mapM onConstraint (Map.toList c)
  where
    onConstraint (var, forbidden) =
      case Map.lookup var rtvSubsts of
      Nothing -> Right [(var, forbidden)]
      Just recType ->
        go recType
        where
          go T.CEmpty             = Right []
          go (T.CVar newVar)      = Right [(newVar, forbidden)]
          go (T.CExtend f _ rest)
            | Set.member f forbidden = Left $ FieldForbidden f var recType
            | otherwise              = go rest
