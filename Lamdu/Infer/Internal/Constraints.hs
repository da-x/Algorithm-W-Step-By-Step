module Lamdu.Infer.Internal.Constraints
    ( applySubst
    ) where

import           Control.Applicative (Applicative(..), (<$>))
import           Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.Set as Set
import           Lamdu.Expr.Constraints (Constraints(..), CompositeVarConstraints(..))
import qualified Lamdu.Expr.Type as T
import           Lamdu.Infer.Error (Error(DuplicateField, DuplicateAlt))
import           Lamdu.Infer.Internal.Subst (Subst(..))

applySubst :: Subst -> Constraints -> Either Error Constraints
applySubst (Subst _ rtvSubsts stvSubsts) (Constraints prodC sumC) =
    Constraints
    <$> applySubstCompositeConstraints DuplicateField rtvSubsts prodC
    <*> applySubstCompositeConstraints DuplicateAlt stvSubsts sumC

applySubstCompositeConstraints ::
    (T.Tag -> T.Var (T.Composite t) -> T.Composite t -> err) ->
    Map (T.Var (T.Composite t)) (T.Composite t) ->
    CompositeVarConstraints t ->
    Either err (CompositeVarConstraints t)
applySubstCompositeConstraints fieldForbidden rtvSubsts (CompositeVarConstraints m) =
    CompositeVarConstraints . Map.fromListWith Set.union . concat <$>
    mapM onConstraint (Map.toList m)
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
                        | Set.member f forbidden = Left $ fieldForbidden f var recType
                        | otherwise              = go rest
