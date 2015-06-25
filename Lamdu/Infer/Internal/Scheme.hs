module Lamdu.Infer.Internal.Scheme
    ( makeScheme
    , instantiate
    ) where

import           Control.Lens.Operators
import           Control.Monad (liftM)
import           Data.Map (Map)
import qualified Data.Map as Map
import           Data.Set (Set)
import qualified Data.Set as Set
import qualified Lamdu.Expr.Constraints as Constraints
import           Lamdu.Expr.Scheme (Scheme(..))
import qualified Lamdu.Expr.Scheme as Scheme
import           Lamdu.Expr.Type (Type)
import qualified Lamdu.Expr.Type as T
import           Lamdu.Expr.TypeVars (TypeVars(..))
import qualified Lamdu.Expr.TypeVars as TV
import           Lamdu.Infer.Internal.Monad (InferCtx)
import qualified Lamdu.Infer.Internal.Monad as M
import           Lamdu.Infer.Internal.Subst (Subst(..))
import qualified Lamdu.Infer.Internal.Subst as Subst

{-# INLINE makeScheme #-}
makeScheme :: M.Context -> Type -> Scheme
makeScheme = Scheme.make . M._constraints . M._ctxResults

{-# INLINE mkInstantiateSubstPart #-}
mkInstantiateSubstPart ::
    Monad m => String -> Set (T.Var t) -> InferCtx m (Map (T.Var t) (T.Var t))
mkInstantiateSubstPart prefix =
    liftM Map.fromList . mapM f . Set.toList
    where
        f oldVar =
            do
                freshVarExpr <- M.freshInferredVarName prefix
                return (oldVar, freshVarExpr)

{-# INLINE instantiate #-}
instantiate :: Monad m => Scheme -> InferCtx m Type
instantiate (Scheme (TypeVars tv rv sv) constraints t) =
    do
        typeVarSubsts <- mkInstantiateSubstPart "i" tv
        recordSubsts <- mkInstantiateSubstPart "k" rv
        sumSubsts <- mkInstantiateSubstPart "s" sv
        let subst =
                Subst
                (fmap TV.lift typeVarSubsts)
                (fmap TV.lift recordSubsts)
                (fmap TV.lift sumSubsts)
            constraints' = Constraints.applyRenames recordSubsts sumSubsts constraints
        -- Avoid tell for these new constraints, because they refer to
        -- fresh variables, no need to apply the ordinary expensive
        -- and error-emitting tell
        M.Infer $ M.ctxResults . M.constraints <>= constraints'
        return $ Subst.apply subst t
