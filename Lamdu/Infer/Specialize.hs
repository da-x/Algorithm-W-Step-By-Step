module Lamdu.Infer.Specialize
	( specialize
	) where

import Data.Monoid (Monoid(..), (<>))
import Lamdu.Expr.TypeVars (TypeVars(..))
import Lamdu.Infer.Internal.Monad (Infer(..))
import Lamdu.Infer.Internal.PositionVars (PositionVars(..), positionVars)
import qualified Data.Set as Set
import qualified Lamdu.Expr.Type as E
import qualified Lamdu.Expr.TypeVars as TypeVars
import qualified Lamdu.Infer.Internal.Monad as M
import qualified Lamdu.Infer.Internal.Subst as Subst

specialize :: E.Type -> Infer ()
specialize t =
  M.tellSubsts $ substVars vars emptyRecord <> substVars vars emptyProduct
  where
    pv = positionVars t
    vars = TypeVars.difference (pvNegative pv) (pvPositive pv <> pvUnknown pv)
    emptyRecord = E.TRecord E.CEmpty
    emptyProduct :: E.ProductType
    emptyProduct = E.CEmpty

substVars :: Subst.HasVar t => TypeVars -> t -> Subst.Subst
substVars vars val = mconcat $ map (`Subst.new` val) (Set.toList (TypeVars.getVars vars))
