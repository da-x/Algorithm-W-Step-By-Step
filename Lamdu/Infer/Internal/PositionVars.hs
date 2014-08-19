module Lamdu.Infer.Internal.PositionVars
  ( PositionVars(..), positionVars
  ) where

import Data.Monoid (Monoid(..), (<>))
import Lamdu.Expr.Type (Type)
import Lamdu.Expr.TypeVars (TypeVars(..))
import qualified Data.Map as Map
import qualified Lamdu.Expr.Type as T
import qualified Lamdu.Expr.TypeVars as TypeVars
import qualified Lamdu.Infer.Internal.Subst as Subst

data PositionVars = PositionVars
  { pvPositive :: TypeVars
  , pvNegative :: TypeVars
  , pvUnknown :: TypeVars
  }

instance Monoid PositionVars where
  mempty = PositionVars mempty mempty mempty
  mappend (PositionVars p0 n0 u0) (PositionVars p1 n1 u1) =
    PositionVars (p0 <> p1) (n0 <> n1) (u0 <> u1)

negatePos :: PositionVars -> PositionVars
negatePos (PositionVars pos neg unk) = PositionVars neg pos unk

positionVars :: Type -> PositionVars
positionVars (T.TVar v) = mempty { pvPositive = TypeVars.newVar v }
positionVars (T.TFun a r) = negatePos (positionVars a) <> positionVars r
positionVars (T.TRecord c) = compositePositionVars c
positionVars (T.TInst _ args) =
  mempty { pvUnknown = mconcat $ map Subst.freeVars $ Map.elems args }

compositePositionVars :: TypeVars.CompositeHasVar p => T.Composite p -> PositionVars
compositePositionVars (T.CVar v) = mempty { pvPositive = TypeVars.newVar v }
compositePositionVars (T.CExtend _ t c) = positionVars t <> compositePositionVars c
compositePositionVars T.CEmpty = mempty
