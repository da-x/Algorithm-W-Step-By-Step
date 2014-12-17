module Lamdu.Expr.Type.Match
  ( VarMatches, matchVars
  ) where

import Control.Applicative (Applicative(..), (<$>))
import Control.Lens.Operators
import Data.Monoid (Monoid(..))
import Lamdu.Expr.FlatComposite (FlatComposite(..))
import Lamdu.Expr.Type
import qualified Data.Map as Map
import qualified Data.Map.Utils as MapUtils
import qualified Lamdu.Expr.FlatComposite as Flat

type VarMatches = ([(Var Type, Var Type)], [(ProductVar, ProductVar)])

matchVars :: Type -> Type -> Maybe VarMatches
matchVars (TVar tv0)         (TVar tv1)         = Just ([(tv0, tv1)], [])
matchVars (TFun a0 b0)       (TFun a1 b1)       = mappend <$> matchVars a0 a1 <*> matchVars b0 b1
matchVars (TInst i0 params0) (TInst i1 params1)
  | i0 == i1 =
    MapUtils.match matchVars params0 params1
    <&> Map.elems >>= sequence <&> mconcat
matchVars (TRecord c0) (TRecord c1) = matchProductVars c0 c1
matchVars _ _ = Nothing

matchProductVars :: Composite Product -> Composite Product -> Maybe VarMatches
matchProductVars c0 c1 =
  case (v0, v1) of
  (Nothing, Nothing) -> fMatch
  (Just r0, Just r1) -> fMatch `mappend` Just ([], [(r0, r1)])
  _ -> Nothing
  where
    FlatComposite f0 v0 = Flat.fromComposite c0
    FlatComposite f1 v1 = Flat.fromComposite c1
    fMatch =
      MapUtils.match matchVars f0 f1
      <&> Map.elems >>= sequence <&> mconcat
