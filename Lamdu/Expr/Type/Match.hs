module Lamdu.Expr.Type.Match
  ( VarMatches, matchVars
  ) where

import Control.Applicative (Applicative(..), (<$>))
import Data.Monoid (Monoid(..))
import Lamdu.Expr.Type
import qualified Data.Map as Map
import qualified Data.Map.Utils as MapUtils

type VarMatches = ([(Var Type, Var Type)], [(ProductVar, ProductVar)])

matchVars :: Type -> Type -> Maybe VarMatches
matchVars (TVar tv0)         (TVar tv1)         = Just ([(tv0, tv1)], [])
matchVars (TFun a0 b0)       (TFun a1 b1)       = mappend <$> matchVars a0 a1 <*> matchVars b0 b1
matchVars (TInst i0 params0) (TInst i1 params1)
  | i0 == i1 =
    fmap mconcat . sequence . Map.elems =<<
    MapUtils.match matchVars params0 params1
matchVars (TRecord c0) (TRecord c1) = matchProductVars c0 c1
matchVars _ _ = Nothing

matchProductVars :: Composite Product -> Composite Product -> Maybe VarMatches
matchProductVars (CExtend tag0 typ0 rest0) (CExtend tag1 typ1 rest1)
  | tag0 == tag1 = matchVars typ0 typ1 `mappend` matchProductVars rest0 rest1
matchProductVars CEmpty CEmpty = Just ([], [])
matchProductVars (CVar tv0) (CVar tv1) = Just ([], [(tv0, tv1)])
matchProductVars _ _ = Nothing
