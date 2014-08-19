{-# LANGUAGE OverloadedStrings #-}

module Lamdu.Expr.Pure
  ( abs, var, global, litInt, recEmpty, app, recExtend, getField, leaf, hole
  ) where

import Data.Monoid (Monoid(..))
import Prelude hiding (abs)
import Lamdu.Expr.Scheme (Scheme(..))
import Lamdu.Expr.Val (Val(..))
import qualified Lamdu.Expr.Type as T
import qualified Lamdu.Expr.TypeVars as TypeVars
import qualified Lamdu.Expr.Val as V

abs :: V.Var -> Val () -> Val ()
abs name body =
  Val () $ V.BAbs $ V.Lam name nullScheme body

nullScheme :: Scheme
nullScheme =
  Scheme (TypeVars.newVar a) mempty (T.TVar a)
  where
    a = "a"

leaf :: V.Leaf -> Val ()
leaf = Val () . V.BLeaf

var :: V.Var -> Val ()
var = leaf . V.LVar

global :: V.GlobalId -> Val ()
global = leaf . V.LGlobal

litInt :: Integer -> Val ()
litInt = leaf . V.LLiteralInteger

recEmpty :: Val ()
recEmpty = Val () $ V.BLeaf V.LRecEmpty

app :: Val () -> Val () -> Val ()
app f x = Val () $ V.BApp $ V.Apply f x

recExtend :: T.Tag -> Val () -> Val () -> Val ()
recExtend name typ rest = Val () $ V.BRecExtend $ V.RecExtend name typ rest

getField :: Val () -> T.Tag -> Val ()
getField r n = Val () $ V.BGetField $ V.GetField r n

hole :: Val ()
hole = leaf V.LHole
