module Lamdu.Expr.Pure
  ( abs, var, global, litInt, recEmpty, app, recExtend, getField, leaf, hole
  ) where

import Prelude hiding (abs)
import Lamdu.Expr.Val (Val(..))
import qualified Lamdu.Expr.Type as T
import qualified Lamdu.Expr.Val as V

abs :: V.Var -> Val () -> Val ()
abs name body = Val () $ V.VAbs $ V.Lam name body

leaf :: V.Leaf -> Val ()
leaf = Val () . V.VLeaf

var :: V.Var -> Val ()
var = leaf . V.VVar

global :: V.GlobalId -> Val ()
global = leaf . V.VGlobal

litInt :: Integer -> Val ()
litInt = leaf . V.VLiteralInteger

recEmpty :: Val ()
recEmpty = Val () $ V.VLeaf V.VRecEmpty

app :: Val () -> Val () -> Val ()
app f x = Val () $ V.VApp $ V.Apply f x

recExtend :: T.Tag -> Val () -> Val () -> Val ()
recExtend name typ rest = Val () $ V.VRecExtend $ V.RecExtend name typ rest

getField :: Val () -> T.Tag -> Val ()
getField r n = Val () $ V.VGetField $ V.GetField r n

hole :: Val ()
hole = leaf V.VHole
