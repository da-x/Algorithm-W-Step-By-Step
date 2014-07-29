module Lamdu.Expr.Pure
  ( abs, var, global, litInt, recEmpty, app, recExtend, getField
  ) where

import Prelude hiding (abs)
import qualified Lamdu.Expr as E

abs :: E.ValVar -> E.Val () -> E.Val ()
abs name body = E.Val () $ E.VAbs $ E.Lam name body

var :: E.ValVar -> E.Val ()
var = E.Val () . E.VLeaf . E.VVar

global :: E.GlobalId -> E.Val ()
global = E.Val () . E.VLeaf . E.VGlobal

litInt :: Integer -> E.Val ()
litInt = E.Val () . E.VLeaf . E.VLiteralInteger

recEmpty :: E.Val ()
recEmpty = E.Val () $ E.VLeaf E.VRecEmpty

app :: E.Val () -> E.Val () -> E.Val ()
app f x = E.Val () $ E.VApp $ E.Apply f x

recExtend :: E.Tag -> E.Val () -> E.Val () -> E.Val ()
recExtend name typ rest = E.Val () $ E.VRecExtend $ E.RecExtend name typ rest

getField :: E.Val () -> E.Tag -> E.Val ()
getField r n = E.Val () $ E.VGetField $ E.GetField r n
