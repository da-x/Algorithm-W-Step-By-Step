module Lamdu.Expr.Pure
  ( abs, var, global, litInt, recEmpty, app, recExtend, getField, leaf, hole
  ) where

import Prelude hiding (abs)
import qualified Lamdu.Expr as E
import qualified Lamdu.Expr.Type as T

abs :: E.ValVar -> E.Val () -> E.Val ()
abs name body = E.Val () $ E.VAbs $ E.Lam name body

leaf :: E.ValLeaf -> E.Val ()
leaf = E.Val () . E.VLeaf

var :: E.ValVar -> E.Val ()
var = leaf . E.VVar

global :: E.GlobalId -> E.Val ()
global = leaf . E.VGlobal

litInt :: Integer -> E.Val ()
litInt = leaf . E.VLiteralInteger

recEmpty :: E.Val ()
recEmpty = E.Val () $ E.VLeaf E.VRecEmpty

app :: E.Val () -> E.Val () -> E.Val ()
app f x = E.Val () $ E.VApp $ E.Apply f x

recExtend :: T.Tag -> E.Val () -> E.Val () -> E.Val ()
recExtend name typ rest = E.Val () $ E.VRecExtend $ E.RecExtend name typ rest

getField :: E.Val () -> T.Tag -> E.Val ()
getField r n = E.Val () $ E.VGetField $ E.GetField r n

hole :: E.Val ()
hole = leaf E.VHole
