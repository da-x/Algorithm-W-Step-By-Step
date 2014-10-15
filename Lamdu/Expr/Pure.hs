module Lamdu.Expr.Pure
  ( abs, var, global, litInt, recEmpty, app, recExtend, getField, leaf, hole
  ) where

import Prelude hiding (abs)

import Data.Monoid (Monoid(..))
import Lamdu.Expr.Scheme (Scheme)
import Lamdu.Expr.Val (Val(..))
import qualified Lamdu.Expr.Type as T
import qualified Lamdu.Expr.Val as V

abs :: Monoid a => V.Var -> Scheme -> Val a -> Val a
abs name template body =
  Val mempty $ V.BAbs $ V.Lam name template body

leaf :: Monoid a => V.Leaf -> Val a
leaf = Val mempty . V.BLeaf

var :: Monoid a => V.Var -> Val a
var = leaf . V.LVar

global :: Monoid a => V.GlobalId -> Val a
global = leaf . V.LGlobal

litInt :: Monoid a => Integer -> Val a
litInt = leaf . V.LLiteralInteger

recEmpty :: Monoid a => Val a
recEmpty = Val mempty $ V.BLeaf V.LRecEmpty

app :: Monoid a => Val a -> Val a -> Val a
app f x = Val mempty $ V.BApp $ V.Apply f x

recExtend :: Monoid a => T.Tag -> Val a -> Val a -> Val a
recExtend name typ rest = Val mempty $ V.BRecExtend $ V.RecExtend name typ rest

getField :: Monoid a => Val a -> T.Tag -> Val a
getField r n = Val mempty $ V.BGetField $ V.GetField r n

hole :: Monoid a => Val a
hole = leaf V.LHole
