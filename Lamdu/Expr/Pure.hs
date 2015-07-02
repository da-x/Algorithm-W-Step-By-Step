module Lamdu.Expr.Pure
    ( abs, var, global, litInt, recEmpty, app, recExtend, getField
    , inject, absurd, _case
    , fromNom, toNom
    , leaf, hole

    , record, lambda, lambdaRecord
    , ($$), ($$:), ($.), ($=)
    ) where

import           Prelude hiding (abs)

import           Data.Monoid (Monoid(..))
import           Lamdu.Expr.Val (Val(..))
import qualified Lamdu.Expr.Type as T
import qualified Lamdu.Expr.Val as V

abs :: Monoid a => V.Var -> Val a -> Val a
abs name body =
    Val mempty $ V.BAbs $ V.Lam name body

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

getField :: Monoid a => T.Tag -> Val a
getField = leaf . V.LGetField

inject :: Monoid a => T.Tag -> Val a
inject = leaf . V.LInject

absurd :: Monoid a => Val a
absurd = leaf V.LAbsurd

_case :: Monoid a => T.Tag -> Val a -> Val a -> Val a
_case tag match mismatch = Val mempty $ V.BCase $ V.Case tag match mismatch

fromNom :: Monoid a => T.Id -> Val a
fromNom = leaf . V.LFromNom

toNom :: Monoid a => T.Id -> Val a
toNom = leaf . V.LToNom

hole :: Monoid a => Val a
hole = leaf V.LHole



infixl 4 $$
($$) :: Monoid a => Val a -> Val a -> Val a
($$) = app

infixl 4 $$:
($$:) :: Val () -> [(T.Tag, Val ())] -> Val ()
func $$: fields = func $$ record fields

infixl 9 $.
($.) :: Monoid a => Val a -> T.Tag -> Val a
($.) r t = getField t $$ r

infixl 3 $=
($=) :: T.Tag -> Val () -> Val () -> Val ()
($=) = recExtend

record :: Monoid a => [(T.Tag, Val a)] -> Val a
record = foldr (uncurry recExtend) recEmpty

lambda :: Monoid a => V.Var -> (Val a -> Val a) -> Val a
lambda v body = abs v $ body $ var v

lambdaRecord :: Monoid a => V.Var -> [T.Tag] -> ([Val a] -> Val a) -> Val a
lambdaRecord v tags body =
    abs v $ body $ map (($.) (var v)) tags
