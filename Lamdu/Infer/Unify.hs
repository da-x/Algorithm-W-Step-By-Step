module Lamdu.Infer.Unify
  ( unify
  ) where

import Lamdu.Expr.Type (Type)
import Lamdu.Infer.Internal.Monad (Infer)
import Lamdu.Infer.Internal.Unify (unifyUnsafe)
import qualified Lamdu.Infer.Internal.Monad as M
import qualified Lamdu.Infer.Internal.Subst as Subst

{-# INLINE unify #-}
unify :: Type -> Type -> Infer ()
unify x y =
  do
    s <- M.getSubst
    unifyUnsafe (Subst.apply s x) (Subst.apply s y)
