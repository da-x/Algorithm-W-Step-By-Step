{-# LANGUAGE OverloadedStrings #-}
module Lamdu.Infer.Recursive
  ( inferEnv
  ) where

import Lamdu.Infer (Infer)
import Lamdu.Infer.Internal.Monad (freshInferredVar)
import Lamdu.Infer.Internal.Scope (Scope)
import qualified Lamdu.Expr.Val as V
import qualified Lamdu.Infer as Infer
import qualified Lamdu.Infer.Internal.Scope as Scope

inferEnv :: V.Var -> Scope -> Infer Infer.Payload
inferEnv recurseVar scope =
  do
    recursiveType <- freshInferredVar "recurse"
    return $
        Infer.Payload recursiveType $
        Scope.insertTypeOf recurseVar recursiveType scope
