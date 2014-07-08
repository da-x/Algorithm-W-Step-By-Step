module Lamdu.Expr.Globals
  ( valGlobals
  ) where

import qualified Data.Foldable as Foldable
import qualified Lamdu.Expr as E

valBodyGlobals :: E.ValBody exp -> [E.Tag]
valBodyGlobals (E.VLeaf (E.VGlobal g)) = [g]
valBodyGlobals _ = []

valGlobals :: E.Val a -> [E.Tag]
valGlobals (E.Val _ body) =
  valBodyGlobals body ++ concatMap valGlobals (Foldable.toList body)
