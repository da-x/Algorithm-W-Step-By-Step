module Lamdu.Expr.Globals
  ( valGlobals
  ) where

import qualified Data.Foldable as Foldable
import qualified Lamdu.Expr as E

valBodyGlobals :: E.ValBody exp -> [E.GlobalId]
valBodyGlobals (E.VLeaf (E.VGlobal g)) = [g]
valBodyGlobals _ = []

valGlobals :: E.Val a -> [E.GlobalId]
valGlobals (E.Val _ body) =
  valBodyGlobals body ++ concatMap valGlobals (Foldable.toList body)
