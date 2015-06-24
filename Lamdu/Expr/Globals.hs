module Lamdu.Expr.Globals
    ( valGlobals
    ) where

import qualified Data.Foldable as Foldable
import           Lamdu.Expr.Val (Val(..))
import qualified Lamdu.Expr.Val as V

valBodyGlobals :: V.Body exp -> [V.GlobalId]
valBodyGlobals (V.BLeaf (V.LGlobal g)) = [g]
valBodyGlobals _ = []

valGlobals :: Val a -> [V.GlobalId]
valGlobals (Val _ body) =
    valBodyGlobals body ++ concatMap valGlobals (Foldable.toList body)
