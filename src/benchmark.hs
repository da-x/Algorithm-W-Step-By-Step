{-# LANGUAGE OverloadedStrings #-}
import Control.DeepSeq (rnf)
import Control.Exception (evaluate)
import Control.Lens (folded)
import Control.Lens.Operators
import Control.Lens.Tuple
import Control.Monad.State (evalStateT)
import Criterion.Main (bench, defaultMain)
import Lamdu.Expr.Val (Val)
import Lamdu.Infer (infer, plType, initialContext, run, emptyScope)
import Text.PrettyPrint ((<+>))
import Text.PrettyPrint.HughesPJClass (Pretty(..))

import TestVals

benchInfer :: Val () -> IO ()
benchInfer e =
    case (`evalStateT` initialContext) $ run $ infer env emptyScope e of
    Left err -> fail $ show $ "error:" <+> pPrint err
    Right eTyped -> evaluate $ rnf $ eTyped ^.. folded . _1 . plType

benches :: [(String, IO ())]
benches =
  [ ("factorial", benchInfer factorialVal)
  , ("euler1", benchInfer euler1Val)
  , ("solveDepressedQuartic", benchInfer solveDepressedQuarticVal)
  ]

main :: IO ()
main =
  defaultMain $ map makeBench benches
  where
    makeBench (name, f) =
      bench name f
