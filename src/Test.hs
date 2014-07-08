{-# LANGUAGE OverloadedStrings #-}
import Lamdu.Infer
import Text.PrettyPrint.HughesPJClass (Pretty(..))
import qualified Data.Map as M
import qualified Lamdu.Expr as E

eLet :: E.ValVar -> E.Val () -> E.Val () -> E.Val ()
eLet name val body = E.eApp (E.eAbs name body) val

exps :: [E.Val ()]
exps =
  [ eLet "id" (E.eAbs "x" (E.eVar "x")) $ E.eVar "id"

  , eLet "id" (E.eAbs "x" (E.eVar "x")) $
    E.eApp (E.eVar "id") (E.eVar "id")

  , eLet "id" (E.eAbs "x" (eLet "y" (E.eVar "x") (E.eVar "y"))) $
    E.eApp (E.eVar "id") (E.eVar "id")

  , eLet "id" (E.eAbs "x" (eLet "y" (E.eVar "x") (E.eVar "y"))) $
    E.eApp (E.eApp (E.eVar "id") (E.eVar "id")) $ E.eLitInt 2

  , eLet "id" (E.eAbs "x" (E.eApp (E.eVar "x") (E.eVar "x"))) $ E.eVar "id"

  , E.eAbs "m" $
    eLet "y" (E.eVar "m") $
    eLet "x" (E.eApp (E.eVar "y") (E.eLitInt 3)) $
    E.eVar "x"

  , E.eApp (E.eLitInt 2) $ E.eLitInt 2

  , E.eAbs "a" $
    eLet "x"
    ( E.eAbs "b" (eLet "y" (E.eAbs "c" (E.eApp (E.eVar "a") (E.eLitInt 1)))
      (E.eApp (E.eVar "y") (E.eLitInt 2)))
    ) $ E.eApp (E.eVar "x") $ E.eLitInt 3

  , E.eAbs "a" $ E.eAbs "b" $ E.eApp (E.eVar "b") $ E.eApp (E.eVar "a") $
    E.eApp (E.eVar "a") $ E.eVar "b"

  , E.eAbs "vec" $
    E.eRecExtend "newX" (E.eGetField (E.eVar "vec") "x") $
    E.eRecExtend "newY" (E.eGetField (E.eVar "vec") "y") E.eRecEmpty

  , eLet "vec"
    ( E.eRecExtend "x" (E.eLitInt 5) $
      E.eRecExtend "y" (E.eLitInt 7) E.eRecEmpty ) $
    E.eGetField (E.eVar "vec") "x"

  , eLet "vec"
    ( E.eRecExtend "x" (E.eLitInt 5) $
      E.eRecExtend "y" (E.eLitInt 7) E.eRecEmpty ) $
    E.eGetField (E.eVar "vec") "z"

  , E.eAbs "x" $ E.eRecExtend "prev" (E.eGetField (E.eVar "x") "cur") $ E.eVar "x"

  , E.eRecExtend "x" (E.eLitInt 2) $ E.eRecExtend "x" (E.eLitInt 3) $ E.eRecEmpty

  , E.eApp (E.eAbs "r" (E.eRecExtend "x" (E.eLitInt 2) (E.eVar "r"))) $
    E.eRecExtend "x" (E.eLitInt 3) $ E.eRecEmpty

  , eLet "f" (E.eAbs "r" (E.eRecExtend "x" (E.eLitInt 3) (E.eVar "r"))) $
    E.eApp (E.eVar "f") (E.eRecExtend "x" (E.eLitInt 2) E.eRecEmpty)
  ]

test :: E.Val () -> IO ()
test e =
    case typeInference M.empty e of
        Left err ->
          putStrLn $ show (pPrint e) ++ "\n " ++ err ++ "\n"
        Right (scheme, _) ->
          putStrLn $ show (pPrint e) ++ " :: " ++ show (pPrint scheme) ++ "\n"

main :: IO ()
main = mapM_ test exps
