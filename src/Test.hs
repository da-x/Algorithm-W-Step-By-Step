{-# LANGUAGE OverloadedStrings #-}
import Lamdu.Infer
import Text.PrettyPrint.HughesPJClass (Pretty(..))
import qualified Lamdu.Expr as E
import qualified Lamdu.Infer.Scope as Scope

exp0 :: E.Val ()
exp0  =  E.eLet "id" (E.eAbs "x" (E.eVar "x"))
          (E.eVar "id")

exp1 :: E.Val ()
exp1  =  E.eLet "id" (E.eAbs "x" (E.eVar "x"))
          (E.eApp (E.eVar "id") (E.eVar "id"))

exp2 :: E.Val ()
exp2  =  E.eLet "id" (E.eAbs "x" (E.eLet "y" (E.eVar "x") (E.eVar "y")))
          (E.eApp (E.eVar "id") (E.eVar "id"))

exp3 :: E.Val ()
exp3  =  E.eLet "id" (E.eAbs "x" (E.eLet "y" (E.eVar "x") (E.eVar "y")))
          (E.eApp (E.eApp (E.eVar "id") (E.eVar "id")) (E.eLitInt 2))

exp4 :: E.Val ()
exp4  =  E.eLet "id" (E.eAbs "x" (E.eApp (E.eVar "x") (E.eVar "x")))
          (E.eVar "id")

exp5 :: E.Val ()
exp5  =  E.eAbs "m" (E.eLet "y" (E.eVar "m")
                   (E.eLet "x" (E.eApp (E.eVar "y") (E.eLitInt 3))
                         (E.eVar "x")))

exp6 :: E.Val ()
exp6  =  E.eApp (E.eLitInt 2) (E.eLitInt 2)

exp7 :: E.Val ()
exp7  =  E.eAbs "a" (E.eLet "x" (E.eAbs "b" (E.eLet "y" (E.eAbs "c" (E.eApp (E.eVar "a") (E.eLitInt 1)))
                                     (E.eApp (E.eVar "y") (E.eLitInt 2))))
                 (E.eApp (E.eVar "x") (E.eLitInt 3)))

exp8 :: E.Val ()
exp8  =  E.eAbs "a" $ E.eAbs "b" $ E.eApp (E.eVar "b") $ E.eApp (E.eVar "a") $ E.eApp (E.eVar "a") (E.eVar "b")

exp9 :: E.Val ()
exp9  =  E.eAbs "vec" $
         E.eRecExtend "newX" (E.eGetField (E.eVar "vec") "x") $
         E.eRecExtend "newY" (E.eGetField (E.eVar "vec") "y") E.eRecEmpty

exp10 :: E.Val ()
exp10  =  E.eLet
         "vec" ( E.eRecExtend "x" (E.eLitInt 5) $
                 E.eRecExtend "y" (E.eLitInt 7) E.eRecEmpty ) $
         E.eGetField (E.eVar "vec") "x"

exp11 :: E.Val ()
exp11  =  E.eLet
         "vec" ( E.eRecExtend "x" (E.eLitInt 5) $
                 E.eRecExtend "y" (E.eLitInt 7) E.eRecEmpty ) $
         E.eGetField (E.eVar "vec") "z"

exp12 :: E.Val ()
exp12  =  E.eAbs "x" $
          E.eRecExtend "prev" (E.eGetField (E.eVar "x") "cur") $ E.eVar "x"

exp13 :: E.Val ()
exp13  =  E.eRecExtend "x" (E.eLitInt 2) $
          E.eRecExtend "x" (E.eLitInt 3) $
          E.eRecEmpty

test :: E.Val () -> IO ()
test e =
    case typeInference Scope.empty e of
        Left err ->
          putStrLn $ show (pPrint e) ++ "\n " ++ err ++ "\n"
        Right (E.Val (t, _) _) ->
          putStrLn $ show (pPrint e) ++ " :: " ++ show (pPrint t) ++ "\n"

main :: IO ()
main =
  mapM_ test
  [ exp0
  , exp1
  , exp2
  , exp3
  , exp4
  , exp5
  , exp6
  , exp7
  , exp8
  , exp9
  , exp10
  , exp11
  , exp12
  , exp13
  ]
