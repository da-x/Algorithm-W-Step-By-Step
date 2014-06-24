import Lamdu.Expr
import Lamdu.Infer
import Text.PrettyPrint.HughesPJClass (Pretty(..))
import qualified Lamdu.Infer.Scope as Scope

exp0 :: Expr ()
exp0  =  eLet "id" (eAbs "x" (eVar "x"))
          (eVar "id")

exp1 :: Expr ()
exp1  =  eLet "id" (eAbs "x" (eVar "x"))
          (eApp (eVar "id") (eVar "id"))

exp2 :: Expr ()
exp2  =  eLet "id" (eAbs "x" (eLet "y" (eVar "x") (eVar "y")))
          (eApp (eVar "id") (eVar "id"))

exp3 :: Expr ()
exp3  =  eLet "id" (eAbs "x" (eLet "y" (eVar "x") (eVar "y")))
          (eApp (eApp (eVar "id") (eVar "id")) (eLit (LInt 2)))

exp4 :: Expr ()
exp4  =  eLet "id" (eAbs "x" (eApp (eVar "x") (eVar "x")))
          (eVar "id")

exp5 :: Expr ()
exp5  =  eAbs "m" (eLet "y" (eVar "m")
                   (eLet "x" (eApp (eVar "y") (eLit (LChar 'x')))
                         (eVar "x")))

exp6 :: Expr ()
exp6  =  eApp (eLit (LInt 2)) (eLit (LInt 2))

exp7 :: Expr ()
exp7  =  eAbs "vec" $
         eRecExtend "newX" (eGetField (eVar "vec") "x") $
         eRecExtend "newY" (eGetField (eVar "vec") "y") $
         eRecEmpty

exp8 :: Expr ()
exp8  =  eLet
         "vec" ( eRecExtend "x" (eLit (LInt 5)) $
                 eRecExtend "y" (eLit (LInt 7)) $
                 eRecEmpty ) $
         eGetField (eVar "vec") "x"

exp9 :: Expr ()
exp9  =  eLet
         "vec" ( eRecExtend "x" (eLit (LInt 5)) $
                 eRecExtend "y" (eLit (LInt 7)) $
                 eRecEmpty ) $
         eGetField (eVar "vec") "z"

test :: Expr () -> IO ()
test e =
    case typeInference Scope.empty e of
        Left err ->
          putStrLn $ show (pPrint e) ++ "\n " ++ err ++ "\n"
        Right (Expr (t, _) _) ->
          putStrLn $ show (pPrint e) ++ " :: " ++ show (pPrint t) ++ "\n"

main :: IO ()
main = mapM_ test [exp0, exp1, exp2, exp3, exp4, exp5, exp6, exp7, exp8, exp9]
