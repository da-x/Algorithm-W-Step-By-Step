import AlgorithmW
import Pretty
import qualified Data.Map as Map

exp0 :: Exp ()
exp0  =  eLet "id" (eAbs "x" (eVar "x"))
          (eVar "id")

exp1 :: Exp ()
exp1  =  eLet "id" (eAbs "x" (eVar "x"))
          (eApp (eVar "id") (eVar "id"))

exp2 :: Exp ()
exp2  =  eLet "id" (eAbs "x" (eLet "y" (eVar "x") (eVar "y")))
          (eApp (eVar "id") (eVar "id"))

exp3 :: Exp ()
exp3  =  eLet "id" (eAbs "x" (eLet "y" (eVar "x") (eVar "y")))
          (eApp (eApp (eVar "id") (eVar "id")) (eLit (LInt 2)))

exp4 :: Exp ()
exp4  =  eLet "id" (eAbs "x" (eApp (eVar "x") (eVar "x")))
          (eVar "id")

exp5 :: Exp ()
exp5  =  eAbs "m" (eLet "y" (eVar "m")
                   (eLet "x" (eApp (eVar "y") (eLit (LChar 'x')))
                         (eVar "x")))

exp6 :: Exp ()
exp6  =  eApp (eLit (LInt 2)) (eLit (LInt 2))

exp7 :: Exp ()
exp7  =  eAbs "vec" $
         eRecExtend "newX" (eGetField (eVar "vec") "x") $
         eRecExtend "newY" (eGetField (eVar "vec") "y") $
         eRecEmpty

exp8 :: Exp ()
exp8  =  eLet
         "vec" ( eRecExtend "x" (eLit (LInt 5)) $
                 eRecExtend "y" (eLit (LInt 7)) $
                 eRecEmpty ) $
         eGetField (eVar "vec") "x"

exp9 :: Exp ()
exp9  =  eLet
         "vec" ( eRecExtend "x" (eLit (LInt 5)) $
                 eRecExtend "y" (eLit (LInt 7)) $
                 eRecEmpty ) $
         eGetField (eVar "vec") "z"

test :: Exp () -> IO ()
test e =
    case typeInference Map.empty e of
        Left err               ->  putStrLn $ show (prExp e) ++ "\n " ++ err ++ "\n"
        Right (Exp (t, _) _)   ->  putStrLn $ show (prExp e) ++ " :: " ++ show (prType t) ++ "\n"

main :: IO ()
main = mapM_ test [exp0, exp1, exp2, exp3, exp4, exp5, exp6, exp7, exp8, exp9]
