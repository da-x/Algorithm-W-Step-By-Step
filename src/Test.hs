{-# LANGUAGE OverloadedStrings #-}

import           Control.Applicative
import           Control.Arrow ((&&&))
import           Control.Lens (zoom)
import           Control.Lens.Operators
import           Control.Lens.Tuple
import           Control.Monad.State (StateT(..), state, runState, evalState, modify, get)
import qualified Data.Map as M
import           Data.String (IsString(..))
import           Data.Traversable (traverse)
import qualified Lamdu.Expr.Pure as P
import           Lamdu.Expr.Type ((~>), Type(..), Composite(..))
import qualified Lamdu.Expr.Type as T
import           Lamdu.Expr.Val (Val(..))
import qualified Lamdu.Expr.Val as V
import           Lamdu.Infer
import qualified Lamdu.Infer.Recursive as Recursive
import           Lamdu.Infer.Unify
import qualified Lamdu.Infer.Update as Update
import qualified Lamdu.Suggest as Suggest
import           Text.PrettyPrint ((<>), (<+>), ($+$))
import qualified Text.PrettyPrint as PP
import           Text.PrettyPrint.HughesPJClass (Pretty(..))

import           TestVals

{-# ANN module ("HLint: ignore Use const" :: String) #-}

exps :: [Val ()]
exps =
    [ eLet "id" (lambda "x" id) id

    , eLet "id" (lambda "x" id) $ \id' -> id' $$ id'

    , eLet "id" (lambda "x" (\x -> eLet "y" x id)) $ \id' -> id' $$ id'

    , eLet "id" (lambda "x" (\x -> eLet "y" x id)) $ \id' -> id' $$ id' $$ P.litInt 2

    , eLet "id" (lambda "x" (\x -> x $$ x)) id

    , lambda "m" $ \m ->
        eLet "y" m $ \y ->
        eLet "x" (y $$ P.litInt 3) id

    , P.litInt 2 $$ P.litInt 2

    , lambda "a" $ \a ->
        eLet "x"
        ( lambda "b"
            ( \_ -> eLet "y" (lambda "c" (\_ -> a $$ P.litInt 1))
                (\y -> y $$ P.litInt 2) )
        ) $ \x -> x $$ P.litInt 3

    , lambda "a" $ \a -> lambda "b" $ \b -> b $$ (a $$ (a $$ b))

    , lambda "vec" $ \vec ->
        "newX" $= (vec $. "x") $
        "newY" $= (vec $. "y") $
        P.recEmpty

    , eLet "vec" ("x" $= P.litInt 5 $ "y" $= P.litInt 7 $ P.recEmpty) ($. "x")

    , eLet "vec" ("x" $= P.litInt 5 $ "y" $= P.litInt 7 $ P.recEmpty) ($. "z")

    , lambda "x" $ \x -> "prev" $= (x $. "cur") $ x

    , "x" $= P.litInt 2 $ "x" $= P.litInt 3 $ P.recEmpty

    , lambda "r" ("x" $= P.litInt 2) $$ ("x" $= P.litInt 3) P.recEmpty

    , eLet "f" (lambda "r" ("x" $= P.litInt 3)) $
        \f -> f $$ ("x" $= P.litInt 2) P.recEmpty

    , "x" $= P.litInt 1 $ P.hole

    , lambda "x" $ \x -> list [x, x]

    , factorialVal, euler1Val, solveDepressedQuarticVal

    , eLet "open"
        ( lambda "x" $ \x ->
            eLet "y" (x $. "x") $
            \_y -> x ) $ \open ->
        open $$ ("x" $= P.litInt 0 $ P.recEmpty)

    , P.global "fix" $$ lambda "f"
        ( \f -> P.hole $$ (f $$ (f $$ (P.global "zipWith" $$ P.hole $$ P.hole $$ P.hole)))
        )

    , list [P.inject "x" (P.litInt 1), P.inject "y" (P.litInt 2), P.inject "x" P.hole]
    , P.absurd

    , lambda "nothing" $ \nothing ->
      lambda "just" $ \just ->
      P._case "Nothing" (lambda "_" (const nothing))
      ( P._case "Just" just $
        P.absurd
      )
    ]

recurseVar :: V.Var
recurseVar = V.Var "Recurse"

recurse :: V.Val ()
recurse = P.var recurseVar

recursiveExps :: [Val ()]
recursiveExps =
    [ eLet "id" (lambda "x" id) id
    , recurse
    ]

suggestTypes :: [Type]
suggestTypes =
    [ T.int ~> T.int
    , T.int ~> T.int ~> T.int
    , TRecord CEmpty
    , TVar "a" ~> TRecord CEmpty
    , TVar "a" ~> TRecord (CExtend "x" T.int (CExtend "y" (T.int ~> T.int) CEmpty))
    , TVar "a" ~> TRecord (CExtend "x" (T.int ~> T.int) (CExtend "y" (T.int ~> T.int) CEmpty))
    ]

unifies :: [(Type, Type)]
unifies =
    [ ( ( TRecord $
          CExtend "z" (TVar "b") $
          CExtend "x" (TVar "c") $
          CExtend "y" (TVar "d") $
          CEmpty
        ) ~> TVar "e"
      , ( TRecord $
          CExtend "x" T.int $
          CExtend "y" (TVar "a") $
          CExtend "z" (TVar "a") $
          CVar "r"
        ) ~> TVar "a"
      )
    ]

runAndPrint :: Val a -> Infer (Type, Val (Payload, b)) -> IO ()
runAndPrint e =
    printResult . (`runStateT` initialContext) . run
    where
        printResult (Left err) = print (V.pPrintUnannotated e $+$ pPrint err)
        printResult (Right ((typ, val), finalContext)) =
            do
                let scheme = makeScheme finalContext typ
                print $ V.pPrintUnannotated val <+> PP.text "::" <+> pPrint scheme
                let next = modify (+1) >> get
                    tag x =
                      do  n <- zoom _1 next
                          zoom _2 $ modify $ M.insert n x
                          return n
                let (taggedVal, (_, types)) =
                      runState (traverse (tag . _plType . fst) val) (0::Int, M.empty)
                print $ pPrint taggedVal
                let indent = PP.hcat $ replicate 4 PP.space
                mapM_ (\(k, t) -> print $ indent <> pPrint k <+> "=" <+> pPrint t) $ M.toList types

inferType :: Scope -> Val a -> Infer (Type, Val (Payload, a))
inferType scope e =
    do
        e' <- infer env scope e
        let t = e' ^. V.payload . _1 . plType
        return (t, e')

test :: Val () -> IO ()
test e = runAndPrint e $ inferType emptyScope e

inferInto :: Payload -> Val a -> Infer (Type, Val (Payload, a))
inferInto pl val =
    do
        (inferredType, inferredVal) <- inferType (pl ^. plScope) val
        unify inferredType (pl ^. plType)
        (,) inferredType <$> Update.inferredVal inferredVal & Update.liftInfer

testRecursive :: Val () -> IO ()
testRecursive e =
    runAndPrint e $
    do
        recursivePos <- Recursive.inferEnv recurseVar emptyScope
        inferInto recursivePos e

testSuggest :: Type -> IO ()
testSuggest typ =
    print $ V.pPrintUnannotated val <+> PP.text "suggested by" <+> pPrint typ
    where
        val =
            evalState (Suggest.suggestValueWith fresh typ)
            [fromString ('x':show (n::Integer)) | n <- [0..]]
        fresh = state $ head &&& tail

testUnify :: Type -> Type -> IO ()
testUnify x y =
    do
        unify x y
        Update.update x & Update.liftInfer
    & printResult . (`runStateT` initialContext) . run
    where
        printCase = pPrint x <+> PP.text "=" <+> pPrint y <+> PP.text ":"
        printResult (Left err) = print $ printCase $+$ pPrint err
        printResult (Right (res, _ctx)) = print $ printCase $+$ pPrint res

main :: IO ()
main =
    do
        putStrLn "Expression types:"
        mapM_ test exps
        putStrLn "Recursive expression types:"
        mapM_ testRecursive recursiveExps
        putStrLn "Suggested values from types:"
        mapM_ testSuggest suggestTypes
        putStrLn "Unify:"
        mapM_ (uncurry testUnify) unifies
