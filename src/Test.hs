{-# LANGUAGE OverloadedStrings #-}
import Control.Lens (zoom)
import Control.Lens.Operators
import Control.Lens.Tuple
import Control.Monad.State (evalStateT, runState, modify', get)
import Data.Traversable (traverse)
import Lamdu.Infer
import Lamdu.Expr.Pretty
import Text.PrettyPrint ((<>), (<+>), ($+$))
import Text.PrettyPrint.HughesPJClass (Pretty(..))
import qualified Data.Map as M
import qualified Lamdu.Expr as E
import qualified Lamdu.Expr.Pure as P
import qualified Text.PrettyPrint as PP

eLet :: E.ValVar -> E.Val () -> (E.Val () -> E.Val ()) -> E.Val ()
eLet name val mkBody = P.app (P.abs name body) val
  where
    body = mkBody $ P.var name

lambda :: E.ValVar -> (E.Val () -> E.Val ()) -> E.Val ()
lambda name mkBody = P.abs name $ mkBody $ P.var name

int :: Integer -> E.Val ()
int = P.litInt

emptyRec :: E.Val ()
emptyRec = P.recEmpty

infixl 4 $$
($$) :: E.Val () -> E.Val () -> E.Val ()
($$) = P.app

infixl 9 $.
($.) :: E.Val () -> E.Tag -> E.Val ()
($.) = P.getField

infixl 3 $=
($=) :: E.Tag -> E.Val () -> E.Val () -> E.Val ()
($=) = P.recExtend

exps :: [E.Val ()]
exps =
  [ eLet "id" (lambda "x" id) id

  , eLet "id" (lambda "x" id) $ \id' -> id' $$ id'

  , eLet "id" (lambda "x" (\x -> eLet "y" x id)) $ \id' -> id' $$ id'

  , eLet "id" (lambda "x" (\x -> eLet "y" x id)) $ \id' -> id' $$ id' $$ int 2

  , eLet "id" (lambda "x" (\x -> x $$ x)) id

  , lambda "m" $ \m ->
    eLet "y" m $ \y ->
    eLet "x" (y $$ int 3) id

  , int 2 $$ int 2

  , lambda "a" $ \a ->
    eLet "x"
    ( lambda "b"
      ( \_ -> eLet "y" (lambda "c" (\_ -> a $$ int 1))
        (\y -> y $$ int 2) )
    ) $ \x -> x $$ int 3

  , lambda "a" $ \a -> lambda "b" $ \b -> b $$ (a $$ (a $$ b))

  , lambda "vec" $ \vec ->
    "newX" $= (vec $. "x") $
    "newY" $= (vec $. "y") $
    emptyRec

  , eLet "vec" ("x" $= int 5 $ "y" $= int 7 $ emptyRec) ($. "x")

  , eLet "vec" ("x" $= int 5 $ "y" $= int 7 $ emptyRec) ($. "z")

  , lambda "x" $ \x -> "prev" $= (x $. "cur") $ x

  , "x" $= int 2 $ "x" $= int 3 $ emptyRec

  , lambda "r" ("x" $= int 2) $$ ("x" $= int 3) emptyRec

  , eLet "f" (lambda "r" ("x" $= int 3)) $
    \f -> f $$ ("x" $= int 2) emptyRec
  ]

test :: E.Val () -> IO ()
test e =
    case inferredType of
        Left err ->
          putStrLn $ show (pPrintValUnannotated e $+$ pPrint err)
        Right (scheme, val) -> do
          print $ pPrintValUnannotated val <+> PP.text "::" <+> pPrint scheme
          let next = modify' (+1) >> get
              tag x =
                do  n <- zoom _1 next
                    zoom _2 $ modify' $ M.insert n x
                    return n
          let (taggedVal, (_, types)) = runState (traverse (tag . _plType) val) (0::Int, M.empty)
          print $ pPrint taggedVal
          let indent = PP.hcat $ replicate 4 PP.space
          mapM_ (\(k, t) -> print $ indent <> pPrint k <+> "=" <+> pPrint t) $ M.toList types
    where
        inferredType =
          (`evalStateT` initialContext) . run $ do
            e' <- infer M.empty emptyScope e
            s <- makeScheme $ E.valPayload e' ^. plType
            return (s, e')

main :: IO ()
main = mapM_ test exps
