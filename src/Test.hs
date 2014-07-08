{-# LANGUAGE OverloadedStrings #-}
import Control.Lens (zoom)
import Control.Lens.Tuple
import Control.Monad.State (runState, modify', get)
import Data.Traversable (traverse)
import Lamdu.Infer
import Text.PrettyPrint ((<>), (<+>))
import Text.PrettyPrint.HughesPJClass (Pretty(..))
import qualified Data.Map as M
import qualified Lamdu.Expr as E
import qualified Text.PrettyPrint as PP

eLet :: E.ValVar -> E.Val () -> (E.Val () -> E.Val ()) -> E.Val ()
eLet name val mkBody = E.eApp (E.eAbs name body) val
  where
    body = mkBody $ E.eVar name

lambda :: E.ValVar -> (E.Val () -> E.Val ()) -> E.Val ()
lambda name mkBody = E.eAbs name $ mkBody $ E.eVar name

int :: Integer -> E.Val ()
int = E.eLitInt

emptyRec :: E.Val ()
emptyRec = E.eRecEmpty

infixl 4 $$
($$) :: E.Val () -> E.Val () -> E.Val ()
($$) = E.eApp

infixl 9 $.
($.) :: E.Val () -> E.Tag -> E.Val ()
($.) = E.eGetField

infixl 3 $=
($=) :: E.Tag -> E.Val () -> E.Val () -> E.Val ()
($=) = E.eRecExtend

exps :: [E.Val ()]
exps =
  [ eLet "id" (lambda "x" id) $ id

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

  , (lambda "r" ("x" $= int 2)) $$ ("x" $= int 3) emptyRec

  , eLet "f" (lambda "r" ("x" $= int 3)) $
    \f -> f $$ ("x" $= int 2) emptyRec
  ]

test :: E.Val () -> IO ()
test e =
    case typeInference M.empty e of
        Left err ->
          putStrLn $ show (pPrintPureVal e) ++ "\n " ++ err
        Right (scheme, val) -> do
          putStrLn $ show (pPrintValUnannotated val <+> PP.text "::" <+> pPrint scheme)
          let next = modify' (+1) >> get
              tag x =
                do  n <- zoom _1 next
                    zoom _2 $ modify' $ M.insert n x
                    return n
          let (taggedVal, (_, types)) = runState (traverse (tag . fst) val) (0::Int, M.empty)
          print $ pPrint taggedVal
          let indent = PP.hcat $ replicate 4 PP.space
          mapM_ (\(k, t) -> print $ indent <> pPrint k <+> "=" <+> pPrint t) $ M.toList types

main :: IO ()
main = mapM_ test exps
