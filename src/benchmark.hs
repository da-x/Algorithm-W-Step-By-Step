{-# LANGUAGE OverloadedStrings #-}
import Control.DeepSeq (rnf)
import Control.Exception (evaluate)
import Control.Lens (folded)
import Control.Lens.Operators
import Control.Monad.State (evalStateT)
import Criterion.Main (bench, defaultMain)
import Data.Map (Map)
import Data.Monoid (Monoid(..))
import Lamdu.Expr.Scheme (Scheme(..))
import Lamdu.Expr.Type (Type)
import Lamdu.Expr.Val (Val)
import Lamdu.Infer (TypeVars(..), infer, plType, initialContext, run, emptyScope)
import Text.PrettyPrint ((<+>))
import Text.PrettyPrint.HughesPJClass (Pretty(..))
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Lamdu.Expr.Pure as P
import qualified Lamdu.Expr.Scheme as S
import qualified Lamdu.Expr.Type as T
import qualified Lamdu.Expr.Val as V

-- TODO: $$ to be type-classed for TApp vs BApp
-- TODO: TCon "->" instead of TFun

lambda :: V.Var -> (Val () -> Val ()) -> Val ()
lambda varName mkBody = P.abs varName S.any (mkBody (P.var varName))

lambdaRecord :: [T.Tag] -> ([Val ()] -> Val ()) -> Val ()
lambdaRecord names mkBody =
  lambda "paramsRecord" $ \paramsRec ->
  mkBody $ map (P.getField paramsRec) names

whereItem :: V.Var -> Val () -> (Val () -> Val ()) -> Val ()
whereItem name val mkBody = lambda name mkBody $$ val

record :: [(T.Tag, Type)] -> Type
record = T.TRecord . foldr (uncurry T.CExtend) T.CEmpty

eRecord :: [(T.Tag, Val ())] -> Val ()
eRecord = foldr (uncurry P.recExtend) P.recEmpty

infixl 4 $$
($$) :: Val () -> Val () -> Val ()
($$) = P.app

infixl 4 $$:
($$:) :: Val () -> [(T.Tag, Val ())] -> Val ()
func $$: fields = func $$ eRecord fields

infixr 4 ~>
(~>) :: Type -> Type -> Type
(~>) = T.TFun

getDef :: V.GlobalId -> Val ()
getDef = P.global

literalInteger :: Integer -> Val ()
literalInteger = P.litInt

integerType :: Type
integerType = T.TInst "Int" Map.empty

boolType :: Type
boolType = T.TInst "Bool" Map.empty

forAll :: [T.Var Type] -> ([Type] -> Type) -> Scheme
forAll tvs mkType =
  Scheme (TypeVars (Set.fromList tvs) Set.empty) mempty $ mkType $ map T.TVar tvs

listOf :: Type -> Type
listOf = T.TInst "List" . Map.singleton "elem"

infixType :: Type -> Type -> Type -> Type
infixType a b c = record [("l", a), ("r", b)] ~> c

infixArgs :: Val () -> Val () -> Val ()
infixArgs l r = eRecord [("l", l), ("r", r)]

env :: Map V.GlobalId Scheme
env = Map.fromList
  [ ("fix",    forAll ["a"] $ \ [a] -> (a ~> a) ~> a)
  , ("if",     forAll ["a"] $ \ [a] -> record [("condition", boolType), ("then", a), ("else", a)] ~> a)
  , ("==",     forAll ["a"] $ \ [a] -> infixType a a boolType)
  , ("%",      forAll ["a"] $ \ [a] -> infixType a a a)
  , ("*",      forAll ["a"] $ \ [a] -> infixType a a a)
  , ("-",      forAll ["a"] $ \ [a] -> infixType a a a)
  , ("+",      forAll ["a"] $ \ [a] -> infixType a a a)
  , ("/",      forAll ["a"] $ \ [a] -> infixType a a a)
  , ("sum",    forAll ["a"] $ \ [a] -> listOf a ~> a)
  , ("filter", forAll ["a"] $ \ [a] -> record [("from", listOf a), ("predicate", a ~> boolType)] ~> listOf a)
  , (":",      forAll ["a"] $ \ [a] -> record [("head", a), ("tail", listOf a)] ~> listOf a)
  , ("[]",     forAll ["a"] $ \ [a] -> listOf a)
  , ("concat", forAll ["a"] $ \ [a] -> listOf (listOf a) ~> listOf a)
  , ("map",    forAll ["a", "b"] $ \ [a, b] -> record [("list", listOf a), ("mapping", a ~> b)] ~> listOf b)
  , ("..",     forAll [] $ \ [] -> infixType integerType integerType (listOf integerType))
  , ("||",     forAll [] $ \ [] -> infixType boolType boolType boolType)
  , ("head",   forAll ["a"] $ \ [a] -> listOf a ~> a)
  , ("negate", forAll ["a"] $ \ [a] -> a ~> a)
  , ("sqrt",   forAll ["a"] $ \ [a] -> a ~> a)
  , ("id",     forAll ["a"] $ \ [a] -> a ~> a)
  ]

list :: [Val ()] -> Val ()
list [] = getDef "[]"
list items@(_x:_) =
  foldr cons nil items
  where
    cons h t = getDef ":" $$: [("head", h), ("tail", t)]
    nil = getDef "[]"

factorialVal :: Val ()
factorialVal =
  getDef "fix" $$
  lambda "loop"
  ( \loop ->
    lambda "x" $ \x ->
    getDef "if" $$:
    [ ( "condition", getDef "==" $$
        infixArgs x (literalInteger 0) )
    , ( "then", literalInteger 1 )
    , ( "else", getDef "*" $$
        infixArgs x (loop $$ (getDef "-" $$ infixArgs x (literalInteger 1)))
      )
    ]
  )

euler1Val :: Val ()
euler1Val =
  getDef "sum" $$
  ( getDef "filter" $$:
    [ ("from", getDef ".." $$ infixArgs (literalInteger 1) (literalInteger 1000))
    , ( "predicate",
        lambda "x" $ \x ->
        getDef "||" $$ infixArgs
        ( getDef "==" $$ infixArgs
          (literalInteger 0) (getDef "%" $$ infixArgs x (literalInteger 3)) )
        ( getDef "==" $$ infixArgs
          (literalInteger 0) (getDef "%" $$ infixArgs x (literalInteger 5)) )
      )
    ]
  )

solveDepressedQuarticVal :: Val ()
solveDepressedQuarticVal =
  lambdaRecord ["e", "d", "c"] $ \[e, d, c] ->
  whereItem "solvePoly" (getDef "id")
  $ \solvePoly ->
  whereItem "sqrts"
  ( lambda "x" $ \x ->
    whereItem "r"
    ( getDef "sqrt" $$ x
    ) $ \r ->
    list [r, getDef "negate" $$ r]
  )
  $ \sqrts ->
  getDef "if" $$:
  [ ("condition", getDef "==" $$ infixArgs d (literalInteger 0))
  , ( "then",
      getDef "concat" $$
      ( getDef "map" $$:
        [ ("list", solvePoly $$ list [e, c, literalInteger 1])
        , ("mapping", sqrts)
        ]
      )
    )
  , ( "else",
      getDef "concat" $$
      ( getDef "map" $$:
        [ ( "list", sqrts $$ (getDef "head" $$ (solvePoly $$ list
            [ getDef "negate" $$ (d %* d)
            , (c %* c) %- (literalInteger 4 %* e)
            , literalInteger 2 %* c
            , literalInteger 1
            ]))
          )
        , ( "mapping",
            lambda "x" $ \x ->
            solvePoly $$ list
            [ (c %+ (x %* x)) %- (d %/ x)
            , literalInteger 2 %* x
            , literalInteger 2
            ]
          )
        ]
      )
    )
  ]
  where
    x %+ y = getDef "+" $$ infixArgs x y
    x %- y = getDef "-" $$ infixArgs x y
    x %* y = getDef "*" $$ infixArgs x y
    x %/ y = getDef "/" $$ infixArgs x y

benchInfer :: Val () -> IO ()
benchInfer e =
    case (`evalStateT` initialContext) $ run $ infer env emptyScope e of
    Left err -> fail $ show $ "error:" <+> pPrint err
    Right eTyped -> evaluate $ rnf $ eTyped ^.. folded . plType

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
