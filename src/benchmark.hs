import Infer
import Expr
import Control.DeepSeq (rnf)
import Control.Exception (evaluate)
import Control.Lens (folded)
import Control.Lens.Operators
import Control.Lens.Tuple
import Control.Monad
import Criterion.Main
import Data.Map (Map)
import qualified Data.Map as Map
import qualified Scope as Scope

-- TODO: $$ to be type-classed for TApp vs EApp
-- TODO: TCon "->" instead of TFun

lambda :: String -> (Expr () -> Expr ()) -> Expr ()
lambda varName mkBody = eAbs varName (mkBody (eVar varName))

lambdaRecord :: [String] -> ([Expr ()] -> Expr ()) -> Expr ()
lambdaRecord names mkBody =
  lambda "paramsRecord" $ \paramsRec ->
  mkBody $ map (eGetField paramsRec) names

whereItem :: String -> Expr () -> (Expr () -> Expr ()) -> Expr ()
whereItem name val mkBody = lambda name mkBody $$ val

record :: [(String, Type)] -> Type
record = foldr (uncurry TRecExtend) TRecEmpty

eRecord :: [(String, Expr ())] -> Expr ()
eRecord = foldr (uncurry eRecExtend) eRecEmpty

infixl 4 $$
($$) :: Expr () -> Expr () -> Expr ()
($$) = eApp

infixl 4 $$:
($$:) :: Expr () -> [(String, Expr ())] -> Expr ()
func $$: fields = func $$ eRecord fields

infixr 4 ~>
(~>) :: Type -> Type -> Type
(~>) = TFun

getDef :: String -> Expr ()
getDef = eVar

literalInteger :: Integer -> Expr ()
literalInteger = eLit . LInt

integerType :: Type
integerType = TCon "Int"

boolType :: Type
boolType = TCon "Bool"

forAll :: [String] -> ([Type] -> Type) -> Scheme
forAll names mkType = Scheme names $ mkType $ map TVar names

listOf :: Type -> Type
listOf = TApp (TCon "List")

infixType :: Type -> Type -> Type -> Type
infixType a b c = record [("l", a), ("r", b)] ~> c

infixArgs :: Expr () -> Expr () -> Expr ()
infixArgs l r = eRecord [("l", l), ("r", r)]

env :: Map String Scheme
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

list :: [Expr ()] -> Expr ()
list [] = getDef "[]"
list items@(_x:_) =
  foldr cons nil items
  where
    cons h t = getDef ":" $$: [("head", h), ("tail", t)]
    nil = getDef "[]"

factorialExpr :: Expr ()
factorialExpr =
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

euler1Expr :: Expr ()
euler1Expr =
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

solveDepressedQuarticExpr :: Expr ()
solveDepressedQuarticExpr =
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

infer :: Expr () -> IO String
infer e =
    case typeInference (Scope.fromTypeMap env) e of
    Left err ->  fail $ "error: " ++ err
    Right eTyped ->
      do  _ <- evaluate $ rnf $ eTyped ^.. folded . _1
          return $ show e ++ " :: " ++ show (eTyped ^. expPayload . _1)

benches :: [(String, IO String)]
benches =
  [ ("factorial", infer factorialExpr)
  , ("euler1", infer euler1Expr)
  , ("solveDepressedQuartic", infer solveDepressedQuarticExpr)
  ]

main :: IO ()
main = do
  mapM_ (putStrLn <=< snd) benches
  defaultMain $ map makeBench benches
  where
    makeBench (name, f) =
      bench name f
