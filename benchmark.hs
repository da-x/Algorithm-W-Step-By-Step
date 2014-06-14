import AlgorithmW hiding (main)
import Control.Monad
import Criterion.Main
import Data.Map (Map)
import qualified Data.Map as Map

-- TODO: $$ to be type-classed for TApp vs EApp
-- TODO: TCon "->" instead of TFun

lambda :: String -> dummy -> (Exp -> Exp) -> Exp
lambda varName _ mkBody = EAbs varName (mkBody (EVar varName))

lambdaRecord _ params mkBody =
  foldr EAbs body names
  where
    body = mkBody $ map EVar names
    names = map fst params

whereItem :: String -> Exp -> (Exp -> Exp) -> Exp
whereItem name val mkBody = lambda name () mkBody $$ val

-- Just use literal int 0 as a hack instead of passing a type to the
-- type-var application
typeVarArg :: Exp
typeVarArg = ELit $ LInt 0

asHole :: a -> Exp
asHole = const typeVarArg

infixl 4 $$
($$) :: Exp -> Exp -> Exp
($$) = EApp

infixl 4 $$:
($$:) :: Exp -> [Exp] -> Exp
($$:) = foldl EApp

infixr 4 ~>
(~>) :: Type -> Type -> Type
(~>) = TFun

getDef :: String -> Exp
getDef = EVar

literalInteger :: Integer -> Exp
literalInteger = ELit . LInt

integerType :: Type
integerType = TCon "Int"

boolType :: Type
boolType = TCon "Bool"

forAll :: [String] -> ([Type] -> Type) -> Scheme
forAll names mkType = Scheme names $ mkType $ map TVar names

listOf = TApp (TCon "List")

env :: Map String Scheme
env = Map.fromList
  [ ("fix",    forAll ["typ", "a"] $ \ [typ, a] -> typ ~> (a ~> a) ~> a)
  , ("if",     forAll ["typ", "a"] $ \ [typ, a] -> typ ~> boolType ~> a ~> a ~> a)
  , ("==",     forAll ["typ", "a"] $ \ [typ, a] -> typ ~> a ~> a ~> boolType)
  , ("%",      forAll ["typ", "a"] $ \ [typ, a] -> typ ~> a ~> a ~> a)
  , ("*",      forAll ["typ", "a"] $ \ [typ, a] -> typ ~> a ~> a ~> a)
  , ("-",      forAll ["typ", "a"] $ \ [typ, a] -> typ ~> a ~> a ~> a)
  , ("+",      forAll ["typ", "a"] $ \ [typ, a] -> typ ~> a ~> a ~> a)
  , ("/",      forAll ["typ", "a"] $ \ [typ, a] -> typ ~> a ~> a ~> a)
  , ("sum",    forAll ["typ", "a"] $ \ [typ, a] -> typ ~> listOf a ~> a)
  , ("filter", forAll ["typ", "a"] $ \ [typ, a] -> typ ~> listOf a ~> (a ~> boolType) ~> listOf a)
  , (":",      forAll ["typ", "a"] $ \ [typ, a] -> typ ~> a ~> listOf a ~> listOf a)
  , ("[]",     forAll ["typ", "a"] $ \ [typ, a] -> typ ~> listOf a)
  , ("concat", forAll ["typ", "a"] $ \ [typ, a] -> typ ~> listOf (listOf a) ~> listOf a)
  , ("map",    forAll ["typ", "a", "b"] $ \ [typ, a, b] -> typ ~> typ ~> listOf a ~> (a ~> b) ~> listOf b)
  , ("..",     forAll [] $ \ [] -> integerType ~> integerType ~> listOf integerType)
  , ("||",     forAll [] $ \ [] -> boolType ~> boolType ~> boolType)
  , ("head",   forAll ["typ", "a"] $ \ [typ, a] -> typ ~> listOf a ~> a)
  , ("negate", forAll ["typ", "a"] $ \ [typ, a] -> typ ~> a ~> a)
  , ("sqrt",   forAll ["typ", "a"] $ \ [typ, a] -> typ ~> a ~> a)
  , ("id",     forAll ["typ", "a"] $ \ [typ, a] -> typ ~> a ~> a)
  ]

list :: [Exp] -> Exp
list [] = getDef "[]" $$ typeVarArg
list items@(x:_) =
  foldr cons nil items
  where
    typ = typeVarArg -- inferredTypeAsHole x
    cons h t = getDef ":" $$ typ $$: [h, t]
    nil = getDef "[]" $$ typ

factorialExpr =
  getDef "fix" $$ facType $$
  lambda "loop" facType
  ( \loop ->
    lambda "x" iInt $ \x ->
    getDef "if" $$ iInt $$:
    [ getDef "==" $$ iInt $$:
      [x, literalInteger 0]
    , literalInteger 1
    , getDef "*" $$ iInt $$:
      [ x
      , loop $$ (getDef "-" $$ iInt $$: [x, literalInteger 1])
      ]
    ]
  )
  where
    facType = asHole (integerType ~> integerType)
    iInt = asHole integerType

euler1Expr =
  getDef "sum" $$ iInt $$
  ( getDef "filter" $$ iInt $$:
    [ getDef ".." $$: [literalInteger 1, literalInteger 1000]
    , lambda "x" iInt $ \x ->
      getDef "||" $$:
      [ getDef "==" $$ iInt $$:
        [ literalInteger 0, getDef "%" $$ iInt $$: [x, literalInteger 3] ]
      , getDef "==" $$ iInt $$:
        [ literalInteger 0, getDef "%" $$ iInt $$: [x, literalInteger 5] ]
      ]
    ]
  )
  where
    iInt = asHole integerType

solveDepressedQuarticExpr :: Exp
solveDepressedQuarticExpr =
  lambdaRecord "params"
  [ ("e", iInt)
  , ("d", iInt)
  , ("c", iInt)
  ] $ \[e, d, c] ->
  whereItem "solvePoly" ( getDef "id" $$ iListInt )
  $ \solvePoly ->
  whereItem "sqrts"
  ( lambda "x" iInt $ \x ->
    whereItem "r"
    ( getDef "sqrt" $$ iInt $$ x
    ) $ \r ->
    list [r, getDef "negate" $$ iInt $$ r]
  )
  $ \sqrts ->
  getDef "if" $$ iListInt $$:
  [ getDef "==" $$ iInt $$: [d, literalInteger 0]
  , getDef "concat" $$ iInt $$
    ( getDef "map" $$ iInt $$ iListInt $$:
      [ solvePoly $$ list [e, c, literalInteger 1]
      , sqrts
      ]
    )
  , getDef "concat" $$ iInt $$
    ( getDef "map" $$ iInt $$ iListInt $$:
      [ sqrts $$ (getDef "head" $$ iInt $$ (solvePoly $$ list
        [ getDef "negate" $$ iInt $$ (d %* d)
        , (c %* c) %- (literalInteger 4 %* e)
        , literalInteger 2 %* c
        , literalInteger 1
        ]))
      , lambda "x" iInt $ \x ->
        solvePoly $$ list
        [ (c %+ (x %* x)) %- (d %/ x)
        , literalInteger 2 %* x
        , literalInteger 2
        ]
      ]
    )
  ]
  where
    iInt = asHole integerType
    iListInt = asHole $ listOf integerType
    x %+ y = getDef "+" $$ iInt $$: [x, y]
    x %- y = getDef "-" $$ iInt $$: [x, y]
    x %* y = getDef "*" $$ iInt $$: [x, y]
    x %/ y = getDef "/" $$ iInt $$: [x, y]

infer :: Exp -> IO String
infer e =
    do  res <- runTI (typeInference env e)
        case res of
          Left err  ->  fail $ "error: " ++ err
          Right t   ->  return $ show e ++ " :: " ++ show t

benches :: [(String, IO String)]
benches =
  [ ("factorial", infer factorialExpr)
  , ("euler1", infer euler1Expr)
  , ("solveDepressedQuartic", infer solveDepressedQuarticExpr)
  ]

main :: IO ()
main = do
  -- putStrLn =<< infer solveDepressedQuarticExpr
  defaultMain $ map makeBench benches
  where
    makeBench (name, f) =
      bench name f
