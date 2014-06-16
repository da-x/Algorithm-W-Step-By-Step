import AlgorithmW hiding (main)
import Criterion.Main
import Data.Map (Map)
import qualified Data.Map as Map

-- TODO: $$ to be type-classed for TApp vs EApp
-- TODO: TCon "->" instead of TFun

lambda :: String -> dummy -> (Exp -> Exp) -> Exp
lambda varName _ mkBody = EAbs varName (mkBody (EVar varName))

lambdaRecord :: dummy -> [(String, b)] -> ([Exp] -> Exp) -> Exp
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

record :: [(String, Type)] -> Type
record = foldr (uncurry TRecExtend) TRecEmpty

eRecord :: [(String, Exp)] -> Exp
eRecord = foldr (uncurry ERecExtend) ERecEmpty

infixl 4 $$
($$) :: Exp -> Exp -> Exp
($$) = EApp

infixl 4 $$:
($$:) :: Exp -> [(String, Exp)] -> Exp
func $$: fields = func $$ eRecord fields

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

listOf :: Type -> Type
listOf = TApp (TCon "List")

infixType :: Type -> Type -> Type -> Type
infixType a b c = record [("l", a), ("r", b)] ~> c

infixArgs :: Exp -> Exp -> Exp
infixArgs l r = eRecord [("l", l), ("r", r)]

env :: Map String Scheme
env = Map.fromList
  [ ("fix",    forAll ["typ", "a"] $ \ [typ, a] -> typ ~> (a ~> a) ~> a)
  , ("if",     forAll ["typ", "a"] $ \ [typ, a] -> typ ~> record [("condition", boolType), ("then", a), ("else", a)] ~> a)
  , ("==",     forAll ["typ", "a"] $ \ [typ, a] -> typ ~> infixType a a boolType)
  , ("%",      forAll ["typ", "a"] $ \ [typ, a] -> typ ~> infixType a a a)
  , ("*",      forAll ["typ", "a"] $ \ [typ, a] -> typ ~> infixType a a a)
  , ("-",      forAll ["typ", "a"] $ \ [typ, a] -> typ ~> infixType a a a)
  , ("+",      forAll ["typ", "a"] $ \ [typ, a] -> typ ~> infixType a a a)
  , ("/",      forAll ["typ", "a"] $ \ [typ, a] -> typ ~> infixType a a a)
  , ("sum",    forAll ["typ", "a"] $ \ [typ, a] -> typ ~> listOf a ~> a)
  , ("filter", forAll ["typ", "a"] $ \ [typ, a] -> typ ~> record [("from", listOf a), ("predicate", a ~> boolType)] ~> listOf a)
  , (":",      forAll ["typ", "a"] $ \ [typ, a] -> typ ~> record [("head", a), ("tail", listOf a)] ~> listOf a)
  , ("[]",     forAll ["typ", "a"] $ \ [typ, a] -> typ ~> listOf a)
  , ("concat", forAll ["typ", "a"] $ \ [typ, a] -> typ ~> listOf (listOf a) ~> listOf a)
  , ("map",    forAll ["typ", "a", "b"] $ \ [typ, a, b] -> typ ~> typ ~> record [("list", listOf a), ("mapping", a ~> b)] ~> listOf b)
  , ("..",     forAll [] $ \ [] -> infixType integerType integerType (listOf integerType))
  , ("||",     forAll [] $ \ [] -> infixType boolType boolType boolType)
  , ("head",   forAll ["typ", "a"] $ \ [typ, a] -> typ ~> listOf a ~> a)
  , ("negate", forAll ["typ", "a"] $ \ [typ, a] -> typ ~> a ~> a)
  , ("sqrt",   forAll ["typ", "a"] $ \ [typ, a] -> typ ~> a ~> a)
  , ("id",     forAll ["typ", "a"] $ \ [typ, a] -> typ ~> a ~> a)
  ]

list :: [Exp] -> Exp
list [] = getDef "[]" $$ typeVarArg
list items@(_x:_) =
  foldr cons nil items
  where
    typ = typeVarArg -- inferredTypeAsHole x
    cons h t = getDef ":" $$ typ $$: [("head", h), ("tail", t)]
    nil = getDef "[]" $$ typ

factorialExpr :: Exp
factorialExpr =
  getDef "fix" $$ facType $$
  lambda "loop" facType
  ( \loop ->
    lambda "x" iInt $ \x ->
    getDef "if" $$ iInt $$:
    [ ( "condition", getDef "==" $$ iInt $$
        infixArgs x (literalInteger 0) )
    , ( "then", literalInteger 1 )
    , ( "else", getDef "*" $$ iInt $$
        infixArgs x (loop $$ (getDef "-" $$ iInt $$ infixArgs x (literalInteger 1)))
      )
    ]
  )
  where
    facType = asHole (integerType ~> integerType)
    iInt = asHole integerType

euler1Expr :: Exp
euler1Expr =
  getDef "sum" $$ iInt $$
  ( getDef "filter" $$ iInt $$:
    [ ("from", getDef ".." $$ infixArgs (literalInteger 1) (literalInteger 1000))
    , ( "predicate",
        lambda "x" iInt $ \x ->
        getDef "||" $$ infixArgs
        ( getDef "==" $$ iInt $$ infixArgs
          (literalInteger 0) (getDef "%" $$ iInt $$ infixArgs x (literalInteger 3)) )
        ( getDef "==" $$ iInt $$ infixArgs
          (literalInteger 0) (getDef "%" $$ iInt $$ infixArgs x (literalInteger 5)) )
      )
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
  [ ("condition", getDef "==" $$ iInt $$ infixArgs d (literalInteger 0))
  , ( "then",
      getDef "concat" $$ iInt $$
      ( getDef "map" $$ iInt $$ iListInt $$:
        [ ("list", solvePoly $$ list [e, c, literalInteger 1])
        , ("mapping", sqrts)
        ]
      )
    )
  , ( "else",
      getDef "concat" $$ iInt $$
      ( getDef "map" $$ iInt $$ iListInt $$:
        [ ( "list", sqrts $$ (getDef "head" $$ iInt $$ (solvePoly $$ list
            [ getDef "negate" $$ iInt $$ (d %* d)
            , (c %* c) %- (literalInteger 4 %* e)
            , literalInteger 2 %* c
            , literalInteger 1
            ]))
          )
        , ( "mapping",
            lambda "x" iInt $ \x ->
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
    iInt = asHole integerType
    iListInt = asHole $ listOf integerType
    x %+ y = getDef "+" $$ iInt $$ infixArgs x y
    x %- y = getDef "-" $$ iInt $$ infixArgs x y
    x %* y = getDef "*" $$ iInt $$ infixArgs x y
    x %/ y = getDef "/" $$ iInt $$ infixArgs x y

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
