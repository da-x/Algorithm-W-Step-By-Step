{-# LANGUAGE OverloadedStrings #-}
import Control.DeepSeq (rnf)
import Control.Exception (evaluate)
import Control.Lens (folded)
import Control.Lens.Operators
import Control.Lens.Tuple
import Control.Monad ((<=<))
import Criterion.Main (bench, defaultMain)
import Data.Map (Map)
import Data.Monoid (Monoid(..))
import Lamdu.Infer (TypeVars(..), Scheme(..), typeInference)
import Text.PrettyPrint ((<+>))
import Text.PrettyPrint.HughesPJClass (Pretty(..))
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Lamdu.Expr as E
import qualified Text.PrettyPrint as PP

-- TODO: $$ to be type-classed for TApp vs VApp
-- TODO: TCon "->" instead of TFun

lambda :: E.ValVar -> (E.Val () -> E.Val ()) -> E.Val ()
lambda varName mkBody = E.eAbs varName (mkBody (E.eVar varName))

lambdaRecord :: [E.Tag] -> ([E.Val ()] -> E.Val ()) -> E.Val ()
lambdaRecord names mkBody =
  lambda "paramsRecord" $ \paramsRec ->
  mkBody $ map (E.eGetField paramsRec) names

whereItem :: E.ValVar -> E.Val () -> (E.Val () -> E.Val ()) -> E.Val ()
whereItem name val mkBody = lambda name mkBody $$ val

record :: [(E.Tag, E.Type)] -> E.Type
record = E.TRecord . foldr (uncurry E.TRecExtend) E.TRecEmpty

eRecord :: [(E.Tag, E.Val ())] -> E.Val ()
eRecord = foldr (uncurry E.eRecExtend) E.eRecEmpty

infixl 4 $$
($$) :: E.Val () -> E.Val () -> E.Val ()
($$) = E.eApp

infixl 4 $$:
($$:) :: E.Val () -> [(E.Tag, E.Val ())] -> E.Val ()
func $$: fields = func $$ eRecord fields

infixr 4 ~>
(~>) :: E.Type -> E.Type -> E.Type
(~>) = E.TFun

getDef :: E.GlobalId -> E.Val ()
getDef = E.eGlobal

literalInteger :: Integer -> E.Val ()
literalInteger = E.eLitInt

integerType :: E.Type
integerType = E.TInst "Int" Map.empty

boolType :: E.Type
boolType = E.TInst "Bool" Map.empty

forAll :: [E.TypeVar] -> ([E.Type] -> E.Type) -> Scheme
forAll tvs mkType =
  Scheme (TypeVars (Set.fromList tvs) Set.empty) mempty $ mkType $ map E.TVar tvs

listOf :: E.Type -> E.Type
listOf = E.TInst "List" . Map.singleton "elem"

infixType :: E.Type -> E.Type -> E.Type -> E.Type
infixType a b c = record [("l", a), ("r", b)] ~> c

infixArgs :: E.Val () -> E.Val () -> E.Val ()
infixArgs l r = eRecord [("l", l), ("r", r)]

env :: Map E.GlobalId Scheme
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

list :: [E.Val ()] -> E.Val ()
list [] = getDef "[]"
list items@(_x:_) =
  foldr cons nil items
  where
    cons h t = getDef ":" $$: [("head", h), ("tail", t)]
    nil = getDef "[]"

factorialVal :: E.Val ()
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

euler1Val :: E.Val ()
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

solveDepressedQuarticVal :: E.Val ()
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

infer :: E.Val () -> IO String
infer e =
    case typeInference env e of
    Left err ->  fail $ "error: " ++ err
    Right (eScheme, eTyped) ->
      do  _ <- evaluate $ rnf (eTyped ^.. folded . _1, eScheme)
          return $ show $ pPrint e <+> PP.text "::" <+> pPrint eScheme

benches :: [(String, IO String)]
benches =
  [ ("factorial", infer factorialVal)
  , ("euler1", infer euler1Val)
  , ("solveDepressedQuartic", infer solveDepressedQuarticVal)
  ]

main :: IO ()
main = do
  mapM_ (putStrLn <=< snd) benches
  defaultMain $ map makeBench benches
  where
    makeBench (name, f) =
      bench name f
