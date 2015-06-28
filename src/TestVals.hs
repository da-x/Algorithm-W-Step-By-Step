{-# LANGUAGE OverloadedStrings #-}

module TestVals
    ( env
    , list
    , factorialVal, euler1Val, solveDepressedQuarticVal
    , lambda, lambdaRecord, whereItem, recordType
    , eLet
    , listTypePair, boolTypePair
    ) where

import           Control.Lens.Operators
import qualified Data.Map as Map
import           Data.Monoid (Monoid(..))
import qualified Data.Set as Set
import           Lamdu.Expr.Nominal (Nominal(..))
import           Lamdu.Expr.Pure (($$), ($$:))
import qualified Lamdu.Expr.Pure as P
import           Lamdu.Expr.Scheme (Scheme(..))
import qualified Lamdu.Expr.Scheme as Scheme
import           Lamdu.Expr.Type (Type, (~>))
import qualified Lamdu.Expr.Type as T
import           Lamdu.Expr.Val (Val)
import qualified Lamdu.Expr.Val as V
import           Lamdu.Infer (TypeVars(..), Loaded(..))

-- TODO: $$ to be type-classed for TApp vs BApp
-- TODO: TCon "->" instead of TFun

eLet :: V.Var -> Val () -> (Val () -> Val ()) -> Val ()
eLet name val mkBody = P.app (P.abs name body) val
    where
        body = mkBody $ P.var name

lambda :: V.Var -> (Val () -> Val ()) -> Val ()
lambda varName mkBody = P.abs varName $ mkBody $ P.var varName

lambdaRecord :: [T.Tag] -> ([Val ()] -> Val ()) -> Val ()
lambdaRecord names mkBody =
    lambda "paramsRecord" $ \paramsRec ->
    mkBody $ map (P.getField paramsRec) names

whereItem :: V.Var -> Val () -> (Val () -> Val ()) -> Val ()
whereItem name val mkBody = lambda name mkBody $$ val

recordType :: [(T.Tag, Type)] -> Type
recordType = T.TRecord . foldr (uncurry T.CExtend) T.CEmpty

forAll :: [T.TypeVar] -> ([Type] -> Type) -> Scheme
forAll tvs mkType =
    Scheme mempty { typeVars = Set.fromList tvs } mempty $ mkType $ map T.TVar tvs

listTypePair :: (T.Id, Nominal)
listTypePair =
    ( "List"
    , Nominal
        { nParams = Map.singleton "elem" tvName
        , nScheme =
            T.CEmpty
            & T.CExtend "[]" (recordType [])
            & T.CExtend ":" (recordType [("head", tv), ("tail", listOf tv)])
            & T.TSum
            & Scheme.mono
        }
    )
    where
        tvName = "a"
        tv = T.TVar tvName

listOf :: Type -> Type
listOf = T.TInst (fst listTypePair) . Map.singleton "elem"

boolType :: Type
boolType = T.TInst (fst boolTypePair) Map.empty

boolTypePair :: (T.Id, Nominal)
boolTypePair =
    ( "Bool"
    , Nominal
        { nParams = Map.empty
        , nScheme =
            T.CEmpty
            & T.CExtend "True" (recordType [])
            & T.CExtend "False" (recordType [])
            & T.TSum
            & Scheme.mono
        }
    )

maybeOf :: Type -> Type
maybeOf t =
    T.TSum $
    T.CExtend "Nothing" (recordType []) $
    T.CExtend "Just" t $
    T.CEmpty

infixType :: Type -> Type -> Type -> Type
infixType a b c = recordType [("l", a), ("r", b)] ~> c

infixArgs :: Val () -> Val () -> Val ()
infixArgs l r = P.record [("l", l), ("r", r)]

env :: Loaded
env =
    Loaded
    { loadedGlobalTypes =
        Map.fromList
        [ ("fix",    forAll ["a"] $ \ [a] -> (a ~> a) ~> a)
        , ("if",     forAll ["a"] $ \ [a] -> recordType [("condition", boolType), ("then", a), ("else", a)] ~> a)
        , ("==",     forAll ["a"] $ \ [a] -> infixType a a boolType)
        , ("%",      forAll ["a"] $ \ [a] -> infixType a a a)
        , ("*",      forAll ["a"] $ \ [a] -> infixType a a a)
        , ("-",      forAll ["a"] $ \ [a] -> infixType a a a)
        , ("+",      forAll ["a"] $ \ [a] -> infixType a a a)
        , ("/",      forAll ["a"] $ \ [a] -> infixType a a a)
        , ("sum",    forAll ["a"] $ \ [a] -> listOf a ~> a)
        , ("filter", forAll ["a"] $ \ [a] -> recordType [("from", listOf a), ("predicate", a ~> boolType)] ~> listOf a)
        , (":",      forAll ["a"] $ \ [a] -> recordType [("head", a), ("tail", listOf a)] ~> listOf a)
        , ("[]",     forAll ["a"] $ \ [a] -> listOf a)
        , ("concat", forAll ["a"] $ \ [a] -> listOf (listOf a) ~> listOf a)
        , ("map",    forAll ["a", "b"] $ \ [a, b] -> recordType [("list", listOf a), ("mapping", a ~> b)] ~> listOf b)
        , ("..",     forAll [] $ \ [] -> infixType T.TInt T.TInt (listOf T.TInt))
        , ("||",     forAll [] $ \ [] -> infixType boolType boolType boolType)
        , ("head",   forAll ["a"] $ \ [a] -> listOf a ~> a)
        , ("negate", forAll ["a"] $ \ [a] -> a ~> a)
        , ("sqrt",   forAll ["a"] $ \ [a] -> a ~> a)
        , ("id",     forAll ["a"] $ \ [a] -> a ~> a)
        , ("zipWith",forAll ["a","b","c"] $ \ [a,b,c] ->
                                  (a ~> b ~> c) ~> listOf a ~> listOf b ~> listOf c )
        , ("Just",   forAll ["a"] $ \ [a] -> a ~> maybeOf a)
        , ("Nothing",forAll ["a"] $ \ [a] -> maybeOf a)
        , ("maybe",  forAll ["a", "b"] $ \ [a, b] -> b ~> (a ~> b) ~> maybeOf a ~> b)
        , ("plus1",  forAll [] $ \ [] -> T.TInt ~> T.TInt)
        , ("True",   forAll [] $ \ [] -> boolType)
        , ("False",  forAll [] $ \ [] -> boolType)
        ]
    , loadedNominals =
        Map.fromList
        [ listTypePair
        , boolTypePair
        ]
    }

list :: [Val ()] -> Val ()
list [] = P.global "[]"
list items@(_x:_) =
    foldr cons nil items
    where
        cons h t = P.global ":" $$: [("head", h), ("tail", t)]
        nil = P.global "[]"

factorialVal :: Val ()
factorialVal =
    P.global "fix" $$
    lambda "loop"
    ( \loop ->
        lambda "x" $ \x ->
        P.global "if" $$:
        [ ( "condition", P.global "==" $$
                infixArgs x (P.litInt 0) )
        , ( "then", P.litInt 1 )
        , ( "else", P.global "*" $$
                infixArgs x (loop $$ (P.global "-" $$ infixArgs x (P.litInt 1)))
            )
        ]
    )

euler1Val :: Val ()
euler1Val =
    P.global "sum" $$
    ( P.global "filter" $$:
        [ ("from", P.global ".." $$ infixArgs (P.litInt 1) (P.litInt 1000))
        , ( "predicate",
                lambda "x" $ \x ->
                P.global "||" $$ infixArgs
                ( P.global "==" $$ infixArgs
                    (P.litInt 0) (P.global "%" $$ infixArgs x (P.litInt 3)) )
                ( P.global "==" $$ infixArgs
                    (P.litInt 0) (P.global "%" $$ infixArgs x (P.litInt 5)) )
            )
        ]
    )

solveDepressedQuarticVal :: Val ()
solveDepressedQuarticVal =
    lambdaRecord ["e", "d", "c"] $ \[e, d, c] ->
    whereItem "solvePoly" (P.global "id")
    $ \solvePoly ->
    whereItem "sqrts"
    ( lambda "x" $ \x ->
        whereItem "r"
        ( P.global "sqrt" $$ x
        ) $ \r ->
        list [r, P.global "negate" $$ r]
    )
    $ \sqrts ->
    P.global "if" $$:
    [ ("condition", P.global "==" $$ infixArgs d (P.litInt 0))
    , ( "then",
            P.global "concat" $$
            ( P.global "map" $$:
                [ ("list", solvePoly $$ list [e, c, P.litInt 1])
                , ("mapping", sqrts)
                ]
            )
        )
    , ( "else",
            P.global "concat" $$
            ( P.global "map" $$:
                [ ( "list", sqrts $$ (P.global "head" $$ (solvePoly $$ list
                        [ P.global "negate" $$ (d %* d)
                        , (c %* c) %- (P.litInt 4 %* e)
                        , P.litInt 2 %* c
                        , P.litInt 1
                        ]))
                    )
                , ( "mapping",
                        lambda "x" $ \x ->
                        solvePoly $$ list
                        [ (c %+ (x %* x)) %- (d %/ x)
                        , P.litInt 2 %* x
                        , P.litInt 2
                        ]
                    )
                ]
            )
        )
    ]
    where
        x %+ y = P.global "+" $$ infixArgs x y
        x %- y = P.global "-" $$ infixArgs x y
        x %* y = P.global "*" $$ infixArgs x y
        x %/ y = P.global "/" $$ infixArgs x y
