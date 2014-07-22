{-# LANGUAGE DeriveFunctor, FlexibleInstances, GeneralizedNewtypeDeriving, MultiParamTypeClasses, BangPatterns #-}
module Lamdu.Infer.Internal.Monad
  ( Results(..), emptyResults
  , deleteResultsVars

  , InferState(..)
  , Infer, run
  , tell, tellSubst, tellConstraint, tellConstraints
  , listen, listenNoTell
  , newInferredVar, newInferredVarName
  , listenSubst
  ) where

import Control.Applicative (Applicative(..))
import Control.Lens.Operators
import Control.Lens.Tuple
import Control.Monad.Except (MonadError(..))
import Control.Monad.State (MonadState(..))
import Data.Monoid (Monoid(..))
import Data.String (IsString(..))
import Lamdu.Infer.Internal.Constraints (Constraints(..))
import Lamdu.Infer.Internal.TypeVars (TypeVars, HasVar(..))
import qualified Control.Monad as Monad
import qualified Control.Monad.State as State
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Lamdu.Expr as E
import qualified Lamdu.Infer.Internal.Constraints as Constraints
import qualified Lamdu.Infer.Internal.TypeVars as TypeVars

data InferState = InferState { inferSupply :: Int }

data Results = Results
  { subst :: TypeVars.Subst
  , constraints :: Constraints
  }

emptyResults :: Results
emptyResults = Results mempty mempty
{-# INLINE emptyResults #-}

appendResults :: Results -> Results -> Either String Results
appendResults (Results s0 c0) (Results s1 c1) =
  do
    c0' <- Constraints.applySubst s1 c0
    return $ Results (mappend s0 s1) (mappend c0' c1)
{-# INLINE appendResults #-}

deleteResultsVars :: TypeVars -> Results -> Results
deleteResultsVars vs (Results s c) =
  Results
  (TypeVars.substDeleteVars vs s)
  (Constraints.constraintDeleteVars vs c)

newtype Infer a = Infer { runInfer :: Results -> InferState -> Either String (a, Results, InferState) }
  deriving Functor

instance Monad Infer where
  return x = Infer $ \w s -> Right (x, w, s)
  {-# INLINE return #-}
  Infer x >>= f =
    Infer $ \w s ->
    do
      (y, w0, s0) <- x w s
      runInfer (f y) w0 s0
  {-# INLINE (>>=) #-}

instance Applicative Infer where
  pure = return
  {-# INLINE pure #-}
  (<*>) = Monad.ap
  {-# INLINE (<*>) #-}

instance MonadState InferState Infer where
  get = Infer $ \w s -> Right (s, w, s)
  put s = Infer $ \w _ -> Right ((), w, s)

instance MonadError [Char] Infer where
  throwError err = Infer $ \_ _ -> Left err
  catchError (Infer x) f =
    Infer $ \w s ->
    case x w s of
    Left e -> runInfer (f e) w s
    Right r -> Right r

run :: Infer a -> Either String (a, Results)
run (Infer t) =
  do
    (r, w, _) <- t emptyResults InferState{inferSupply = 0}
    Right (r, w)

tell :: Results -> Infer ()
tell w =
  Infer $ \w0 s ->
  do
    !w1 <- appendResults w0 w
    Right ((), w1, s)
{-# INLINE tell #-}

tellSubst :: TypeVars.HasVar t => E.TypeVar t -> t -> Infer ()
tellSubst v t =
  tell $ emptyResults
  { subst = TypeVars.newSubst v t }

tellConstraints :: Constraints -> Infer ()
tellConstraints x = tell $ emptyResults { constraints = x }

tellConstraint :: E.TypeVar E.ProductType -> E.Tag -> Infer ()
tellConstraint v tag = tellConstraints $ Constraints $ Map.singleton v (Set.singleton tag)

listen :: Infer a -> Infer (a, Results)
listen (Infer act) =
  Infer $ \w0 s0 ->
  do
    (y, w, s1) <- act emptyResults s0
    !w1 <- appendResults w0 w
    Right ((y, w), w1, s1)
{-# INLINE listen #-}

-- Duplicate of listen because building one on top of the other has a
-- large (~15%) performance penalty.
listenNoTell :: Infer a -> Infer (a, Results)
listenNoTell (Infer act) =
  Infer $ \w0 s0 ->
  do
    (y, w, s1) <- act emptyResults s0
    Right ((y, w), w0, s1)
{-# INLINE listenNoTell #-}

newInferredVarName :: IsString v => String -> Infer v
newInferredVarName prefix =
  do  s <- State.get
      State.put s{inferSupply = inferSupply s + 1}
      return $ fromString $ prefix ++ show (inferSupply s)

newInferredVar :: HasVar t => String -> Infer t
newInferredVar = fmap liftVar . newInferredVarName

listenSubst :: Infer a -> Infer (a, TypeVars.Subst)
listenSubst x = listen x <&> _2 %~ subst
