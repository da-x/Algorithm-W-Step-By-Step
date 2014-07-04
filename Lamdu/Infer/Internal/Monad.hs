{-# LANGUAGE DeriveFunctor, FlexibleInstances, GeneralizedNewtypeDeriving, MultiParamTypeClasses #-}
module Lamdu.Infer.Internal.Monad
  ( InferState(..), Results(..), emptyResults
  , Infer, run, tell, listen
  , newInferredVar
  ) where

import Control.Applicative (Applicative(..))
import Control.Monad.Except (MonadError(..))
import Control.Monad.State (MonadState(..))
import Data.Monoid (Monoid(..))
import Data.String (IsString(..))
import Lamdu.Infer.Constraints (Constraints)
import qualified Control.Monad as Monad
import qualified Control.Monad.State as State
import qualified Lamdu.Expr as E
import qualified Lamdu.Infer.Constraints as Constraints
import qualified Lamdu.Infer.Internal.FreeTypeVars as FreeTypeVars

data InferState = InferState { inferSupply :: Int }

data Results = Results
  { subst :: FreeTypeVars.Subst
  , constraints :: Constraints
  }

emptyResults :: Results
emptyResults = Results mempty mempty

appendResults :: Results -> Results -> Either String Results
appendResults (Results s0 c0) (Results s1 c1) =
  do
    c0' <- Constraints.applySubst s1 c0
    return $ Results (mappend s0 s1) (mappend c0' c1)

newtype Infer a = Infer { runInfer :: InferState -> Either String (a, Results, InferState) }
  deriving Functor

instance Monad Infer where
  return x = Infer $ \s -> Right (x, emptyResults, s)
  Infer x >>= f =
    Infer $ \s ->
    do
      (y, w0, s0) <- x s
      (z, w1, s1) <- runInfer (f y) s0
      w <- appendResults w0 w1
      Right (z, w, s1)

instance Applicative Infer where
  pure = return
  (<*>) = Monad.ap

instance MonadState InferState Infer where
  get = Infer $ \s -> Right (s, emptyResults, s)
  put s = Infer $ const $ Right ((), emptyResults, s)

instance MonadError [Char] Infer where
  throwError = Infer . const . Left
  catchError (Infer x) f =
    Infer $ \s ->
    case x s of
    Left e -> runInfer (f e) s
    Right r -> Right r

run :: Infer a -> Either String (a, Results)
run (Infer t) =
  do
    (r, w, _) <- t InferState{inferSupply = 0}
    Right (r, w)

tell :: Results -> Infer ()
tell w = Infer $ \s -> Right ((), w, s)

listen :: Infer a -> Infer (a, Results)
listen (Infer x) =
  Infer $ \s ->
  do
    (y, w, s0) <- x s
    Right ((y, w), w, s0)

newInferredVar :: E.TypePart t => String -> Infer t
newInferredVar prefix =
  do  s <- State.get
      State.put s{inferSupply = inferSupply s + 1}
      return $ E.liftVar $ fromString $ prefix ++ show (inferSupply s)
