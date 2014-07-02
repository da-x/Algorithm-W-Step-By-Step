{-# LANGUAGE DeriveFunctor, FlexibleInstances, MultiParamTypeClasses #-}
module Lamdu.Infer.Internal.Monad
  ( InferState(..)
  , Infer, run
  , newInferredVar
  ) where

import Control.Applicative (Applicative(..))
import Control.Monad.Except (MonadError(..))
import Control.Monad.State (MonadState(..))
import Control.Monad.Writer (MonadWriter(..))
import Data.Monoid (Monoid(..))
import Data.String (IsString(..))
import qualified Control.Monad as Monad
import qualified Control.Monad.State as State
import qualified Lamdu.Expr as E
import qualified Lamdu.Infer.Internal.FreeTypeVars as FreeTypeVars

data InferState = InferState { inferSupply :: Int }

newtype Infer a = Infer { runInfer :: InferState -> Either String (a, FreeTypeVars.Subst, InferState) }
  deriving Functor

instance Monad Infer where
  return x = Infer $ \s -> Right (x, mempty, s)
  Infer x >>= f =
    Infer $ \s ->
    do
      (y, w0, s0) <- x s
      (z, w1, s1) <- runInfer (f y) s0
      Right (z, mappend w0 w1, s1)

instance Applicative Infer where
  pure = return
  (<*>) = Monad.ap

instance MonadState InferState Infer where
  get = Infer $ \s -> Right (s, mempty, s)
  put s = Infer $ const $ Right ((), mempty, s)

instance MonadWriter FreeTypeVars.Subst Infer where
  tell w = Infer $ \s -> Right ((), w, s)
  listen (Infer x) =
    Infer $ \s ->
    do
      (y, w, s0) <- x s
      Right ((y, w), w, s0)
  pass (Infer x) =
    Infer $ \s ->
    do
      ((y, f), w, s0) <- x s
      Right (y, f w, s0)

instance MonadError [Char] Infer where
  throwError = Infer . const . Left
  catchError (Infer x) f =
    Infer $ \s ->
    case x s of
    Left e -> runInfer (f e) s
    Right r -> Right r

run :: Infer a -> Either String (a, FreeTypeVars.Subst)
run (Infer t) =
  do
    (r, w, _) <- t InferState{inferSupply = 0}
    Right (r, w)

newInferredVar :: E.TypePart t => String -> Infer t
newInferredVar prefix =
  do  s <- State.get
      State.put s{inferSupply = inferSupply s + 1}
      return $ E.liftVar $ fromString $ prefix ++ show (inferSupply s)
