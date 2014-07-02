{-# LANGUAGE FlexibleContexts, FlexibleInstances, GeneralizedNewtypeDeriving, MultiParamTypeClasses #-}
module Lamdu.Infer.Internal.Monad
  ( InferState(..)
  , Infer, run
  , InferW, runW
  , MonadInfer(..)
  ) where

import Control.Applicative (Applicative)
import Control.Monad.Except (ExceptT(..), runExceptT, MonadError(..))
import Control.Monad.State (evalState, State)
import Control.Monad.Trans (lift)
import Control.Monad.Writer (WriterT, runWriterT)
import Data.Monoid (Monoid)
import Data.String (IsString(..))
import qualified Control.Monad.State as State
import qualified Lamdu.Expr as E
import qualified Lamdu.Infer.Internal.FreeTypeVars as FreeTypeVars

data InferState = InferState { inferSupply :: Int }

newtype Infer a = Infer { runInfer :: ExceptT String (State InferState) a }
  deriving (Functor, Applicative, Monad)

-- TODO: Remove MonadError instance (Catch makes no sense in inference context!)
-- Export "annotateErrors" instead.
instance MonadError [Char] Infer where
  throwError = Infer . throwError
  catchError (Infer act) f = Infer $ catchError act (runInfer . f)

type InferW = WriterT FreeTypeVars.Subst Infer

run :: Infer a -> Either String a
run (Infer t) = evalState (runExceptT t) initInferState
  where initInferState = InferState{inferSupply = 0}

runW :: InferW a -> Infer (a, FreeTypeVars.Subst)
runW = runWriterT

class (Applicative m, Monad m) => MonadInfer m where
  newInferredVar :: E.TypePart t => String -> m t

instance MonadInfer Infer where
  newInferredVar prefix =
    Infer $
    do  s <- State.get
        State.put s{inferSupply = inferSupply s + 1}
        return $ E.liftVar $ fromString $ prefix ++ show (inferSupply s)

instance (Monoid w, MonadInfer m) => MonadInfer (WriterT w m) where
  newInferredVar = lift . newInferredVar
