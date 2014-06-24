module Monad
  ( InferState(..)
  , Infer, runInfer
  , InferW
  , newTyVarName
  , newTyVar
  ) where

import Control.Applicative ((<$>))
import Control.Monad.State (evalState, State)
import Control.Monad.Trans.Either (EitherT, runEitherT)
import Control.Monad.Writer (WriterT)
import Expr
import FreeTypeVars
import qualified Control.Monad.State as State

data InferState = InferState { inferSupply :: Int }

-- TODO: Consider newtypes?
type Infer = EitherT String (State InferState)
type InferW = WriterT Subst Infer

runInfer :: Infer a -> Either String a
runInfer t = evalState (runEitherT t) initInferState
  where initInferState = InferState{inferSupply = 0}

newTyVarName :: String -> Infer TypeVar
newTyVarName prefix =
    do  s <- State.get
        State.put s{inferSupply = inferSupply s + 1}
        return $ TypeVar $ prefix ++ show (inferSupply s)

newTyVar :: String -> Infer Type
newTyVar prefix = TVar <$> newTyVarName prefix

