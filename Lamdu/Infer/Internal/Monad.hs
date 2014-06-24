module Lamdu.Infer.Internal.Monad
  ( InferState(..)
  , Infer, run
  , InferW, runW
  , newTyVarName
  , newTyVar
  ) where

import Control.Applicative ((<$>))
import Control.Monad.State (evalState, State)
import Control.Monad.Trans.Either (EitherT, runEitherT)
import Control.Monad.Writer (WriterT, runWriterT)
import Lamdu.Expr
import Lamdu.Infer.Internal.FreeTypeVars
import qualified Control.Monad.State as State

data InferState = InferState { inferSupply :: Int }

-- TODO: Consider newtypes?
type Infer = EitherT String (State InferState)
type InferW = WriterT Subst Infer

run :: Infer a -> Either String a
run t = evalState (runEitherT t) initInferState
  where initInferState = InferState{inferSupply = 0}

runW :: InferW a -> Infer (a, Subst)
runW = runWriterT

newTyVarName :: String -> Infer TypeVar
newTyVarName prefix =
    do  s <- State.get
        State.put s{inferSupply = inferSupply s + 1}
        return $ TypeVar $ prefix ++ show (inferSupply s)

newTyVar :: String -> Infer Type
newTyVar prefix = TVar <$> newTyVarName prefix

