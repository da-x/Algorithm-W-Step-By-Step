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
import qualified Control.Monad.State as State
import qualified Lamdu.Expr as E
import qualified Lamdu.Infer.Internal.FreeTypeVars as FreeTypeVars

data InferState = InferState { inferSupply :: Int }

-- TODO: Consider newtypes?
type Infer = EitherT String (State InferState)
type InferW = WriterT FreeTypeVars.Subst Infer

run :: Infer a -> Either String a
run t = evalState (runEitherT t) initInferState
  where initInferState = InferState{inferSupply = 0}

runW :: InferW a -> Infer (a, FreeTypeVars.Subst)
runW = runWriterT

newTyVarName :: String -> Infer E.TypeVar
newTyVarName prefix =
    do  s <- State.get
        State.put s{inferSupply = inferSupply s + 1}
        return $ E.TypeVar $ prefix ++ show (inferSupply s)

newTyVar :: String -> Infer E.Type
newTyVar prefix = E.TVar <$> newTyVarName prefix

