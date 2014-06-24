module Lamdu.Infer.Internal.Monad
  ( InferState(..)
  , Infer, run
  , InferW, runW
  , InfersVars, newInferredVar
  ) where

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

class InfersVars t where
  liftName :: String -> t

instance InfersVars E.Type where
  liftName = E.TVar . E.TypeVar

instance InfersVars E.RecordType where
  liftName = E.TRecVar . E.RecordTypeVar

newInferredVar :: InfersVars t => String -> Infer t
newInferredVar prefix =
  do
    s <- State.get
    State.put s{inferSupply = inferSupply s + 1}
    return $ liftName $ prefix ++ show (inferSupply s)
