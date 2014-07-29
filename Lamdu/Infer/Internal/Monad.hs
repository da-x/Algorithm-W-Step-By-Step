{-# LANGUAGE DeriveFunctor, FlexibleInstances, GeneralizedNewtypeDeriving, MultiParamTypeClasses, BangPatterns #-}
module Lamdu.Infer.Internal.Monad
  ( Results(..), emptyResults
  , deleteResultsVars
  , Context(..), initialContext
  , InferState(..)
  , Infer(..)
  , tell, tellSubst, tellConstraint, tellConstraints
  , listen, listenNoTell
  , getConstraints
  , newInferredVar, newInferredVarName
  , listenSubst
  ) where

import Control.Applicative (Applicative(..))
import Control.Lens.Operators
import Control.Lens.Tuple
import Control.Monad.Except (MonadError(..))
import Control.Monad.State (StateT(..))
import Data.Monoid (Monoid(..))
import Data.String (IsString(..))
import Lamdu.Infer.Internal.Constraints (Constraints(..))
import Lamdu.Infer.Internal.TypeVars (TypeVars, HasVar(..))
import qualified Control.Monad.State as State
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Lamdu.Expr as E
import qualified Lamdu.Infer.Internal.Constraints as Constraints
import qualified Lamdu.Infer.Internal.TypeVars as TypeVars

data InferState = InferState { inferSupply :: Int }

data Results = Results
  { subst :: {-# UNPACK #-} !TypeVars.Subst
  , constraints :: !Constraints
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

data Context = Context
  { ctxResults :: {-# UNPACK #-} !Results
  , ctxState :: {-# UNPACK #-} !InferState
  }

initialContext :: Context
initialContext =
  Context
  { ctxResults = emptyResults
  , ctxState = InferState { inferSupply = 0 }
  }

-- We use StateT, but it is composed of an actual stateful fresh
-- supply and a component used as a writer avoiding the
-- associativity/performance issues of WriterT
newtype Infer a = Infer { run :: StateT Context (Either String) a }
  deriving (Functor, Applicative, Monad, MonadError String)

tell :: Results -> Infer ()
tell w =
  Infer $ StateT $ \c ->
  do
    !newRes <- appendResults (ctxResults c) w
    Right ((), c { ctxResults = newRes} )
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
listen (Infer (StateT act)) =
  Infer $ StateT $ \c0 ->
  do
    (y, c1) <- act c0 { ctxResults = emptyResults }
    !w <- appendResults (ctxResults c0) (ctxResults c1)
    Right ((y, w), c1 { ctxResults = w} )
{-# INLINE listen #-}

-- Duplicate of listen because building one on top of the other has a
-- large (~15%) performance penalty.
listenNoTell :: Infer a -> Infer (a, Results)
listenNoTell (Infer (StateT act)) =
  Infer $ StateT $ \c0 ->
  do
    (y, c1) <- act c0 { ctxResults = emptyResults }
    Right ((y, ctxResults c1), c1 { ctxResults = ctxResults c0} )
{-# INLINE listenNoTell #-}

newInferredVarName :: IsString v => String -> Infer v
newInferredVarName prefix =
  Infer $
  do  oldCtx <- State.get
      let oldSupply = inferSupply (ctxState oldCtx)
      State.put oldCtx{ctxState = (ctxState oldCtx){inferSupply = oldSupply+1}}
      return $ fromString $ prefix ++ show oldSupply

newInferredVar :: HasVar t => String -> Infer t
newInferredVar = fmap liftVar . newInferredVarName

listenSubst :: Infer a -> Infer (a, TypeVars.Subst)
listenSubst x = listen x <&> _2 %~ subst

getConstraints :: Infer Constraints
getConstraints = Infer $ State.gets (constraints . ctxResults)
