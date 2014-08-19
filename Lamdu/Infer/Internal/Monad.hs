{-# LANGUAGE DeriveFunctor, FlexibleInstances, GeneralizedNewtypeDeriving, MultiParamTypeClasses, BangPatterns #-}
module Lamdu.Infer.Internal.Monad
  ( Results(..), emptyResults, intersectResults
  , Context(..), initialContext
  , InferState(..)
  , Infer(..)
  , throwError
  , tell, tellSubst, tellSubsts, tellConstraint, tellConstraints
  , listen, listenNoTell
  , getConstraints, getSubst
  , newInferredVar, newInferredVarName
  , listenSubst
  ) where

import Control.Applicative (Applicative(..))
import Control.Lens.Operators
import Control.Lens.Tuple
import Control.Monad.Trans.State (StateT(..))
import Data.Monoid (Monoid(..))
import Data.String (IsString(..))
import Lamdu.Expr.Constraints (Constraints(..))
import Lamdu.Expr.TypeVars (HasVar(..))
import Lamdu.Expr.TypeVars (TypeVars)
import Lamdu.Infer.Error (Error)
import Lamdu.Infer.Internal.Subst (Subst)
import qualified Control.Monad.Trans.State as State
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Lamdu.Expr as E
import qualified Lamdu.Infer.Internal.Constraints as Constraints
import qualified Lamdu.Infer.Internal.Subst as Subst

data InferState = InferState { inferSupply :: Int }

data Results = Results
  { subst :: {-# UNPACK #-} !Subst
  , constraints :: !Constraints
  }

emptyResults :: Results
emptyResults = Results mempty mempty
{-# INLINE emptyResults #-}

appendResults :: Results -> Results -> Either Error Results
appendResults (Results s0 c0) (Results s1 c1) =
  do
    c0' <- Constraints.applySubst s1 c0
    return $ Results (mappend s0 s1) (mappend c0' c1)
{-# INLINE appendResults #-}

intersectResults :: TypeVars -> Results -> Results
intersectResults tvs (Results s c) =
  Results (Subst.intersect tvs s) (Constraints.intersect tvs c)

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
newtype Infer a = Infer { run :: StateT Context (Either Error) a }
  deriving (Functor, Applicative, Monad)

throwError :: Error -> Infer a
throwError = Infer . StateT . const . Left

tell :: Results -> Infer ()
tell w =
  Infer $ StateT $ \c ->
  do
    !newRes <- appendResults (ctxResults c) w
    Right ((), c { ctxResults = newRes} )
{-# INLINE tell #-}

tellSubsts :: Subst -> Infer ()
tellSubsts s = tell $ emptyResults { subst = s }

tellSubst :: Subst.HasVar t => E.TypeVar t -> t -> Infer ()
tellSubst v t = tell $ emptyResults { subst = Subst.new v t }

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

listenSubst :: Infer a -> Infer (a, Subst)
listenSubst x = listen x <&> _2 %~ subst

getConstraints :: Infer Constraints
getConstraints = Infer $ State.gets (constraints . ctxResults)

getSubst :: Infer Subst
getSubst = Infer $ State.gets (subst . ctxResults)
