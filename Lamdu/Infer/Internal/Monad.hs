{-# LANGUAGE NoImplicitPrelude, DeriveFunctor, FlexibleInstances, GeneralizedNewtypeDeriving, MultiParamTypeClasses, BangPatterns, RecordWildCards #-}
module Lamdu.Infer.Internal.Monad
    ( Results(..), subst, constraints, emptyResults
    , Context(..), ctxResults, ctxState, initialContext
    , InferState(..)
    , InferCtx(..), inferCtx
    , Infer
    , throwError
    , isSkolem
    , addSkolems
    , tell, tellSubst
    , tellProductConstraint
    , tellSumConstraint
    , tellConstraints
    , listen, listenNoTell
    , getConstraints, getSubst
    , freshInferredVar, freshInferredVarName
    , listenSubst
    ) where

import           Prelude.Compat

import           Control.Lens (Lens')
import qualified Control.Lens as Lens
import           Control.Lens.Operators
import           Control.Lens.Tuple
import           Control.Monad (liftM)
import           Control.Monad.Trans.State (StateT(..))
import qualified Control.Monad.Trans.State as State
import qualified Data.Map as Map
import qualified Data.Set as Set
import           Data.String (IsString(..))
import           Lamdu.Expr.Constraints (Constraints(..), CompositeVarConstraints(..))
import qualified Lamdu.Expr.Type as T
import qualified Lamdu.Expr.TypeVars as TV
import           Lamdu.Infer.Error (Error)
import qualified Lamdu.Infer.Internal.Constraints as Constraints
import           Lamdu.Infer.Internal.Subst (Subst)
import qualified Lamdu.Infer.Internal.Subst as Subst

data InferState = InferState
    { _inferSupply :: Int
    , _inferSkolems :: TV.TypeVars
    , _inferSkolemConstraints :: Constraints
    }

inferSupply :: Lens' InferState Int
inferSupply f InferState {..} = f _inferSupply <&> \_inferSupply -> InferState {..}
{-# INLINE inferSupply #-}

inferSkolems :: Lens' InferState TV.TypeVars
inferSkolems f InferState {..} = f _inferSkolems <&> \_inferSkolems -> InferState {..}
{-# INLINE inferSkolems #-}


inferSkolemConstraints :: Lens' InferState Constraints
inferSkolemConstraints f InferState {..} = f _inferSkolemConstraints <&> \_inferSkolemConstraints -> InferState {..}
{-# INLINE inferSkolemConstraints #-}

data Results = Results
    { _subst :: {-# UNPACK #-} !Subst
    , _constraints :: !Constraints
    }

subst :: Lens' Results Subst
subst f Results {..} = f _subst <&> \_subst -> Results {..}
{-# INLINE subst #-}

constraints :: Lens' Results Constraints
constraints f Results {..} = f _constraints <&> \_constraints -> Results {..}
{-# INLINE constraints #-}

emptyResults :: Results
emptyResults = Results mempty mempty
{-# INLINE emptyResults #-}

appendResults :: Results -> Results -> Either Error Results
appendResults (Results s0 c0) (Results s1 c1) =
    do
        c0' <- Constraints.applySubst s1 c0
        return $ Results (mappend s0 s1) (mappend c0' c1)
{-# INLINE appendResults #-}

data Context = Context
    { _ctxResults :: {-# UNPACK #-} !Results
    , _ctxState :: {-# UNPACK #-} !InferState
    }

ctxResults :: Lens' Context Results
ctxResults f Context {..} = f _ctxResults <&> \_ctxResults -> Context {..}
{-# INLINE ctxResults #-}

ctxState :: Lens' Context InferState
ctxState f Context {..} = f _ctxState <&> \_ctxState -> Context {..}
{-# INLINE ctxState #-}

initialContext :: Context
initialContext =
    Context
    { _ctxResults = emptyResults
    , _ctxState =
        InferState
        { _inferSupply = 0
        , _inferSkolems = mempty
        , _inferSkolemConstraints = mempty
        }
    }

-- We use StateT, but it is composed of an actual stateful fresh
-- supply and a component used as a writer avoiding the
-- associativity/performance issues of WriterT
newtype InferCtx m a = Infer { run :: StateT Context m a }
    deriving (Functor, Applicative, Monad)

inferCtx ::
    Lens.Iso
    (InferCtx m a)
    (InferCtx n b)
    (StateT Context m a)
    (StateT Context n b)
inferCtx = Lens.iso run Infer

type Infer = InferCtx (Either Error)

throwError :: Error -> Infer a
throwError = Infer . StateT . const . Left
{-# INLINE throwError #-}

isSkolem :: (Monad m, TV.VarKind t) => T.Var t -> InferCtx m Bool
isSkolem v = Infer $ Lens.uses (ctxState . inferSkolems) (v `TV.member`)
{-# INLINE isSkolem #-}

addSkolems :: Monad m => TV.TypeVars -> Constraints -> InferCtx m ()
addSkolems skolems skolemConstraints =
    Infer $ Lens.zoom ctxState $
    do
        inferSkolems <>= skolems
        inferSkolemConstraints <>= skolemConstraints

{-# INLINE addSkolems #-}

tell :: Results -> Infer ()
tell w =
    Infer $ StateT $ \c ->
    do
        !newRes <- appendResults (_ctxResults c) w
        Right ((), c { _ctxResults = newRes} )
{-# INLINE tell #-}

tellSubst :: Subst.HasVar t => T.Var t -> t -> Infer ()
tellSubst v t = tell $ emptyResults { _subst = Subst.new v t }
{-# INLINE tellSubst #-}

tellConstraints :: Constraints -> Infer ()
tellConstraints x = tell $ emptyResults { _constraints = x }
{-# INLINE tellConstraints #-}

tellProductConstraint :: T.ProductVar -> T.Tag -> Infer ()
tellProductConstraint v tag =
    tellConstraints $ mempty
    { productVarConstraints =
      CompositeVarConstraints $ Map.singleton v $ Set.singleton tag
    }
{-# INLINE tellProductConstraint #-}

tellSumConstraint :: T.SumVar -> T.Tag -> Infer ()
tellSumConstraint v tag =
    tellConstraints $ mempty
    { sumVarConstraints =
      CompositeVarConstraints $ Map.singleton v $ Set.singleton tag
    }
{-# INLINE tellSumConstraint #-}

listen :: Infer a -> Infer (a, Results)
listen (Infer (StateT act)) =
    Infer $ StateT $ \c0 ->
    do
        (y, c1) <- act c0 { _ctxResults = emptyResults }
        !w <- appendResults (_ctxResults c0) (_ctxResults c1)
        Right ((y, _ctxResults c1), c1 { _ctxResults = w} )
{-# INLINE listen #-}

-- Duplicate of listen because building one on top of the other has a
-- large (~15%) performance penalty.
listenNoTell :: Monad m => InferCtx m a -> InferCtx m (a, Results)
listenNoTell (Infer (StateT act)) =
    Infer $ StateT $ \c0 ->
    do
        (y, c1) <- act c0 { _ctxResults = emptyResults }
        return ((y, _ctxResults c1), c1 { _ctxResults = _ctxResults c0} )
{-# INLINE listenNoTell #-}

freshInferredVarName :: Monad m => String -> InferCtx m (T.Var t)
freshInferredVarName prefix =
    Infer $
    do
        oldSupply <-
            Lens.zoom (ctxState . inferSupply) $
            do
                old <- State.get
                id += 1
                return old
        return $ fromString $ prefix ++ show oldSupply
{-# INLINE freshInferredVarName #-}

freshInferredVar :: Monad m => TV.VarKind t => String -> InferCtx m t
freshInferredVar = liftM TV.lift . freshInferredVarName
{-# INLINE freshInferredVar #-}

listenSubst :: Infer a -> Infer (a, Subst)
listenSubst x = listen x <&> _2 %~ _subst
{-# INLINE listenSubst #-}

getConstraints :: Monad m => InferCtx m Constraints
getConstraints = Infer $ State.gets (_constraints . _ctxResults)
{-# INLINE getConstraints #-}

getSubst :: Monad m => InferCtx m Subst
getSubst = Infer $ State.gets (_subst . _ctxResults)
{-# INLINE getSubst #-}
