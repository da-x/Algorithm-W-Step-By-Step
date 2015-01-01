{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Lamdu.Infer.Update
  ( Update(..), liftInfer
  , Subst.CanSubst, update, inferredVal
  ) where

import Control.Applicative (Applicative)
import Control.Lens.Operators
import Control.Lens.Tuple
import Data.Traversable (traverse)
import Lamdu.Expr.Val (Val)
import Lamdu.Infer (Infer(Infer))
import qualified Control.Monad.Trans.State as State
import qualified Lamdu.Infer as Infer
import qualified Lamdu.Infer.Internal.Monad as M
import qualified Lamdu.Infer.Internal.Subst as Subst

newtype Update a = Update { run :: Infer.Context -> a }
  deriving (Functor, Applicative, Monad)

liftInfer :: Update a -> Infer a
liftInfer = Infer . State.gets . run

-- | When inferring expressions in a non-empty scope, or unifying
-- different types, existing type expressions may refer to "old" type
-- variables that have known substitutions. To update old types to
-- contain knowledge from all accumulated substitutions, use this
-- action.
update :: Subst.CanSubst a => a -> Update a
update t = Update $ \ctx -> Subst.apply (M.subst (M.ctxResults ctx)) t

inferredVal :: Val (Infer.Payload, a) -> Update (Val (Infer.Payload, a))
inferredVal = traverse . _1 %%~ update
