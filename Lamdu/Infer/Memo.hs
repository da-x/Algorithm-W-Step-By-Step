module Lamdu.Infer.Memo
  ( Memo, make
  , infer
  , loadInfer
  ) where

import Control.Lens.Operators
import Control.Monad.Trans.State (StateT(..), evalState, state)
import Data.Functor (void)
import Data.Map (Map)
import Data.MemoUgly (memo)
import Data.Traversable (Traversable, traverse)
import Lamdu.Expr.Scheme (Scheme)
import Lamdu.Expr.Val (Val)
import Lamdu.Infer (Scope, Infer(..), Payload)
import qualified Data.Foldable as Foldable
import qualified Lamdu.Expr.Val as V
import qualified Lamdu.Infer as Infer
import qualified Lamdu.Infer.Load as InferLoad


newtype Memo = Memo
  { infer :: Map V.GlobalId Scheme -> Scope -> Val () -> Infer (Val Payload) }

make :: Memo
make = Memo $
  memo $ \globals ->
  memo $ \scope ->
  memo $ \val ->
  Infer $ StateT $
  memo $
  runStateT $ Infer.run $
  Infer.infer globals scope val <&> fmap fst

zipTraversable :: Traversable t => t a -> t b -> t (a, b)
zipTraversable a b =
  -- TODO: Assert resulting state is []
  evalState (traverse (const next) a) pairs
  where
    -- TODO: Better assertion about end of list
    next = state (\(x:xs) -> (x, xs))
    -- TODO: zip that verifies same length
    pairs = zip (Foldable.toList a) (Foldable.toList b)

loadInfer ::
  Monad m => InferLoad.Loader m -> Memo -> Scope -> Val a -> m (Infer (Val (Payload, a)))
loadInfer loader iMemo scope val =
  do  globals <- InferLoad.loadValGlobals loader val
      infer iMemo globals scope (void val)
        <&> (`zipTraversable` val)
        & return
