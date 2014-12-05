module Lamdu.Infer.Memo
  ( Memo, make
  , infer
  , loadInfer
  ) where

import Control.Monad.Trans.State (StateT(..))
import Data.Map (Map)
import Data.MemoUgly (memo)
import Lamdu.Expr.Scheme (Scheme)
import Lamdu.Expr.Val (Val)
import Lamdu.Infer (Scope, Infer(..), Payload)
import qualified Lamdu.Expr.Val as V
import qualified Lamdu.Infer as Infer
import qualified Lamdu.Infer.Load as InferLoad

newtype Memo a = Memo
  { infer :: Map V.GlobalId Scheme -> Scope -> Val a -> Infer (Val (Payload, a)) }

make :: Ord a => Memo a
make = Memo $
  memo $ \globals ->
  memo $ \scope ->
  memo $ \val ->
  Infer $ StateT $
  memo $
  runStateT $ Infer.run $
  Infer.infer globals scope val

loadInfer ::
  (Ord a, Monad m) => InferLoad.Loader m -> Memo a -> Scope -> Val a -> m (Infer (Val (Payload, a)))
loadInfer loader iMemo scope val =
  do  globals <- InferLoad.loadValGlobals loader val
      return $ infer iMemo globals scope val
