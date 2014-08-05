module Lamdu.Infer.Load
  ( Loader(..)
  , loadInfer
  ) where

import Data.Map (Map)
import Lamdu.Expr.Globals (valGlobals)
import Lamdu.Expr.Scheme (Scheme)
import Lamdu.Infer (Scope, Infer, infer, Payload)
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Traversable as Traversable
import qualified Lamdu.Expr as E

newtype Loader m = Loader
  { loadTypeOf :: E.GlobalId -> m Scheme
  }

loadValGlobals :: Monad m => Loader m -> E.Val a -> m (Map E.GlobalId Scheme)
loadValGlobals (Loader load) =
    Traversable.sequence . Map.fromSet load . Set.fromList . valGlobals

loadInfer :: Monad m => Loader m -> Scope -> E.Val a -> m (Infer (E.Val (Payload a)))
loadInfer loader scope val =
  do  globals <- loadValGlobals loader val
      return $ infer globals scope val
