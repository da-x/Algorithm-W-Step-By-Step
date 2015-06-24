module Lamdu.Infer.Load
    ( Loader(..)
    , loadInfer
    ) where

import           Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Traversable as Traversable
import           Lamdu.Expr.Globals (valGlobals)
import           Lamdu.Expr.Scheme (Scheme)
import           Lamdu.Expr.Val (Val)
import qualified Lamdu.Expr.Val as V
import           Lamdu.Infer (Scope, Infer, infer, Payload)

newtype Loader m = Loader
    { loadTypeOf :: V.GlobalId -> m Scheme
    }

loadValGlobals :: Monad m => Loader m -> Val a -> m (Map V.GlobalId Scheme)
loadValGlobals (Loader load) =
    Traversable.sequence . Map.fromSet load . Set.fromList . valGlobals

loadInfer :: Monad m => Loader m -> Scope -> Val a -> m (Infer (Val (Payload, a)))
loadInfer loader scope val =
    do
        globals <- loadValGlobals loader val
        return $ infer globals scope val
