module Lamdu.Suggest
  ( suggestValue
  ) where

import Control.Monad (replicateM)
import Control.Monad.Trans.State (State, state)
import Data.String (IsString(..))
import Lamdu.Expr.Type (Type)
import Lamdu.Expr.Val (Val(..))
import System.Random (RandomGen, random)
import qualified Lamdu.Expr.Pure as P
import qualified Lamdu.Expr.Type as T

suggestValue :: RandomGen g => Type -> State g (Val ())
suggestValue T.TVar{}              = return P.hole
suggestValue T.TInst{}             = return P.hole
suggestValue (T.TRecord composite) = suggestRecord composite
suggestValue (T.TFun _ r)          = do
                                       param <-
                                         fmap fromString $
                                         replicateM 16 $ state random
                                       res <- suggestValue r
                                       return $ P.abs param res

suggestRecord :: RandomGen g => T.Composite T.Product -> State g (Val ())
suggestRecord T.CVar{}          = return P.hole
suggestRecord T.CEmpty          = return P.recEmpty
suggestRecord (T.CExtend f t r) = do
                                    fv <- suggestValue t
                                    rv <- suggestRecord r
                                    return $ P.recExtend f fv rv
