module Lamdu.Suggest
  ( suggestValue
  ) where

import Control.Monad (replicateM)
import Control.Monad.Trans.State (State, state)
import Data.String (IsString(..))
import Lamdu.Expr.Type (Type)
import Lamdu.Expr.Val (Val(..))
import System.Random (RandomGen, random)
import qualified Lamdu.Expr.Type as T
import qualified Lamdu.Expr.Val as V

suggestValue :: RandomGen g => Type -> State g (Val ())
suggestValue T.TVar{}              = return $ Val () $ V.VLeaf V.VHole
suggestValue T.TInst{}             = return $ Val () $ V.VLeaf V.VHole
suggestValue (T.TRecord composite) = suggestRecord composite
suggestValue (T.TFun _ r)          = do
                                       param <-
                                         fmap fromString $
                                         replicateM 16 $ state random
                                       res <- suggestValue r
                                       return $ Val () $ V.VAbs $ V.Lam param res

suggestRecord :: RandomGen g => T.Composite T.Product -> State g (Val ())
suggestRecord T.CVar{}          = return $ Val () $ V.VLeaf V.VHole
suggestRecord T.CEmpty          = return $ Val () $ V.VLeaf V.VRecEmpty
suggestRecord (T.CExtend f t r) = do
                                    fv <- suggestValue t
                                    rv <- suggestRecord r
                                    return $ Val () $ V.VRecExtend $ V.RecExtend f fv rv
