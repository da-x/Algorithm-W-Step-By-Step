module Lamdu.Suggest
  ( suggestValue
  ) where

import Control.Monad (replicateM)
import Control.Monad.Trans.State (State, state)
import Data.String (IsString(..))
import System.Random (RandomGen, random)
import qualified Lamdu.Expr as E
import qualified Lamdu.Expr.Type as E

suggestValue :: RandomGen g => E.Type -> State g (E.Val ())
suggestValue E.TVar{}              = return $ E.Val () $ E.VLeaf E.VHole
suggestValue E.TInst{}             = return $ E.Val () $ E.VLeaf E.VHole
suggestValue (E.TRecord composite) = suggestRecord composite
suggestValue (E.TFun _ r)          = do
                                       param <-
                                         fmap fromString $
                                         replicateM 16 $ state random
                                       res <- suggestValue r
                                       return $ E.Val () $ E.VAbs $ E.Lam param res

suggestRecord :: RandomGen g => E.ProductType -> State g (E.Val ())
suggestRecord E.CVar{}          = return $ E.Val () $ E.VLeaf E.VHole
suggestRecord E.CEmpty          = return $ E.Val () $ E.VLeaf E.VRecEmpty
suggestRecord (E.CExtend f t r) = do
                                    fv <- suggestValue t
                                    rv <- suggestRecord r
                                    return $ E.Val () $ E.VRecExtend $ E.RecExtend f fv rv
