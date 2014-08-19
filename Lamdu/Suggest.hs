module Lamdu.Suggest
  ( suggestValue
  ) where

import Control.Monad (replicateM)
import Control.Monad.Trans.State (State, state)
import Data.String (IsString(..))
import Lamdu.Expr.Type (Type)
import System.Random (RandomGen, random)
import qualified Lamdu.Expr as E
import qualified Lamdu.Expr.Type as T

suggestValue :: RandomGen g => Type -> State g (E.Val ())
suggestValue T.TVar{}              = return $ E.Val () $ E.VLeaf E.VHole
suggestValue T.TInst{}             = return $ E.Val () $ E.VLeaf E.VHole
suggestValue (T.TRecord composite) = suggestRecord composite
suggestValue (T.TFun _ r)          = do
                                       param <-
                                         fmap fromString $
                                         replicateM 16 $ state random
                                       res <- suggestValue r
                                       return $ E.Val () $ E.VAbs $ E.Lam param res

suggestRecord :: RandomGen g => T.Composite T.Product -> State g (E.Val ())
suggestRecord T.CVar{}          = return $ E.Val () $ E.VLeaf E.VHole
suggestRecord T.CEmpty          = return $ E.Val () $ E.VLeaf E.VRecEmpty
suggestRecord (T.CExtend f t r) = do
                                    fv <- suggestValue t
                                    rv <- suggestRecord r
                                    return $ E.Val () $ E.VRecExtend $ E.RecExtend f fv rv
