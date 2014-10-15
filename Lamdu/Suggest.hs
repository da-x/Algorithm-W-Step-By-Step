module Lamdu.Suggest
  ( suggestValue
  ) where

import Control.Monad (replicateM)
import Control.Monad.Trans.State (State, state)
import Data.String (IsString(..))
import Control.Applicative (Applicative(..), (<$>))
import Lamdu.Expr.Type (Type)
import Lamdu.Expr.Val (Val(..))
import System.Random (RandomGen, random)
import qualified Lamdu.Expr.Pure as P
import qualified Lamdu.Expr.Scheme as S
import qualified Lamdu.Expr.Type as T
import qualified Lamdu.Expr.Val as V

suggestValueWith :: Applicative f => f V.Var -> Type -> f (Val ())
suggestValueWith _ T.TVar{}                  = pure P.hole
suggestValueWith _ T.TInst{}                 = pure P.hole
suggestValueWith mkVar (T.TRecord composite) = suggestRecordWith mkVar composite
suggestValueWith mkVar (T.TFun _ r)          =
  P.abs <$> mkVar <*> pure S.any <*> suggestValueWith mkVar r

suggestRecordWith :: Applicative f => f V.Var -> T.Composite T.Product -> f (Val ())
suggestRecordWith _ T.CVar{}          = pure P.hole
suggestRecordWith _ T.CEmpty          = pure P.recEmpty
suggestRecordWith mkVar (T.CExtend f t r) =
  P.recExtend f <$> suggestValueWith mkVar t <*> suggestRecordWith mkVar r

suggestValue :: RandomGen g => Type -> State g (Val ())
suggestValue = suggestValueWith ((fmap fromString . replicateM 16 . state) random)
