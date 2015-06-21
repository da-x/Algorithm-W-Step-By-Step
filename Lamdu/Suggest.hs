module Lamdu.Suggest
    ( suggestValueWith, suggestRecordWith
    ) where

import Control.Applicative (Applicative(..), (<$>))
import Lamdu.Expr.Type (Type)
import Lamdu.Expr.Val (Val(..))
import qualified Lamdu.Expr.Pure as P
import qualified Lamdu.Expr.Type as T
import qualified Lamdu.Expr.Val as V

suggestValueWith :: Applicative f => f V.Var -> Type -> f (Val ())
suggestValueWith _ T.TVar{}                  = pure P.hole
suggestValueWith _ T.TInst{}                 = pure P.hole
-- TODO: Special case sum of 1?
suggestValueWith _ T.TSum {}                 = pure P.hole
suggestValueWith mkVar (T.TRecord composite) = suggestRecordWith mkVar composite
suggestValueWith mkVar (T.TFun _ r)          =
    P.abs <$> mkVar <*> suggestValueWith mkVar r

suggestRecordWith :: Applicative f => f V.Var -> T.Composite T.Product -> f (Val ())
suggestRecordWith _ T.CVar{}          = pure P.hole
suggestRecordWith _ T.CEmpty          = pure P.recEmpty
suggestRecordWith mkVar (T.CExtend f t r) =
    P.recExtend f <$> suggestValueWith mkVar t <*> suggestRecordWith mkVar r
