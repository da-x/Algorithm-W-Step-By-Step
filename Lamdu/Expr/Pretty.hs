module Lamdu.Expr.Pretty
  ( pPrintValUnannotated
  ) where

import Control.Applicative ((<$))
import Lamdu.Expr.Val (Val)
import Text.PrettyPrint.HughesPJClass (Pretty(..))
import qualified Text.PrettyPrint as PP

data EmptyDoc = EmptyDoc
instance Pretty EmptyDoc where
  pPrint _ = PP.empty

pPrintValUnannotated :: Val a -> PP.Doc
pPrintValUnannotated = pPrint . (EmptyDoc <$)
