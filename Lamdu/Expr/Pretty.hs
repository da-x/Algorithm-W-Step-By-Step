module Lamdu.Expr.Pretty
  ( pPrintValUnannotated
  ) where

import Control.Applicative ((<$))
import Text.PrettyPrint.HughesPJClass (Pretty(..))
import qualified Lamdu.Expr as E
import qualified Text.PrettyPrint as PP

data EmptyDoc = EmptyDoc
instance Pretty EmptyDoc where
  pPrint _ = PP.empty

pPrintValUnannotated :: E.Val a -> PP.Doc
pPrintValUnannotated = pPrint . (EmptyDoc <$)
