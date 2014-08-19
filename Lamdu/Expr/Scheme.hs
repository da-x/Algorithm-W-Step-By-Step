{-# LANGUAGE DeriveGeneric #-}
module Lamdu.Expr.Scheme
  ( Scheme(..), mono
  ) where

import Control.DeepSeq (NFData(..))
import Control.DeepSeq.Generics (genericRnf)
import Data.Binary (Binary)
import Data.Monoid (Monoid(..))
import GHC.Generics (Generic)
import Lamdu.Expr.Constraints (Constraints(..))
import Lamdu.Expr.Type (Type)
import Lamdu.Expr.TypeVars (TypeVars(..))
import Text.PrettyPrint ((<+>), (<>))
import Text.PrettyPrint.HughesPJClass (Pretty(..), prettyParen)
import qualified Data.Set as Set
import qualified Text.PrettyPrint as PP

data Scheme = Scheme
  { schemeForAll :: TypeVars
  , schemeConstraints :: Constraints
  , schemeType :: Type
  } deriving (Generic, Show)

mono :: Type -> Scheme
mono x =
  Scheme
  { schemeForAll = mempty
  , schemeConstraints = mempty
  , schemeType = x
  }

instance NFData Scheme where
  rnf = genericRnf

instance Pretty Scheme where
  pPrintPrec lvl prec (Scheme vars@(TypeVars tv rv) constraints t)  =
    prettyParen (0 < prec) $
    forallStr <+> constraintsStr <+> pPrintPrec lvl 0 t
    where
      forallStr
        | mempty == vars = mempty
        | otherwise =
          PP.text "forall" <+>
          PP.hsep (map pPrint (Set.toList tv) ++ map pPrint (Set.toList rv)) <>
          PP.text "."
      constraintsStr
        | mempty == constraints = mempty
        | otherwise = pPrint constraints <+> PP.text "=>"

instance Binary Scheme
