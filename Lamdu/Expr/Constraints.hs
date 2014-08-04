{-# LANGUAGE DeriveGeneric #-}
module Lamdu.Expr.Constraints
  ( Constraints(..)
  , applyRenames
  ) where

import Control.DeepSeq (NFData(..))
import Control.DeepSeq.Generics (genericRnf)
import Data.Map (Map)
import Data.Maybe (fromMaybe)
import Data.Monoid (Monoid(..))
import Data.Set (Set)
import GHC.Generics (Generic)
import Text.PrettyPrint ((<+>), (<>))
import Text.PrettyPrint.HughesPJClass (Pretty(..))
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Lamdu.Expr as E
import qualified Text.PrettyPrint as PP

newtype Constraints = Constraints
  { forbiddenRecordFields :: Map (E.TypeVar E.ProductType) (Set E.Tag)
  } deriving (Generic, Eq, Show)

instance NFData Constraints where
  rnf = genericRnf

instance Monoid Constraints where
  mempty = Constraints Map.empty
  mappend (Constraints x) (Constraints y) = Constraints $ Map.unionWith mappend x y

instance Pretty Constraints where
  pPrint (Constraints m)
    | Map.null m = PP.text "NoConstraints"
    | otherwise =
      PP.hcat $ PP.punctuate PP.comma $ map (uncurry pPrintConstraint) $
      Map.toList m

pPrintConstraint :: E.TypeVar E.ProductType -> Set E.Tag -> PP.Doc
pPrintConstraint tv forbiddenFields =
  PP.text "{" <>
  (PP.hsep . map pPrint . Set.toList) forbiddenFields <>
  PP.text "}" <+>
  PP.text "∉" <+> pPrint tv

applyRenames :: Map (E.TypeVar E.ProductType) (E.TypeVar E.ProductType) -> Constraints -> Constraints
applyRenames renames (Constraints m) =
  Constraints $ Map.mapKeys rename m
  where
    rename x = fromMaybe x $ Map.lookup x renames
