{-# LANGUAGE DeriveGeneric #-}
module Lamdu.Expr.Constraints
  ( Constraints(..)
  , applyRenames
  , intersect
  ) where

import Control.DeepSeq (NFData(..))
import Control.DeepSeq.Generics (genericRnf)
import Data.Binary (Binary)
import Data.Map (Map)
import Data.Maybe (fromMaybe)
import Data.Monoid (Monoid(..))
import Data.Set (Set)
import GHC.Generics (Generic)
import Lamdu.Expr.TypeVars (TypeVars)
import Text.PrettyPrint ((<+>), (<>))
import Text.PrettyPrint.HughesPJClass (Pretty(..))
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Lamdu.Expr.Type as T
import qualified Lamdu.Expr.TypeVars as TypeVars
import qualified Text.PrettyPrint as PP

newtype Constraints = Constraints
  { forbiddenRecordFields :: Map T.ProductVar (Set T.Tag)
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

instance Binary Constraints

pPrintConstraint :: T.ProductVar -> Set T.Tag -> PP.Doc
pPrintConstraint tv forbiddenFields =
  PP.text "{" <>
  (PP.hsep . map pPrint . Set.toList) forbiddenFields <>
  PP.text "}" <+>
  PP.text "∉" <+> pPrint tv

applyRenames :: Map T.ProductVar T.ProductVar -> Constraints -> Constraints
applyRenames renames (Constraints m) =
  Constraints $ Map.mapKeys rename m
  where
    rename x = fromMaybe x $ Map.lookup x renames

intersect :: TypeVars -> Constraints -> Constraints
intersect tvs (Constraints c) =
  Constraints (Map.filterWithKey inTVs c)
  where
    inTVs rtv _ = rtv `TypeVars.member` tvs
