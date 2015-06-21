{-# LANGUAGE DeriveGeneric, GeneralizedNewtypeDeriving #-}
module Lamdu.Expr.Constraints
    ( Constraints(..)
    , ForbiddenFields
    , applyProductRenames
    , intersect
    , CompositeVarConstraints(..)
    , getProductVarConstraints
    , TypeVarConstraints
    , getTypeVarConstraints
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

type ForbiddenFields = Set T.Tag

type TypeVarConstraints = ()

newtype CompositeVarConstraints t = CompositeVarConstraints
    { compositeVarConstraints :: Map (T.Var (T.Composite t)) ForbiddenFields
    } deriving (Generic, Eq, Show)

instance Monoid (CompositeVarConstraints t) where
    mempty = CompositeVarConstraints Map.empty
    mappend (CompositeVarConstraints x) (CompositeVarConstraints y) =
        CompositeVarConstraints $ Map.unionWith mappend x y

instance NFData (CompositeVarConstraints t) where
    rnf = genericRnf

instance Pretty (CompositeVarConstraints t) where
    pPrint (CompositeVarConstraints m)
        | Map.null m = PP.text "NoConstraints"
        | otherwise =
            PP.hcat $ PP.punctuate PP.comma $ map (uncurry pPrintConstraint) $
            Map.toList m

instance Binary (CompositeVarConstraints t)

renameApply ::
    Map (T.Var (T.Composite t)) (T.Var (T.Composite t)) ->
    CompositeVarConstraints t -> CompositeVarConstraints t
renameApply renames (CompositeVarConstraints m) =
    CompositeVarConstraints (Map.mapKeys rename m)
    where
        rename x = fromMaybe x $ Map.lookup x renames

newtype Constraints = Constraints
    { productVarConstraints :: CompositeVarConstraints T.Product
    } deriving (Monoid, NFData, Binary, Pretty, Generic, Eq, Show)

getProductVarConstraints :: T.ProductVar -> Constraints -> ForbiddenFields
getProductVarConstraints tv c =
    fromMaybe Set.empty $ Map.lookup tv $ compositeVarConstraints $ productVarConstraints c

getTypeVarConstraints :: T.Var T.Type -> Constraints -> TypeVarConstraints
getTypeVarConstraints _ _ = ()

pPrintConstraint :: T.Var t -> Set T.Tag -> PP.Doc
pPrintConstraint tv forbiddenFields =
    PP.text "{" <>
    (PP.hsep . map pPrint . Set.toList) forbiddenFields <>
    PP.text "}" <+>
    PP.text "∉" <+> pPrint tv

applyProductRenames :: Map T.ProductVar T.ProductVar -> Constraints -> Constraints
applyProductRenames renames (Constraints prodConstraints) =
    Constraints (renameApply renames prodConstraints)

compositeIntersect ::
    TypeVars.CompositeVarKind t =>
    TypeVars -> CompositeVarConstraints t -> CompositeVarConstraints t
compositeIntersect tvs (CompositeVarConstraints c) =
    CompositeVarConstraints (Map.filterWithKey inTVs c)
    where
        inTVs rtv _ = rtv `TypeVars.member` tvs

intersect :: TypeVars -> Constraints -> Constraints
intersect tvs (Constraints c) =
    Constraints (compositeIntersect tvs c)
