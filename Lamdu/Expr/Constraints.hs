{-# LANGUAGE DeriveGeneric, GeneralizedNewtypeDeriving #-}
module Lamdu.Expr.Constraints
    ( Constraints(..)
    , ForbiddenFields
    , applyRenames
    , intersect
    , CompositeVarConstraints(..)
    , getProductVarConstraints
    , getSumVarConstraints
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

instance NFData (CompositeVarConstraints t) where rnf = genericRnf

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

data Constraints = Constraints
    { productVarConstraints :: CompositeVarConstraints T.Product
    , sumVarConstraints :: CompositeVarConstraints T.Sum
    } deriving (Generic, Eq, Show)

instance Monoid Constraints where
    mempty = Constraints mempty mempty
    mappend (Constraints p0 s0) (Constraints p1 s1) =
        Constraints (mappend p0 p1) (mappend s0 s1)

instance Binary Constraints
instance NFData Constraints where rnf = genericRnf
instance Pretty Constraints where
    pPrint (Constraints p s) =
        PP.text "{" <> pPrint p <> PP.text "}" <>
        PP.text "[" <> pPrint s <> PP.text "]"

getTVCompositeConstraints :: T.Var (T.Composite t) -> CompositeVarConstraints t -> Set T.Tag
getTVCompositeConstraints tv = fromMaybe Set.empty . Map.lookup tv . compositeVarConstraints

getProductVarConstraints :: T.ProductVar -> Constraints -> ForbiddenFields
getProductVarConstraints tv c = getTVCompositeConstraints tv $ productVarConstraints c

getSumVarConstraints :: T.SumVar -> Constraints -> ForbiddenFields
getSumVarConstraints tv c = getTVCompositeConstraints tv $ sumVarConstraints c

getTypeVarConstraints :: T.Var T.Type -> Constraints -> TypeVarConstraints
getTypeVarConstraints _ _ = ()

pPrintConstraint :: T.Var t -> Set T.Tag -> PP.Doc
pPrintConstraint tv forbiddenFields =
    PP.text "{" <>
    (PP.hsep . map pPrint . Set.toList) forbiddenFields <>
    PP.text "}" <+>
    PP.text "∉" <+> pPrint tv

applyRenames ::
    Map T.ProductVar T.ProductVar ->
    Map T.SumVar T.SumVar ->
    Constraints -> Constraints
applyRenames prodRenames sumRenames (Constraints prodConstraints sumConstraints) =
    Constraints
    (renameApply prodRenames prodConstraints)
    (renameApply sumRenames sumConstraints)

compositeIntersect ::
    TypeVars.CompositeVarKind t =>
    TypeVars -> CompositeVarConstraints t -> CompositeVarConstraints t
compositeIntersect tvs (CompositeVarConstraints c) =
    CompositeVarConstraints (Map.filterWithKey inTVs c)
    where
        inTVs rtv _ = rtv `TypeVars.member` tvs

intersect :: TypeVars -> Constraints -> Constraints
intersect tvs (Constraints p s) =
    Constraints (compositeIntersect tvs p) (compositeIntersect tvs s)
