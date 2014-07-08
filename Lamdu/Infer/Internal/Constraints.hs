{-# LANGUAGE FlexibleContexts #-}
module Lamdu.Infer.Internal.Constraints
  ( Constraints(..), applySubst, applyRenames
  , constraintDeleteVars
  ) where

import Data.Map (Map)
import Data.Maybe (fromMaybe)
import Data.Monoid (Monoid(..))
import Data.Set (Set)
import Lamdu.Infer.Internal.TypeVars (TypeVars(..))
import Text.PrettyPrint ((<+>), (<>))
import Text.PrettyPrint.HughesPJClass (Pretty(..))
import qualified Data.Map as Map
import qualified Data.Map.Utils as MapU
import qualified Data.Set as Set
import qualified Lamdu.Expr as E
import qualified Lamdu.Infer.Internal.FreeTypeVars as FreeTypeVars
import qualified Text.PrettyPrint as PP

newtype Constraints = Constraints
  { forbiddenRecordFields :: Map E.RecordTypeVar (Set E.Tag)
  } deriving (Eq)

instance Monoid Constraints where
  mempty = Constraints Map.empty
  mappend (Constraints x) (Constraints y) = Constraints $ Map.unionWith mappend x y

instance Pretty Constraints where
  pPrint (Constraints m)
    | Map.null m = PP.text "NoConstraints"
    | otherwise =
      PP.hcat $ PP.punctuate PP.comma $ map (uncurry pPrintConstraint) $
      Map.toList m

constraintDeleteVars :: TypeVars -> Constraints -> Constraints
constraintDeleteVars (TypeVars _ rvs) (Constraints m) =
  Constraints $ MapU.deleteKeySet rvs m

pPrintConstraint :: E.RecordTypeVar -> Set E.Tag -> PP.Doc
pPrintConstraint tv forbiddenFields =
  PP.text "{" <>
  (PP.hsep . map pPrint . Set.toList) forbiddenFields <>
  PP.text "}" <+>
  PP.text "∉" <+> pPrint tv

applyRenames :: Map E.RecordTypeVar E.RecordTypeVar -> Constraints -> Constraints
applyRenames renames (Constraints m) =
  Constraints $ Map.mapKeys rename m
  where
    rename x = fromMaybe x $ Map.lookup x renames

applySubst ::
  FreeTypeVars.Subst -> Constraints -> Either String Constraints
applySubst (FreeTypeVars.Subst _ recordSubsts) (Constraints c) =
  fmap (Constraints . Map.fromList . concat) $ mapM onConstraint $ Map.toList c
  where
    onConstraint (var, forbidden) =
      case Map.lookup var recordSubsts of
      Nothing -> Right [(var, forbidden)]
      Just recType ->
        go recType
        where
          go E.TRecEmpty             = Right []
          go (E.TRecVar newVar)      = Right [(newVar, forbidden)]
          go (E.TRecExtend f _ rest)
            | Set.member f forbidden = Left $
                                       show $
                                       PP.text "Field forbidden:" <+>
                                       pPrint f <+> PP.text "in." <+>
                                       pPrint recType
            | otherwise              = go rest
