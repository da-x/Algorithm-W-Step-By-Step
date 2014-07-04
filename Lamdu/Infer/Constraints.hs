{-# LANGUAGE FlexibleContexts #-}

module Lamdu.Infer.Constraints
  ( Constraints(..), applySubst
  ) where

import Data.Map (Map)
import Data.Monoid (Monoid(..))
import Data.Set (Set)
import Text.PrettyPrint ((<+>))
import Text.PrettyPrint.HughesPJClass (Pretty(..))
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Lamdu.Expr as E
import qualified Lamdu.Infer.Internal.FreeTypeVars as FreeTypeVars
import qualified Text.PrettyPrint as PP

newtype Constraints = Constraints
  { forbiddenRecordFields :: Map E.RecordTypeVar (Set E.Tag)
  }

instance Monoid Constraints where
  mempty = Constraints Map.empty
  mappend (Constraints x) (Constraints y) = Constraints $ Map.unionWith mappend x y

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
