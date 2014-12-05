{-# LANGUAGE DeriveGeneric, OverloadedStrings, PatternGuards #-}
module Lamdu.Expr.Scheme
  ( Scheme(..), make, mono, any
  , alphaEq
  ) where

import Prelude hiding (any)

import Control.DeepSeq (NFData(..))
import Control.DeepSeq.Generics (genericRnf)
import Control.Lens.Operators
import Control.Lens.Tuple
import Control.Monad (guard)
import Data.Binary (Binary)
import Data.Map (Map)
import Data.Monoid (Monoid(..))
import Data.Traversable (sequenceA)
import GHC.Generics (Generic)
import Lamdu.Expr.Constraints (Constraints(..), getTypeVarConstraints, getProductVarConstraints)
import Lamdu.Expr.Type (Type)
import Lamdu.Expr.TypeVars (TypeVars(..))
import Text.PrettyPrint ((<+>), (<>))
import Text.PrettyPrint.HughesPJClass (Pretty(..), prettyParen)
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Tuple as Tuple
import qualified Lamdu.Expr.Constraints as Constraints
import qualified Lamdu.Expr.Type as T
import qualified Lamdu.Expr.TypeVars as TypeVars
import qualified Text.PrettyPrint as PP

data Scheme = Scheme
  { schemeForAll :: TypeVars
  , schemeConstraints :: Constraints
  , schemeType :: Type
  } deriving (Generic, Show, Eq, Ord)

-- a Consistent List is an assoc list where each key is never
-- associated to non-eq values
fromConsistentList :: (Ord a, Eq b) => [(a, b)] -> Maybe (Map a b)
fromConsistentList pairs =
  pairs
  <&> _2 %~ Just
  & Map.fromListWith checkConsistency
  & sequenceA
  where
    checkConsistency x y = guard (x == y) >> x

fromDoublyConsistentList :: (Ord a, Ord b) => [(a, b)] -> Maybe (Map a b)
fromDoublyConsistentList pairs =
  do
    m <- fromConsistentList pairs
    _ <- fromConsistentList $ map Tuple.swap $ Map.toList m
    return m

alphaEq :: Scheme -> Scheme -> Bool
alphaEq (Scheme aForall aConstraints aType)
        (Scheme bForall bConstraints bType) =
  case T.matchVars aType bType of
    Just (tvPairs, ctvPairs)
      | Just tvMap <- fromDoublyConsistentList tvPairs
      , Just ctvMap <- fromDoublyConsistentList ctvPairs
      -> all (checkVarsMatch getProductVarConstraints) (Map.toList ctvMap) &&
         all (checkVarsMatch getTypeVarConstraints) (Map.toList tvMap)
    _ -> False
  where
    checkVarsMatch getTVConstraints (a, b) =
      ( a `TypeVars.member` aForall ==
        b `TypeVars.member` bForall
      ) &&
      ( getTVConstraints a aConstraints ==
        getTVConstraints b bConstraints
      )

make :: Constraints -> Type -> Scheme
make c t =
  Scheme freeVars (freeVars `Constraints.intersect` c) t
  where
    freeVars = TypeVars.free t

mono :: Type -> Scheme
mono x =
  Scheme
  { schemeForAll = mempty
  , schemeConstraints = mempty
  , schemeType = x
  }

any :: Scheme
any =
  Scheme (TypeVars.singleton a) mempty (T.TVar a)
  where
    a = "a"

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
