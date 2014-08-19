{-# LANGUAGE DeriveGeneric #-}
module Lamdu.Infer.Internal.Scope
  ( Scope, emptyScope
  , fromTypeMap
  , insertTypeOf
  , lookupTypeOf
  ) where

import Control.DeepSeq (NFData(..))
import Control.DeepSeq.Generics (genericRnf)
import Data.Map (Map)
import Data.Monoid (Monoid(..))
import GHC.Generics (Generic)
import Lamdu.Expr.Type (Type)
import Lamdu.Infer.Internal.Subst (CanSubst(..))
import qualified Data.Map as Map
import qualified Lamdu.Expr as E
import qualified Lamdu.Infer.Internal.Subst as Subst

newtype Scope = Scope { typeOfVar :: Map E.ValVar Type }
  deriving (Generic, Show)
instance NFData Scope where rnf = genericRnf
instance CanSubst Scope where
    freeVars (Scope env) =  mconcat $ map Subst.freeVars $ Map.elems env
    apply s (Scope env) =  Scope $ Map.map (Subst.apply s) env

emptyScope :: Scope
emptyScope = Scope Map.empty

lookupTypeOf :: E.ValVar -> Scope -> Maybe Type
lookupTypeOf key = Map.lookup key . typeOfVar

insertTypeOf :: E.ValVar -> Type -> Scope -> Scope
insertTypeOf key scheme (Scope env) = Scope (Map.insert key scheme env)

fromTypeMap :: Map E.ValVar Type -> Scope
fromTypeMap = Scope
