module Lamdu.Infer.Scheme
  ( Scheme(..)
  , generalize
  , instantiate
  , specific
  ) where

import Control.Monad (forM)
import Data.Set (Set)
import Lamdu.Expr
import Lamdu.Infer.Internal.FreeTypeVars (FreeTypeVars(..))
import Lamdu.Infer.Internal.Monad (Infer)
import qualified Data.Set as Set
import qualified Lamdu.Infer.Internal.FreeTypeVars as FreeTypeVars
import qualified Lamdu.Infer.Internal.Monad as InferMonad

data Scheme = Scheme (Set TypeVar) Type

instance FreeTypeVars Scheme where
    freeTypeVars (Scheme vars t) = freeTypeVars t `Set.difference` vars
    applySubst s (Scheme vars t) =
      Scheme vars $ applySubst (Set.foldr FreeTypeVars.substDelete s vars) t

specific :: Type -> Scheme
specific = Scheme Set.empty

generalize :: Set TypeVar -> Type -> Scheme
generalize outsideTVs t  =   Scheme vars t
  where vars = freeTypeVars t `Set.difference` outsideTVs

instantiate :: Scheme -> Infer Type
instantiate (Scheme vars t) =
  do
    -- Create subst from old Scheme-bound TVs to new free TVs
    subst <-
      fmap FreeTypeVars.substFromList $
      forM (Set.toList vars) $ \ oldTv ->
        do
          newTv <- InferMonad.newTyVar "a"
          return (oldTv, newTv)
    return $ applySubst subst t

