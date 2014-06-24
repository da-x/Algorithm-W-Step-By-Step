module Lamdu.Infer.Scheme
  ( Scheme(..)
  , generalize
  , instantiate
  ) where

import Control.Monad (forM)
import Data.Set (Set)
import Lamdu.Expr
import Lamdu.Infer.Internal.FreeTypeVars
import Lamdu.Infer.Internal.Monad
import qualified Data.Set as Set

data Scheme = Scheme (Set TypeVar) Type

instance FreeTypeVars Scheme where
    freeTypeVars (Scheme vars t)      =  (freeTypeVars t) `Set.difference` vars
    apply s (Scheme vars t)  =  Scheme vars (apply (Set.foldr substDelete s vars) t)

generalize :: Set TypeVar -> Type -> Scheme
generalize outsideTVs t  =   Scheme vars t
  where vars = freeTypeVars t `Set.difference` outsideTVs

instantiate :: Scheme -> Infer Type
instantiate (Scheme vars t) =
  do
    -- Create subst from old Scheme-bound TVs to new free TVs
    subst <-
      fmap substFromList $
      forM (Set.toList vars) $ \ oldTv ->
        do
          newTv <- newTyVar "a"
          return (oldTv, newTv)
    return $ apply subst t

