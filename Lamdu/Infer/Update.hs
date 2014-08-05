module Lamdu.Infer.Update
  ( update
  ) where

import Lamdu.Infer (Infer)
import qualified Lamdu.Expr as E
import qualified Lamdu.Infer.Internal.Monad as M
import qualified Lamdu.Infer.Internal.Subst as Subst

-- | When inferring expressions in a non-empty scope, or unifying
-- different types, existing type expressions may refer to "old" type
-- variables that have known substitutions. To update old types to
-- contain knowledge from all accumulated substitutions, use this
-- action.
update :: E.Type -> Infer E.Type
update typ =
  do  s <- M.getSubst
      return $ Subst.apply s typ
