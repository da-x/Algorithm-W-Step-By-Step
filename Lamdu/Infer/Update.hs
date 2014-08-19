module Lamdu.Infer.Update
  ( update, updatePayload, updateInferredVal
  ) where

import Control.Lens.Tuple
import Data.Traversable (traverse)
import Lamdu.Expr.Type (Type)
import Lamdu.Expr.Val (Val)
import Lamdu.Infer (Infer)
import qualified Lamdu.Infer as Infer
import qualified Lamdu.Infer.Internal.Monad as M
import qualified Lamdu.Infer.Internal.Subst as Subst

-- | When inferring expressions in a non-empty scope, or unifying
-- different types, existing type expressions may refer to "old" type
-- variables that have known substitutions. To update old types to
-- contain knowledge from all accumulated substitutions, use this
-- action.
update :: Type -> Infer Type
update typ =
  do  s <- M.getSubst
      return $ Subst.apply s typ

updatePayload :: Infer.Payload -> Infer Infer.Payload
updatePayload (Infer.Payload typ scope) =
  do  s <- M.getSubst
      return $ Infer.Payload (Subst.apply s typ) (Subst.apply s scope)

updateInferredVal :: Val (Infer.Payload, a) -> Infer (Val (Infer.Payload, a))
updateInferredVal = (traverse . _1) updatePayload
