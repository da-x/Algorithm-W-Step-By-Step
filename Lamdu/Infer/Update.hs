module Lamdu.Infer.Update
  ( update, updatePayload, updateInferredVal
  ) where

import Data.Traversable (traverse)
import Lamdu.Expr.Type (Type)
import Lamdu.Infer (Infer)
import qualified Lamdu.Expr as E
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

updatePayload :: Infer.Payload a -> Infer (Infer.Payload a)
updatePayload (Infer.Payload typ scope dat) =
  do  s <- M.getSubst
      return $ Infer.Payload (Subst.apply s typ) (Subst.apply s scope) dat

updateInferredVal :: E.Val (Infer.Payload a) -> Infer (E.Val (Infer.Payload a))
updateInferredVal = traverse updatePayload
