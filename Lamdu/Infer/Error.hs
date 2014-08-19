{-# LANGUAGE OverloadedStrings #-}

module Lamdu.Infer.Error
  ( Error(..)
  ) where

import Text.PrettyPrint ((<+>), Doc)
import Text.PrettyPrint.HughesPJClass (Pretty(..))
import qualified Lamdu.Expr.Type as T
import qualified Lamdu.Expr.Val as V

data Error
  = FieldAlreadyInRecord T.Tag (T.Composite T.Product)
  | FieldForbidden T.Tag T.ProductVar (T.Composite T.Product)
  | IncompatibleCompositeTypes Doc Doc
  | MissingGlobal V.GlobalId
  | OccursCheckFail Doc Doc
  | TypesDoNotUnity Doc Doc
  | UnboundVariable V.Var

instance Pretty Error where
  pPrint (FieldAlreadyInRecord t r) =
    "Added field" <+> pPrint t <+> "but already in record" <+> pPrint r
  pPrint (FieldForbidden t v r) =
    "Field" <+> pPrint t <+> "forbidden in var" <+> pPrint v <+> "from record" <+> pPrint r
  pPrint (IncompatibleCompositeTypes x y) =
    "Incompatible composite types" <+> x <+> "vs." <+> y
  pPrint (MissingGlobal g) = "Missing global:" <+> pPrint g
  pPrint (OccursCheckFail v t) = "Occurs check fails:" <+> v <+> "vs." <+> t
  pPrint (UnboundVariable v) = "Unbound variable:" <+> pPrint v
  pPrint (TypesDoNotUnity x y) = "Types do not unify" <+> x <+> "vs." <+> y
