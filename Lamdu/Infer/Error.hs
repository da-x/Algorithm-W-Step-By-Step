{-# LANGUAGE OverloadedStrings #-}

module Lamdu.Infer.Error
  ( Error(..)
  ) where

import Text.PrettyPrint ((<+>), Doc)
import Text.PrettyPrint.HughesPJClass (Pretty(..))
import qualified Lamdu.Expr as E
import qualified Lamdu.Expr.Type as T

data Error
  = FieldAlreadyInRecord T.Tag T.ProductType
  | FieldForbidden T.Tag (T.TypeVar T.ProductType) T.ProductType
  | IncompatibleCompositeTypes Doc Doc
  | MissingGlobal E.GlobalId
  | OccursCheckFail Doc Doc
  | TypesDoNotUnity Doc Doc
  | UnboundVariable E.ValVar

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
