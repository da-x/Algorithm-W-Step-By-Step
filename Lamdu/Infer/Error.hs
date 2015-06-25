{-# LANGUAGE OverloadedStrings #-}

module Lamdu.Infer.Error
    ( Error(..)
    ) where

import qualified Lamdu.Expr.Type as T
import qualified Lamdu.Expr.Val as V
import           Text.PrettyPrint ((<+>), Doc)
import           Text.PrettyPrint.HughesPJClass (Pretty(..))

data Error
    = FieldAlreadyInRecord T.Tag T.Product
    | DuplicateField T.Tag T.ProductVar T.Product
    | DuplicateAlt T.Tag T.SumVar T.Sum
    | MissingGlobal V.GlobalId
    | OccursCheckFail Doc Doc
    | TypesDoNotUnity Doc Doc
    | UnboundVariable V.Var

instance Pretty Error where
    pPrint (FieldAlreadyInRecord t r) =
        "Added field" <+> pPrint t <+> "but already in record" <+> pPrint r
    pPrint (DuplicateField t v r) =
        "Field" <+> pPrint t <+> "forbidden in var" <+> pPrint v <+> "from record" <+> pPrint r
    pPrint (DuplicateAlt t v r) =
        "Alternative" <+> pPrint t <+> "forbidden in var" <+> pPrint v <+> "from sum" <+> pPrint r
    pPrint (MissingGlobal g) = "Missing global:" <+> pPrint g
    pPrint (OccursCheckFail v t) = "Occurs check fails:" <+> v <+> "vs." <+> t
    pPrint (UnboundVariable v) = "Unbound variable:" <+> pPrint v
    pPrint (TypesDoNotUnity x y) = "Types do not unify" <+> x <+> "vs." <+> y
