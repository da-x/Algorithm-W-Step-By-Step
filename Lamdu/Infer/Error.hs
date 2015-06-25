{-# LANGUAGE OverloadedStrings #-}

module Lamdu.Infer.Error
    ( Error(..)
    ) where

import qualified Lamdu.Expr.Type as T
import qualified Lamdu.Expr.Val as V
import           Text.PrettyPrint ((<+>), Doc)
import           Text.PrettyPrint.HughesPJClass (Pretty(..))

data Error
    = DuplicateField T.Tag T.Product
    | DuplicateAlt T.Tag T.Sum
    | MissingGlobal V.GlobalId
    | MissingNominal T.Id
    | OccursCheckFail Doc Doc
    | TypesDoNotUnity Doc Doc
    | UnboundVariable V.Var

instance Pretty Error where
    pPrint (DuplicateField t r) =
        "Field" <+> pPrint t <+> "forbidden in record" <+> pPrint r
    pPrint (DuplicateAlt t r) =
        "Alternative" <+> pPrint t <+> "forbidden in sum" <+> pPrint r
    pPrint (MissingGlobal g) = "Missing global:" <+> pPrint g
    pPrint (MissingNominal i) = "Missing nominal:" <+> pPrint i
    pPrint (OccursCheckFail v t) = "Occurs check fails:" <+> v <+> "vs." <+> t
    pPrint (UnboundVariable v) = "Unbound variable:" <+> pPrint v
    pPrint (TypesDoNotUnity x y) = "Types do not unify" <+> x <+> "vs." <+> y
