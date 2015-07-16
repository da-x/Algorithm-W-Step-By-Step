{-# LANGUAGE OverloadedStrings #-}

module Lamdu.Infer.Error
    ( Error(..)
    ) where

import           Lamdu.Expr.Constraints (Constraints)
import           Lamdu.Expr.Scheme (Scheme)
import qualified Lamdu.Expr.Type as T
import qualified Lamdu.Expr.Val as V
import           Text.PrettyPrint ((<+>), ($+$), Doc)
import           Text.PrettyPrint.HughesPJClass (Pretty(..))

data Error
    = DuplicateField T.Tag T.Product
    | DuplicateAlt T.Tag T.Sum
    | MissingGlobal V.GlobalId
    | MissingNominal T.Id
    | OccursCheckFail Doc Doc
    | TypesDoNotUnity Doc Doc
    | UnboundVariable V.Var
    | SkolemEscapes Scheme Doc
    | UnexpectedConstraints Scheme Constraints
    | ExpectedPolymorphicType Scheme Scheme

instance Pretty Error where
    pPrint (DuplicateField t r) =
        "Field" <+> pPrint t <+> "forbidden in record" <+> pPrint r
    pPrint (DuplicateAlt t r) =
        "Alternative" <+> pPrint t <+> "forbidden in sum" <+> pPrint r
    pPrint (MissingGlobal g) =
        "Missing global:" <+> pPrint g
    pPrint (MissingNominal i) =
        "Missing nominal:" <+> pPrint i
    pPrint (OccursCheckFail v t) =
        "Occurs check fails:" <+> v <+> "vs." <+> t
    pPrint (UnboundVariable v) =
        "Unbound variable:" <+> pPrint v
    pPrint (TypesDoNotUnity x y) =
        "Types do not unify" <+> x <+> "vs." <+> y
    pPrint (SkolemEscapes poly var) =
        "When expecting a polymorphic type:" <+> pPrint poly
        $+$ "the variable:" <+> var <+> "escapes its scope"
    pPrint (UnexpectedConstraints poly constraints) =
        "Unexpected constraints " <+> pPrint constraints
        <+> "in polymorphic type:" <+> pPrint poly
    pPrint (ExpectedPolymorphicType poly mono) =
        "Expected a polymorphic type:" <+> pPrint poly
        $+$ "got:" <+> pPrint mono
