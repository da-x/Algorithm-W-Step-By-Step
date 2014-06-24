{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS -fno-warn-orphans #-}
module Lamdu.Pretty () where

import Control.Lens.Operators
import Control.Lens.Tuple
import Data.List (intersperse)
import Data.Map (Map)
import Data.Monoid (Monoid(..))
import Lamdu.Expr
import Lamdu.Infer.Internal.FlatRecordType (FlatRecordType(..))
import Lamdu.Infer.Scheme
import Text.PrettyPrint ((<+>), (<>), ($$))
import Text.PrettyPrint.HughesPJClass (Pretty(..), prettyParen)
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Lamdu.Infer.Internal.FlatRecordType as FlatRecordType
import qualified Text.PrettyPrint as PP

instance Pretty TypeVar where
  pPrint = PP.text . tvName

instance Pretty Scheme where
  pPrintPrec lvl prec (Scheme vars t)  =
    prettyParen (0 < prec) $
    PP.text "All" <+>
    PP.hcat (PP.punctuate PP.comma (map pPrint (Set.toList vars))) <>
    PP.text "." <+> pPrintPrec lvl 0 t

instance Pretty Lit where
  pPrint (LInt i)   =   pPrint i
  pPrint (LChar c)  =   pPrint c

instance Pretty (Expr ()) where
  pPrintPrec lvl prec expr =
    case valBody expr of
    VLeaf (VVar name) ->   PP.text name
    VLeaf (VLit lit)  ->   pPrint lit
    VLet x b body     ->   prettyParen (1 < prec) $
                           PP.text "let" <+>
                           PP.text x <+> PP.text "=" <+>
                           pPrint b <+> PP.text "in" $$
                           PP.nest 2 (pPrint body)
    VApp e1 e2        ->   prettyParen (10 < prec) $
                           pPrintPrec lvl 10 e1 <+> pPrintPrec lvl 11 e2
    VAbs n e          ->   prettyParen (0 < prec) $
                           PP.char '\\' <> PP.text n <+>
                           PP.text "->" <+>
                           pPrint e
    VGetField e n     ->   prettyParen (12 < prec) $
                           pPrintPrec lvl 12 e <> PP.char '.' <> PP.text n
    VLeaf VRecEmpty   ->   PP.text "{}"
    VRecExtend {}     ->
        PP.text "V{" <+>
            mconcat (intersperse (PP.text ", ")
              (map prField (Map.toList fields))) <>
            moreFields <+>
        PP.text "}"
      where
        prField (name, val) = PP.text name <+> PP.text "=" <+> pPrint val
        moreFields =
          case mRest of
          Nothing -> PP.empty
          Just rest -> PP.comma <+> PP.text "{" <+> pPrint rest <+> PP.text "}"
        (fields, mRest) = flatRecordValue expr

flatRecordValue :: Expr a -> (Map String (Expr a), Maybe (Expr a))
flatRecordValue (Expr _ (VRecExtend name val body)) =
  flatRecordValue body
  & _1 %~ Map.insert name val
flatRecordValue (Expr _ (VLeaf VRecEmpty)) = (Map.empty, Nothing)
flatRecordValue other = (Map.empty, Just other)

instance Pretty Type where
  pPrintPrec lvl prec typ =
    case typ of
    TVar n -> pPrint n
    TCon s -> PP.text s
    TFun t s ->
      prettyParen (8 < prec) $
      pPrintPrec lvl 9 t <+> PP.text "->" <+> pPrintPrec lvl 8 s
    TApp t s ->
      prettyParen (10 < prec) $
      pPrintPrec lvl 10 t <+> pPrintPrec lvl 11 s
    TRecEmpty -> PP.text "T{}"
    TRecExtend name t rest ->
      case FlatRecordType.from typ of
      Left _ -> -- Fall back to nested record presentation:
        PP.text "T{" <+>
          PP.text name <+> PP.text ":" <+> pPrint t <+>
          PP.text "**" <+> pPrint rest <+>
        PP.text "}"
      Right flatRecord -> pPrint flatRecord

instance Pretty FlatRecordType where
  pPrint (FlatRecordType fields varName) =
      PP.text "T{" <+>
        mconcat (intersperse (PP.text ", ") (map prField (Map.toList fields))) <>
        moreFields <+>
      PP.text "}"
      where
        prField (name, typ) = PP.text name <+> PP.text ":" <+> pPrint typ
        moreFields =
          case varName of
          Nothing -> PP.empty
          Just tv -> PP.comma <+> pPrint tv <> PP.text "..."
