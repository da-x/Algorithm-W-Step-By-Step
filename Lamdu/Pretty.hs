{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS -fno-warn-orphans #-}
module Lamdu.Pretty () where

import Control.Lens.Operators
import Control.Lens.Tuple
import Data.List (intersperse)
import Data.Monoid (Monoid(..))
import Lamdu.Infer.Scheme
import Lamdu.Infer.TypeVars (TypeVars(..))
import Text.PrettyPrint ((<+>), (<>), ($$))
import Text.PrettyPrint.HughesPJClass (Pretty(..), prettyParen)
import qualified Data.ByteString.Char8 as BS
import qualified Data.Set as Set
import qualified Lamdu.Expr as E
import qualified Text.PrettyPrint as PP

instance Pretty E.ValVar        where pPrint = PP.text . BS.unpack . E.vvName
instance Pretty E.TypeVar       where pPrint = PP.text . BS.unpack . E.tvName
instance Pretty E.Tag           where pPrint = PP.text . BS.unpack . E.tagName
instance Pretty E.RecordTypeVar where pPrint = PP.text . BS.unpack . E.rtvName

instance Pretty Scheme where
  pPrintPrec lvl prec (Scheme (TypeVars tv rv) t)  =
    prettyParen (0 < prec) $
    PP.text "All" <+>
    PP.hcat (PP.punctuate PP.comma (map pPrint (Set.toList tv) ++ map pPrint (Set.toList rv))) <>
    PP.text "." <+> pPrintPrec lvl 0 t

instance Pretty (E.Val ()) where
  pPrintPrec lvl prec expr =
    case E.valBody expr of
    E.VLeaf (E.VVar var)          -> pPrint var
    E.VLeaf (E.VLiteralInteger i) -> pPrint i
    E.VLeaf E.VHole               -> PP.text "?"
    E.VLet x b body               -> prettyParen (1 < prec) $
                                     PP.text "let" <+>
                                     pPrint x <+> PP.text "=" <+>
                                     pPrint b <+> PP.text "in" $$
                                     PP.nest 2 (pPrint body)
    E.VApp (E.Apply e1 e2)        -> prettyParen (10 < prec) $
                                     pPrintPrec lvl 10 e1 <+> pPrintPrec lvl 11 e2
    E.VAbs (E.Lam n e)            -> prettyParen (0 < prec) $
                                     PP.char '\\' <> pPrint n <+>
                                     PP.text "->" <+>
                                     pPrint e
    E.VGetField (E.GetField e n)  -> prettyParen (12 < prec) $
                                     pPrintPrec lvl 12 e <> PP.char '.' <> pPrint n
    E.VLeaf E.VRecEmpty           -> PP.text "{}"
    E.VRecExtend {}               ->
        PP.text "V{" <+>
            mconcat (intersperse (PP.text ", ")
              (map prField fields)) <>
            moreFields <+>
        PP.text "}"
      where
        prField (field, val) = pPrint field <+> PP.text "=" <+> pPrint val
        moreFields =
          case mRest of
          Nothing -> PP.empty
          Just rest -> PP.comma <+> PP.text "{" <+> pPrint rest <+> PP.text "}"
        (fields, mRest) = flatRecordValue expr

flatRecordValue :: E.Val a -> ([(E.Tag, E.Val a)], Maybe (E.Val a))
flatRecordValue (E.Val _ (E.VRecExtend field val body)) =
  flatRecordValue body
  & _1 %~ ((field, val):)
flatRecordValue (E.Val _ (E.VLeaf E.VRecEmpty)) = ([], Nothing)
flatRecordValue other = ([], Just other)

instance Pretty E.Type where
  pPrintPrec lvl prec typ =
    case typ of
    E.TVar n -> pPrint n
    E.TCon s -> PP.text s
    E.TFun t s ->
      prettyParen (8 < prec) $
      pPrintPrec lvl 9 t <+> PP.text "->" <+> pPrintPrec lvl 8 s
    E.TApp t s ->
      prettyParen (10 < prec) $
      pPrintPrec lvl 10 t <+> pPrintPrec lvl 11 s
    E.TRecord r -> pPrint r

instance Pretty E.RecordType where
  pPrint x =
    PP.text "T{" <+> go PP.empty x <+> PP.text "}"
    where
      go _   E.TRecEmpty          = PP.empty
      go sep (E.TRecVar tv)       = sep <> pPrint tv <> PP.text "..."
      go sep (E.TRecExtend f t r) = sep <> pPrint f <+> PP.text ":" <+> pPrint t <> go (PP.text ", ") r
