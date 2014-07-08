{-# OPTIONS -fno-warn-orphans #-}
module Lamdu.Pretty (pPrintPureVal, pPrintValUnannotated) where

import Control.Applicative ((<$))
import Data.Monoid (Monoid(..))
import Lamdu.Infer.Internal.Scheme
import Lamdu.Infer.Internal.TypeVars (TypeVars(..))
import Text.PrettyPrint ((<+>), (<>))
import Text.PrettyPrint.HughesPJClass (Pretty(..), PrettyLevel, prettyParen)
import qualified Data.ByteString.Char8 as BS
import qualified Data.Set as Set
import qualified Lamdu.Expr as E
import qualified Text.PrettyPrint as PP

instance Pretty E.ValVar        where pPrint = PP.text . BS.unpack . E.vvName

instance Pretty Scheme where
  pPrintPrec lvl prec (Scheme vars@(TypeVars tv rv) constraints t)  =
    prettyParen (0 < prec) $
    forallStr <+> constraintsStr <+> pPrintPrec lvl 0 t
    where
      forallStr
        | mempty == vars = mempty
        | otherwise =
          PP.text "forall" <+>
          PP.hsep (map pPrint (Set.toList tv) ++ map pPrint (Set.toList rv)) <>
          PP.text "."
      constraintsStr
        | mempty == constraints = mempty
        | otherwise = pPrint constraints <+> PP.text "=>"

instance Pretty a => Pretty (E.Val a) where
  pPrintPrec lvl prec (E.Val pl body)
    | PP.isEmpty plDoc = pPrintPrecBody lvl prec body
    | otherwise =
      prettyParen (13 < prec) $ mconcat
      [ pPrintPrecBody lvl 14 body, PP.text "{", plDoc, PP.text "}" ]
    where
      plDoc = pPrintPrec lvl 0 pl

data EmptyDoc = EmptyDoc
instance Pretty EmptyDoc where
  pPrint _ = PP.empty

pPrintPureVal :: E.Val () -> PP.Doc
pPrintPureVal = pPrint . (EmptyDoc <$)

pPrintValUnannotated :: E.Val a -> PP.Doc
pPrintValUnannotated = pPrint . (EmptyDoc <$)

pPrintPrecBody ::
  Pretty pl => PrettyLevel -> Rational -> E.ValBody (E.Val pl) -> PP.Doc
pPrintPrecBody lvl prec body =
  case body of
  E.VLeaf (E.VVar var)          -> pPrint var
  E.VLeaf (E.VGlobal tag)       -> pPrint tag
  E.VLeaf (E.VLiteralInteger i) -> pPrint i
  E.VLeaf E.VHole               -> PP.text "?"
  E.VApp (E.Apply e1 e2)        -> prettyParen (10 < prec) $
                                   pPrintPrec lvl 10 e1 <+> pPrintPrec lvl 11 e2
  E.VAbs (E.Lam n e)            -> prettyParen (0 < prec) $
                                   PP.char '\\' <> pPrint n <+>
                                   PP.text "->" <+>
                                   pPrint e
  E.VGetField (E.GetField e n)  -> prettyParen (12 < prec) $
                                   pPrintPrec lvl 12 e <> PP.char '.' <> pPrint n
  E.VLeaf E.VRecEmpty           -> PP.text "V{}"
  E.VRecExtend tag val rest     -> PP.text "{" <+>
                                     prField <>
                                     PP.comma <+>
                                     pPrint rest <+>
                                   PP.text "}"
    where
      prField = pPrint tag <+> PP.text "=" <+> pPrint val
