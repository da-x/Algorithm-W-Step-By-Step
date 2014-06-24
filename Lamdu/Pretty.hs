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
import Text.PrettyPrint ((<+>), (<>))
import Text.PrettyPrint.HughesPJClass (Pretty(..))
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Lamdu.Infer.Internal.FlatRecordType as FlatRecordType
import qualified Text.PrettyPrint as PP

instance Pretty TypeVar where
  pPrint = PP.text . tvName

instance Pretty Scheme where
  pPrint (Scheme vars t)  =
    PP.text "All" <+>
    PP.hcat (PP.punctuate PP.comma (map pPrint (Set.toList vars))) <>
    PP.text "." <+> pPrint t

prParenExp    ::  Expr () -> PP.Doc
prParenExp t  =   case expBody t of
                    ELet _ _ _  -> PP.parens (pPrint t)
                    EApp _ _    -> PP.parens (pPrint t)
                    EAbs _ _    -> PP.parens (pPrint t)
                    _           -> pPrint t

instance Pretty Lit where
  pPrint (LInt i)   =   pPrint i
  pPrint (LChar c)  =   pPrint c

instance Pretty (Expr ()) where
  pPrint expr =
    case expBody expr of
    ELeaf (EVar name) ->   PP.text name
    ELeaf (ELit lit)  ->   pPrint lit
    ELet x b body     ->   PP.text "let" <+>
                           PP.text x <+> PP.text "=" <+>
                           pPrint b <+> PP.text "in" PP.$$
                           PP.nest 2 (pPrint body)
    EApp e1 e2        ->   pPrint e1 <+> prParenExp e2
    EAbs n e          ->   PP.char '\\' <> PP.text n <+>
                           PP.text "->" <+>
                           pPrint e
    EGetField e n     ->   prParenExp e <> PP.char '.' <> PP.text n
    ELeaf ERecEmpty   ->   PP.text "{}"
    ERecExtend {}     ->
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
flatRecordValue (Expr _ (ERecExtend name val body)) =
  flatRecordValue body
  & _1 %~ Map.insert name val
flatRecordValue (Expr _ (ELeaf ERecEmpty)) = (Map.empty, Nothing)
flatRecordValue other = (Map.empty, Just other)

instance Pretty Type where
  pPrint (TVar n)    =   pPrint n
  pPrint (TCon s)    =   PP.text s
  pPrint (TFun t s)  =   prParenType t <+> PP.text "->" <+> pPrint s
  pPrint (TApp t s)  =   prParenType t <+> pPrint s
  pPrint r@(TRecExtend name typ rest) = case FlatRecordType.from r of
    Left _ -> -- Fall back to nested record presentation:
      PP.text "T{" <+>
        PP.text name <+> PP.text ":" <+> pPrint typ <+>
        PP.text "**" <+> pPrint rest <+>
      PP.text "}"
    Right flatRecord -> pPrint flatRecord
  pPrint TRecEmpty   =   PP.text "T{}"

prParenType     ::  Type -> PP.Doc
prParenType  t  =   case t of
                      TFun _ _  -> PP.parens (pPrint t)
                      _         -> pPrint t

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
          Just name -> PP.comma <+> pPrint name <> PP.text "..."
