module Pretty
  ( prTypeVar
  , prScheme
  , prExp
  , prType
  , prFlatRecordType
  ) where

import Data.List (intersperse)
import Data.Monoid (Monoid(..))
import Expr
import FlatRecordType
import Scheme
import Text.PrettyPrint ((<+>), (<>))
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Text.PrettyPrint as PP

prTypeVar :: TypeVar -> PP.Doc
prTypeVar (TypeVar n) = PP.text n

prScheme :: Scheme -> PP.Doc
prScheme (Scheme vars t)  =
  PP.text "All" <+>
  PP.hcat (PP.punctuate PP.comma (map prTypeVar (Set.toList vars))) <>
  PP.text "." <+> prType t

prParenExp    ::  Expr a -> PP.Doc
prParenExp t  =   case expBody t of
                    ELet _ _ _  -> PP.parens (prExp t)
                    EApp _ _    -> PP.parens (prExp t)
                    EAbs _ _    -> PP.parens (prExp t)
                    _           -> prExp t

prLit            ::  Lit -> PP.Doc
prLit (LInt i)   =   PP.integer i
prLit (LChar c)  =   PP.text (show c)

prExp                  ::  Expr a -> PP.Doc
prExp expr =
    case expBody expr of
    ELeaf (EVar name) ->   PP.text name
    ELeaf (ELit lit)  ->   prLit lit
    ELet x b body     ->   PP.text "let" <+>
                           PP.text x <+> PP.text "=" <+>
                           prExp b <+> PP.text "in" PP.$$
                           PP.nest 2 (prExp body)
    EApp e1 e2        ->   prExp e1 <+> prParenExp e2
    EAbs n e          ->   PP.char '\\' <> PP.text n <+>
                           PP.text "->" <+>
                           prExp e
    EGetField e n     ->   prParenExp e <> PP.char '.' <> PP.text n
    ELeaf ERecEmpty   ->   PP.text "{}"
    ERecExtend {}     ->
        PP.text "V{" <+>
            mconcat (intersperse (PP.text ", ") (map prField (Map.toList fields))) <>
            moreFields <+>
        PP.text "}"
      where
        prField (name, val) = PP.text name <+> PP.text "=" <+> prExp val
        moreFields =
          case mRest of
          Nothing -> PP.empty
          Just rest -> PP.comma <+> PP.text "{" <+> prExp rest <+> PP.text "}"
        (fields, mRest) = flattenERec expr

prType             ::  Type -> PP.Doc
prType (TVar n)    =   prTypeVar n
prType (TCon s)    =   PP.text s
prType (TFun t s)  =   prParenType t <+> PP.text "->" <+> prType s
prType (TApp t s)  =   prParenType t <+> prType s
prType r@(TRecExtend name typ rest) = case flattenRec r of
  Left _ -> -- Fall back to nested record presentation:
    PP.text "T{" <+>
      PP.text name <+> PP.text ":" <+> prType typ <+>
      PP.text "**" <+> prType rest <+>
    PP.text "}"
  Right flatRecord -> prFlatRecordType flatRecord
prType TRecEmpty   =   PP.text "T{}"

prParenType     ::  Type -> PP.Doc
prParenType  t  =   case t of
                      TFun _ _  -> PP.parens (prType t)
                      _         -> prType t

prFlatRecordType :: FlatRecordType -> PP.Doc
prFlatRecordType (FlatRecordType fields varName) =
    PP.text "T{" <+>
      mconcat (intersperse (PP.text ", ") (map prField (Map.toList fields))) <>
      moreFields <+>
    PP.text "}"
    where
      prField (name, typ) = PP.text name <+> PP.text ":" <+> prType typ
      moreFields =
        case varName of
        Nothing -> PP.empty
        Just name -> PP.comma <+> prTypeVar name <> PP.text "..."
