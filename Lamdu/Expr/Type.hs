{-# LANGUAGE NoImplicitPrelude, DeriveGeneric, EmptyDataDecls, GeneralizedNewtypeDeriving, OverloadedStrings #-}
module Lamdu.Expr.Type
    ( Type(..), Composite(..)
    , Product   , Sum
    , ProductTag, SumTag
    , ProductVar, SumVar, TypeVar
    , Var(..), NominalId(..), PrimId(..), Tag(..), ParamId(..)
    , (~>)
    ) where

import           Prelude.Compat

import           Control.DeepSeq (NFData(..))
import           Control.DeepSeq.Generics (genericRnf)
import           Data.Binary (Binary)
import           Data.Hashable (Hashable)
import qualified Data.List as List
import           Data.Map (Map)
import qualified Data.Map as Map
import           Data.String (IsString(..))
import           GHC.Generics (Generic)
import           Lamdu.Expr.Identifier (Identifier)
import           Text.PrettyPrint ((<+>), (<>))
import qualified Text.PrettyPrint as PP
import           Text.PrettyPrint.HughesPJClass.Compat (Pretty(..), maybeParens)

newtype Var t = Var { tvName :: Identifier }
    deriving (Eq, Ord, Show, NFData, IsString, Pretty, Binary, Hashable)

newtype NominalId = NominalId { nomId :: Identifier }
    deriving (Eq, Ord, Show, NFData, IsString, Pretty, Binary, Hashable)

newtype PrimId = PrimId { primId :: Identifier }
    deriving (Eq, Ord, Show, NFData, IsString, Pretty, Binary, Hashable)

newtype Tag = Tag { tagName :: Identifier }
    deriving (Eq, Ord, Show, NFData, IsString, Pretty, Binary, Hashable)

newtype ParamId = ParamId { typeParamId :: Identifier }
    deriving (Eq, Ord, Show, NFData, IsString, Pretty, Binary, Hashable)

data ProductTag
data SumTag

type Product = Composite ProductTag
type Sum = Composite SumTag

type ProductVar = Var Product
type SumVar = Var Sum
type TypeVar = Var Type

data Composite p
    = CExtend Tag Type (Composite p)
    | CEmpty
    | CVar (Var (Composite p))
    deriving (Generic, Show, Eq, Ord)
instance NFData (Composite p) where rnf = genericRnf
instance Binary (Composite p)

data Type
    = TVar TypeVar
    | TFun Type Type
    | TInst NominalId (Map ParamId Type)
    | TPrim PrimId
    | TRecord Product
    | TSum Sum
    deriving (Generic, Show, Eq, Ord)
instance NFData Type where rnf = genericRnf
instance Binary Type

infixr 2 ~>
(~>) :: Type -> Type -> Type
(~>) = TFun

instance Pretty Type where
    pPrintPrec lvl prec typ =
        case typ of
        TVar n -> pPrint n
        TFun t s ->
            maybeParens (8 < prec) $
            pPrintPrec lvl 9 t <+> PP.text "->" <+> pPrintPrec lvl 8 s
        TInst n ps ->
            pPrint n <>
            if Map.null ps then PP.empty
            else
                PP.text "<" <>
                PP.hsep (List.intersperse PP.comma (map showParam (Map.toList ps))) <>
                PP.text ">"
            where
                showParam (p, v) = pPrint p <+> PP.text "=" <+> pPrint v
        TPrim p -> pPrint p
        TRecord r -> PP.text "*" <> pPrint r
        TSum s -> PP.text "+" <> pPrint s

instance Pretty (Composite p) where
    pPrint CEmpty = PP.text "{}"
    pPrint x =
        PP.text "{" <+> go PP.empty x <+> PP.text "}"
        where
            go _   CEmpty          = PP.empty
            go sep (CVar tv)       = sep <> pPrint tv <> PP.text "..."
            go sep (CExtend f t r) = sep <> pPrint f <+> PP.text ":" <+> pPrint t <> go (PP.text ", ") r
