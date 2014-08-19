{-# LANGUAGE DeriveGeneric, EmptyDataDecls, GeneralizedNewtypeDeriving, OverloadedStrings #-}
module Lamdu.Expr.Type
  ( Type(..), CompositeType(..), Product, ProductType
  , TypeVar(..), TypeId(..), Tag(..), TypeParamId(..)
  , (~>), intType
  , compositeTypeTypes, typeNextLayer
  ) where

import Control.Applicative ((<$>), Applicative(..))
import Control.DeepSeq (NFData(..))
import Control.DeepSeq.Generics (genericRnf)
import Data.Binary (Binary)
import Data.Map (Map)
import Data.String (IsString(..))
import GHC.Generics (Generic)
import Lamdu.Expr.Identifier (Identifier)
import Text.PrettyPrint ((<+>), (<>))
import Text.PrettyPrint.HughesPJClass (Pretty(..), prettyParen)
import qualified Control.Lens as Lens
import qualified Data.List as List
import qualified Data.Map as Map
import qualified Text.PrettyPrint as PP

newtype TypeVar t = TypeVar { tvName :: Identifier }
  deriving (Eq, Ord, Show, NFData, IsString, Pretty, Binary)

newtype TypeId = TypeId { typeId :: Identifier }
  deriving (Eq, Ord, Show, NFData, IsString, Pretty, Binary)

newtype Tag = Tag { tagName :: Identifier }
  deriving (Eq, Ord, Show, NFData, IsString, Pretty, Binary)

newtype TypeParamId = TypeParamId { typeParamId :: Identifier }
  deriving (Eq, Ord, Show, NFData, IsString, Pretty, Binary)

data Product

data CompositeType p = CExtend Tag Type (CompositeType p)
                     | CEmpty
                     | CVar (TypeVar (CompositeType p))
  deriving (Generic, Show, Eq, Ord)
instance NFData (CompositeType p) where rnf = genericRnf
instance Binary (CompositeType p)

compositeTypeTypes :: Lens.Traversal' (CompositeType p) Type
compositeTypeTypes f (CExtend tag typ rest) = CExtend tag <$> f typ <*> compositeTypeTypes f rest
compositeTypeTypes _ CEmpty = pure CEmpty
compositeTypeTypes _ (CVar tv) = pure (CVar tv)

type ProductType = CompositeType Product

data Type    =  TVar (TypeVar Type)
             |  TFun Type Type
             |  TInst TypeId (Map TypeParamId Type)
             |  TRecord ProductType
  deriving (Generic, Show, Eq, Ord)
instance NFData Type where rnf = genericRnf
instance Binary Type

-- | Traverse direct types within a type
typeNextLayer :: Lens.Traversal' Type Type
typeNextLayer _ (TVar tv) = pure (TVar tv)
typeNextLayer f (TFun a r) = TFun <$> f a <*> f r
typeNextLayer f (TInst tid m) = TInst tid <$> Lens.traverse f m
typeNextLayer f (TRecord p) = TRecord <$> compositeTypeTypes f p

-- The type of LiteralInteger
intType :: Type
intType = TInst "Int" Map.empty

infixr 1 ~>
(~>) :: Type -> Type -> Type
(~>) = TFun

instance Pretty Type where
  pPrintPrec lvl prec typ =
    case typ of
    TVar n -> pPrint n
    TFun t s ->
      prettyParen (8 < prec) $
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
    TRecord r -> pPrint r

instance Pretty (CompositeType p) where
  pPrint CEmpty = PP.text "T{}"
  pPrint x =
    PP.text "{" <+> go PP.empty x <+> PP.text "}"
    where
      go _   CEmpty          = PP.empty
      go sep (CVar tv)       = sep <> pPrint tv <> PP.text "..."
      go sep (CExtend f t r) = sep <> pPrint f <+> PP.text ":" <+> pPrint t <> go (PP.text ", ") r
