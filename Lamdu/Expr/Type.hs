{-# LANGUAGE DeriveGeneric, EmptyDataDecls, GeneralizedNewtypeDeriving, OverloadedStrings #-}
module Lamdu.Expr.Type
    ( Type(..), Composite(..)
    , ProductTag, SumTag
    , Var(..), Id(..), Tag(..), ParamId(..)
    , ProductVar, SumVar
    , (~>), int
    , compositeTypes, nextLayer
    ) where

import           Control.Applicative ((<$>), Applicative(..))
import           Control.DeepSeq (NFData(..))
import           Control.DeepSeq.Generics (genericRnf)
import qualified Control.Lens as Lens
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
import           Text.PrettyPrint.HughesPJClass (Pretty(..), prettyParen)

newtype Var t = Var { tvName :: Identifier }
    deriving (Eq, Ord, Show, NFData, IsString, Pretty, Binary, Hashable)

newtype Id = Id { typeId :: Identifier }
    deriving (Eq, Ord, Show, NFData, IsString, Pretty, Binary, Hashable)

newtype Tag = Tag { tagName :: Identifier }
    deriving (Eq, Ord, Show, NFData, IsString, Pretty, Binary, Hashable)

newtype ParamId = ParamId { typeParamId :: Identifier }
    deriving (Eq, Ord, Show, NFData, IsString, Pretty, Binary, Hashable)

data ProductTag
data SumTag

data Composite p
    = CExtend Tag Type (Composite p)
    | CEmpty
    | CVar (Var (Composite p))
    deriving (Generic, Show, Eq, Ord)
instance NFData (Composite p) where rnf = genericRnf
instance Binary (Composite p)

type ProductVar = Var (Composite ProductTag)
type SumVar = Var (Composite SumTag)

compositeTypes :: Lens.Traversal' (Composite p) Type
compositeTypes f (CExtend tag typ rest) = CExtend tag <$> f typ <*> compositeTypes f rest
compositeTypes _ CEmpty = pure CEmpty
compositeTypes _ (CVar tv) = pure (CVar tv)

data Type
    = TVar (Var Type)
    | TFun Type Type
    | TInst Id (Map ParamId Type)
    | TRecord (Composite ProductTag)
    | TSum (Composite SumTag)
    deriving (Generic, Show, Eq, Ord)
instance NFData Type where rnf = genericRnf
instance Binary Type

-- | Traverse direct types within a type
nextLayer :: Lens.Traversal' Type Type
nextLayer _ (TVar tv) = pure (TVar tv)
nextLayer f (TFun a r) = TFun <$> f a <*> f r
nextLayer f (TInst tid m) = TInst tid <$> Lens.traverse f m
nextLayer f (TRecord p) = TRecord <$> compositeTypes f p
nextLayer f (TSum s) = TSum <$> compositeTypes f s

-- The type of LiteralInteger
int :: Type
int = TInst "Int" Map.empty

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
