{-# LANGUAGE DeriveGeneric, EmptyDataDecls, GeneralizedNewtypeDeriving, OverloadedStrings #-}
module Lamdu.Expr.Type
  ( Type(..), Composite(..), Product
  , Var(..), Id(..), Tag(..), ParamId(..)
  , ProductVar
  , (~>), int
  , compositeTypes, nextLayer
  , LiftVar(..)
  , matchVars
  ) where

import Control.Applicative ((<$>), Applicative(..))
import Control.DeepSeq (NFData(..))
import Control.DeepSeq.Generics (genericRnf)
import Data.Binary (Binary)
import Data.Hashable (Hashable)
import Data.Map (Map)
import Data.Monoid (Monoid(..))
import Data.String (IsString(..))
import GHC.Generics (Generic)
import Lamdu.Expr.Identifier (Identifier)
import Text.PrettyPrint ((<+>), (<>))
import Text.PrettyPrint.HughesPJClass (Pretty(..), prettyParen)
import qualified Control.Lens as Lens
import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Map.Utils as MapUtils
import qualified Text.PrettyPrint as PP

newtype Var t = Var { tvName :: Identifier }
  deriving (Eq, Ord, Show, NFData, IsString, Pretty, Binary, Hashable)

newtype Id = Id { typeId :: Identifier }
  deriving (Eq, Ord, Show, NFData, IsString, Pretty, Binary, Hashable)

newtype Tag = Tag { tagName :: Identifier }
  deriving (Eq, Ord, Show, NFData, IsString, Pretty, Binary, Hashable)

newtype ParamId = ParamId { typeParamId :: Identifier }
  deriving (Eq, Ord, Show, NFData, IsString, Pretty, Binary, Hashable)

data Product

data Composite p = CExtend Tag Type (Composite p)
                 | CEmpty
                 | CVar (Var (Composite p))
  deriving (Generic, Show, Eq, Ord)
instance NFData (Composite p) where rnf = genericRnf
instance Binary (Composite p)

type ProductVar = Var (Composite Product)

compositeTypes :: Lens.Traversal' (Composite p) Type
compositeTypes f (CExtend tag typ rest) = CExtend tag <$> f typ <*> compositeTypes f rest
compositeTypes _ CEmpty = pure CEmpty
compositeTypes _ (CVar tv) = pure (CVar tv)

data Type    =  TVar (Var Type)
             |  TFun Type Type
             |  TInst Id (Map ParamId Type)
             |  TRecord (Composite Product)
  deriving (Generic, Show, Eq, Ord)
instance NFData Type where rnf = genericRnf
instance Binary Type

-- | Traverse direct types within a type
nextLayer :: Lens.Traversal' Type Type
nextLayer _ (TVar tv) = pure (TVar tv)
nextLayer f (TFun a r) = TFun <$> f a <*> f r
nextLayer f (TInst tid m) = TInst tid <$> Lens.traverse f m
nextLayer f (TRecord p) = TRecord <$> compositeTypes f p

type VarMatches = ([(Var Type, Var Type)], [(ProductVar, ProductVar)])

matchVars :: Type -> Type -> Maybe VarMatches
matchVars (TVar tv0)         (TVar tv1)         = Just ([(tv0, tv1)], [])
matchVars (TFun a0 b0)       (TFun a1 b1)       = matchVars a0 a1 `mappend` matchVars b0 b1
matchVars (TInst i0 params0) (TInst i1 params1)
  | i0 == i1 =
    mconcat . Map.elems =<< MapUtils.match matchVars params0 params1
matchVars (TRecord c0) (TRecord c1) = matchProductVars c0 c1
matchVars _ _ = Nothing

matchProductVars :: Composite Product -> Composite Product -> Maybe VarMatches
matchProductVars (CExtend tag0 typ0 rest0) (CExtend tag1 typ1 rest1)
  | tag0 == tag1 = matchVars typ0 typ1 `mappend` matchProductVars rest0 rest1
matchProductVars CEmpty CEmpty = Just ([], [])
matchProductVars (CVar tv0) (CVar tv1) = Just ([], [(tv0, tv1)])
matchProductVars _ _ = Nothing

-- The type of LiteralInteger
int :: Type
int = TInst "Int" Map.empty

infixr 1 ~>
(~>) :: Type -> Type -> Type
(~>) = TFun

class    LiftVar t             where liftVar :: Var t -> t
instance LiftVar Type          where liftVar = TVar
instance LiftVar (Composite c) where liftVar = CVar

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

instance Pretty (Composite p) where
  pPrint CEmpty = PP.text "T{}"
  pPrint x =
    PP.text "{" <+> go PP.empty x <+> PP.text "}"
    where
      go _   CEmpty          = PP.empty
      go sep (CVar tv)       = sep <> pPrint tv <> PP.text "..."
      go sep (CExtend f t r) = sep <> pPrint f <+> PP.text ":" <+> pPrint t <> go (PP.text ", ") r
