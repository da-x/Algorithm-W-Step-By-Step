{-# LANGUAGE DeriveGeneric, DeriveFunctor, DeriveFoldable, DeriveTraversable, EmptyDataDecls, GeneralizedNewtypeDeriving #-}
module Lamdu.Expr
  ( Identifier(..)
  , ValLeaf(..)
  , ValBody(..), Apply(..), GetField(..), Lam(..), RecExtend(..)
  , Val(..), valBody, valPayload
  , ValVar(..)
  , CompositeType(..), ProductType
  , Type(..), TypeVar(..), Product
  , GlobalId(..), TypeId(..)
  , Tag(..), TypeParamId(..)
  , (~>)
  ) where

import Control.Applicative ((<$>))
import Control.DeepSeq (NFData(..))
import Control.DeepSeq.Generics (genericRnf)
import Control.Lens (Lens')
import Data.Binary (Binary)
import Data.ByteString (ByteString)
import Data.Foldable (Foldable)
import Data.Map (Map)
import Data.Monoid (Monoid(..))
import Data.String (IsString(..))
import Data.Traversable (Traversable)
import GHC.Generics (Generic)
import Text.PrettyPrint ((<+>), (<>))
import Text.PrettyPrint.HughesPJClass (Pretty(..), PrettyLevel, prettyParen)
import qualified Data.ByteString.Char8 as BS
import qualified Data.List as List
import qualified Data.Map as Map
import qualified Text.PrettyPrint as PP

newtype Identifier = Identifier ByteString
  deriving (Eq, Ord, Generic, Show, Binary)
instance NFData Identifier    where rnf = genericRnf
instance IsString Identifier  where fromString = Identifier . fromString
instance Pretty Identifier    where pPrint (Identifier x) = PP.text $ BS.unpack x

newtype ValVar = ValVar { vvName :: Identifier }
  deriving (Eq, Ord, Show, NFData, IsString, Pretty, Binary)

newtype TypeVar t = TypeVar { tvName :: Identifier }
  deriving (Eq, Ord, Show, NFData, IsString, Pretty, Binary)

newtype GlobalId = GlobalId { globalId :: Identifier }
  deriving (Eq, Ord, Show, NFData, IsString, Pretty, Binary)

newtype TypeId = TypeId { typeId :: Identifier }
  deriving (Eq, Ord, Show, NFData, IsString, Pretty, Binary)

newtype Tag = Tag { tagName :: Identifier }
  deriving (Eq, Ord, Show, NFData, IsString, Pretty, Binary)

newtype TypeParamId = TypeParamId { typeParamId :: Identifier }
  deriving (Eq, Ord, Show, NFData, IsString, Pretty, Binary)

data ValLeaf
  =  VVar ValVar
  |  VGlobal GlobalId
  |  VHole
  |  VLiteralInteger Integer
  |  VRecEmpty
  deriving (Eq, Ord, Generic, Show)
instance NFData ValLeaf where rnf = genericRnf
instance Binary ValLeaf

data Apply expr = Apply
  { _applyFunc :: expr
  , _applyArg :: expr
  } deriving (Functor, Foldable, Traversable, Generic, Show)
instance NFData exp => NFData (Apply exp) where rnf = genericRnf
instance Binary exp => Binary (Apply exp)

data GetField expr = GetField
  { _getFieldRecord :: expr
  , _getFieldTag :: Tag
  } deriving (Functor, Foldable, Traversable, Generic, Show)
instance NFData exp => NFData (GetField exp) where rnf = genericRnf
instance Binary exp => Binary (GetField exp)

data Lam expr = Lam
  { _lamParamId :: ValVar
  , _lamResult :: expr
  } deriving (Functor, Foldable, Traversable, Generic, Show)
instance NFData exp => NFData (Lam exp) where rnf = genericRnf
instance Binary exp => Binary (Lam exp)

data RecExtend expr = RecExtend
  { _recTag :: Tag
  , _recFieldVal :: expr
  , _recRest :: expr
  } deriving (Functor, Foldable, Traversable, Generic, Show)
instance NFData exp => NFData (RecExtend exp) where rnf = genericRnf
instance Binary exp => Binary (RecExtend exp)

data ValBody exp
  =  VApp {-# UNPACK #-}!(Apply exp)
  |  VAbs {-# UNPACK #-}!(Lam exp)
  |  VGetField {-# UNPACK #-}!(GetField exp)
  |  VRecExtend {-# UNPACK #-}!(RecExtend exp)
  |  VLeaf ValLeaf
  deriving (Functor, Foldable, Traversable, Generic, Show)
instance NFData exp => NFData (ValBody exp) where rnf = genericRnf
instance Binary exp => Binary (ValBody exp)

data Val a = Val
  { _valPayload :: a
  , _valBody :: !(ValBody (Val a))
  } deriving (Functor, Foldable, Traversable, Generic, Show)
instance NFData a => NFData (Val a) where rnf = genericRnf
instance Binary a => Binary (Val a)

valBody :: Lens' (Val a) (ValBody (Val a))
valBody f (Val pl body) = Val pl <$> f body

valPayload :: Lens' (Val a) a
valPayload f (Val pl body) = (`Val` body) <$> f pl

data Product

data CompositeType p = CExtend Tag Type (CompositeType p)
                     | CEmpty
                     | CVar (TypeVar (CompositeType p))
  deriving (Generic, Show)
instance NFData (CompositeType p) where rnf = genericRnf
instance Binary (CompositeType p)

type ProductType = CompositeType Product

data Type    =  TVar (TypeVar Type)
             |  TFun Type Type
             |  TInst TypeId (Map TypeParamId Type)
             |  TRecord ProductType
  deriving (Generic, Show)
instance NFData Type where rnf = genericRnf
instance Binary Type

infixr 1 ~>
(~>) :: Type -> Type -> Type
(~>) = TFun

pPrintPrecBody :: Pretty pl => PrettyLevel -> Rational -> ValBody (Val pl) -> PP.Doc
pPrintPrecBody lvl prec body =
  case body of
  VLeaf (VVar var)          -> pPrint var
  VLeaf (VGlobal tag)       -> pPrint tag
  VLeaf (VLiteralInteger i) -> pPrint i
  VLeaf VHole               -> PP.text "?"
  VApp (Apply e1 e2)        -> prettyParen (10 < prec) $
                                   pPrintPrec lvl 10 e1 <+> pPrintPrec lvl 11 e2
  VAbs (Lam n e)            -> prettyParen (0 < prec) $
                               PP.char '\\' <> pPrint n <+>
                               PP.text "->" <+>
                               pPrint e
  VGetField (GetField e n)  -> prettyParen (12 < prec) $
                               pPrintPrec lvl 12 e <> PP.char '.' <> pPrint n
  VLeaf VRecEmpty           -> PP.text "V{}"
  VRecExtend
    (RecExtend tag val rest)  -> PP.text "{" <+>
                                 prField <>
                                 PP.comma <+>
                                 pPrint rest <+>
                                 PP.text "}"
    where
      prField = pPrint tag <+> PP.text "=" <+> pPrint val

instance Pretty a => Pretty (Val a) where
  pPrintPrec lvl prec (Val pl body)
    | PP.isEmpty plDoc = pPrintPrecBody lvl prec body
    | otherwise =
      prettyParen (13 < prec) $ mconcat
      [ pPrintPrecBody lvl 14 body, PP.text "{", plDoc, PP.text "}" ]
    where
      plDoc = pPrintPrec lvl 0 pl

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
