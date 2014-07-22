{-# LANGUAGE DeriveGeneric, DeriveFunctor, DeriveFoldable, DeriveTraversable, FlexibleContexts, TypeFamilies #-}
module Lamdu.Expr
  ( ValLeaf(..)
  , ValBody(..), Apply(..), GetField(..), Lam(..), RecExtend(..)
  , Val(..), expPayload
  , ValVar(..)
  , RecordType(..), RecordTypeVar(..)
  , Type(..), TypeVar(..)
  , eAbs, eVar, eGlobal, eLitInt, eRecEmpty, eApp, eRecExtend, eGetField
  , GlobalId(..), TypeId(..)
  , Tag(..), TypeParamId(..)
  , TypePart(..)
  ) where

import Control.Applicative ((<$>))
import Control.DeepSeq (NFData(..))
import Control.DeepSeq.Generics (genericRnf)
import Control.Lens (Lens')
import Data.ByteString (ByteString)
import Data.Foldable (Foldable)
import Data.Map (Map)
import Data.String (IsString(..))
import Data.Traversable (Traversable)
import GHC.Generics (Generic)
import Text.PrettyPrint ((<+>), (<>))
import Text.PrettyPrint.HughesPJClass (Pretty(..), prettyParen)
import qualified Data.ByteString.Char8 as BS
import qualified Data.List as List
import qualified Data.Map as Map
import qualified Text.PrettyPrint as PP

type Identifier = ByteString

newtype ValVar = ValVar { vvName :: Identifier }
  deriving (Eq, Ord, Generic, Show)
instance NFData ValVar where rnf = genericRnf
instance IsString ValVar where fromString = ValVar . fromString

newtype TypeVar = TypeVar { tvName :: Identifier }
  deriving (Eq, Ord, Generic, Show)
instance NFData TypeVar where rnf = genericRnf
instance IsString TypeVar where fromString = TypeVar . fromString

newtype RecordTypeVar = RecordTypeVar { rtvName :: Identifier }
  deriving (Eq, Ord, Generic, Show)
instance NFData RecordTypeVar where rnf = genericRnf
instance IsString RecordTypeVar where fromString = RecordTypeVar . fromString

newtype GlobalId = GlobalId { globalId :: Identifier }
  deriving (Eq, Ord, Generic, Show)
instance NFData GlobalId where rnf = genericRnf
instance IsString GlobalId where fromString = GlobalId . fromString

newtype TypeId = TypeId { typeId :: Identifier }
  deriving (Eq, Ord, Generic, Show)
instance NFData TypeId where rnf = genericRnf
instance IsString TypeId where fromString = TypeId . fromString

newtype Tag = Tag { tagName :: Identifier }
  deriving (Eq, Ord, Generic, Show)
instance NFData Tag where rnf = genericRnf
instance IsString Tag where fromString = Tag . fromString

newtype TypeParamId = TypeParamId { typeParamId :: Identifier }
  deriving (Eq, Ord, Generic, Show)
instance NFData TypeParamId where rnf = genericRnf
instance IsString TypeParamId where fromString = TypeParamId . fromString

data ValLeaf
  =  VVar ValVar
  |  VGlobal GlobalId
  |  VHole
  |  VLiteralInteger Integer
  |  VRecEmpty
  deriving (Eq, Ord, Generic, Show)
instance NFData ValLeaf where rnf = genericRnf

data Apply expr = Apply
  { _applyFunc :: expr
  , _applyArg :: expr
  } deriving (Functor, Foldable, Traversable, Generic, Show)
instance NFData exp => NFData (Apply exp) where rnf = genericRnf

data GetField expr = GetField
  { _getFieldRecord :: expr
  , _getFieldTag :: Tag
  } deriving (Functor, Foldable, Traversable, Generic, Show)
instance NFData exp => NFData (GetField exp) where rnf = genericRnf

data Lam expr = Lam
  { _lamParamId :: ValVar
  , _lamResult :: expr
  } deriving (Functor, Foldable, Traversable, Generic, Show)
instance NFData exp => NFData (Lam exp) where rnf = genericRnf

data RecExtend expr = RecExtend
  { _recTag :: Tag
  , _recFieldVal :: expr
  , _recRest :: expr
  } deriving (Functor, Foldable, Traversable, Generic, Show)
instance NFData exp => NFData (RecExtend exp) where rnf = genericRnf

data ValBody exp
  =  VApp {-# UNPACK #-}!(Apply exp)
  |  VAbs {-# UNPACK #-}!(Lam exp)
  |  VGetField {-# UNPACK #-}!(GetField exp)
  |  VRecExtend {-# UNPACK #-}!(RecExtend exp)
  |  VLeaf ValLeaf
  deriving (Functor, Foldable, Traversable, Generic, Show)
instance NFData exp => NFData (ValBody exp) where rnf = genericRnf

data Val a = Val
  { _expPayload :: a
  , valBody :: !(ValBody (Val a))
  } deriving (Functor, Foldable, Traversable, Generic, Show)
instance NFData a => NFData (Val a) where rnf = genericRnf

expPayload :: Lens' (Val a) a
expPayload f (Val pl body) = (`Val` body) <$> f pl

data RecordType = TRecExtend Tag Type RecordType
                | TRecEmpty
                | TRecVar RecordTypeVar
  deriving (Generic, Show)
instance NFData RecordType where rnf = genericRnf

data Type    =  TVar TypeVar
             |  TFun Type Type
             |  TInst TypeId (Map TypeParamId Type)
             |  TRecord RecordType
  deriving (Generic, Show)
instance NFData Type where rnf = genericRnf

class IsString (VarOf t) => TypePart t where
  type family VarOf t
  liftVar :: VarOf t -> t
instance TypePart Type where
  type VarOf Type = TypeVar
  liftVar = TVar
instance TypePart RecordType where
  type VarOf RecordType = RecordTypeVar
  liftVar = TRecVar

eAbs :: ValVar -> Val () -> Val ()
eAbs name body = Val () $ VAbs $ Lam name body

eVar :: ValVar -> Val ()
eVar = Val () . VLeaf . VVar

eGlobal :: GlobalId -> Val ()
eGlobal = Val () . VLeaf . VGlobal

eLitInt :: Integer -> Val ()
eLitInt = Val () . VLeaf . VLiteralInteger

eRecEmpty :: Val ()
eRecEmpty = Val () $ VLeaf VRecEmpty

eApp :: Val () -> Val () -> Val ()
eApp f x = Val () $ VApp $ Apply f x

eRecExtend :: Tag -> Val () -> Val () -> Val ()
eRecExtend name typ rest = Val () $ VRecExtend $ RecExtend name typ rest

eGetField :: Val () -> Tag -> Val ()
eGetField r n = Val () $ VGetField $ GetField r n

instance Pretty RecordTypeVar where pPrint = PP.text . BS.unpack . rtvName
instance Pretty TypeVar       where pPrint = PP.text . BS.unpack . tvName
instance Pretty GlobalId      where pPrint = PP.text . BS.unpack . globalId
instance Pretty TypeId        where pPrint = PP.text . BS.unpack . typeId
instance Pretty Tag           where pPrint = PP.text . BS.unpack . tagName
instance Pretty TypeParamId   where pPrint = PP.text . BS.unpack . typeParamId

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

instance Pretty RecordType where
  pPrint TRecEmpty = PP.text "T{}"
  pPrint x =
    PP.text "{" <+> go PP.empty x <+> PP.text "}"
    where
      go _   TRecEmpty          = PP.empty
      go sep (TRecVar tv)       = sep <> pPrint tv <> PP.text "..."
      go sep (TRecExtend f t r) = sep <> pPrint f <+> PP.text ":" <+> pPrint t <> go (PP.text ", ") r
