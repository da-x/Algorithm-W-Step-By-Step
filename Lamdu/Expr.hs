{-# LANGUAGE DeriveGeneric, DeriveFunctor, DeriveFoldable, DeriveTraversable #-}
module Lamdu.Expr
  ( ValLeaf(..)
  , ValBody(..), Apply(..), GetField(..), Lam(..)
  , Val(..), expPayload
  , ValVar(..)
  , RecordType(..), RecordTypeVar(..)
  , Type(..), TypeVar(..)
  , eLet, eAbs, eVar, eGlobal, eLitInt, eRecEmpty, eApp, eRecExtend, eGetField
  , Tag(..)
  ) where

import Control.Applicative ((<$>))
import Control.DeepSeq (NFData(..))
import Control.DeepSeq.Generics (genericRnf)
import Control.Lens (Lens')
import Data.ByteString (ByteString)
import Data.Foldable (Foldable)
import Data.String (IsString(..))
import Data.Traversable (Traversable)
import GHC.Generics (Generic)

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

newtype Tag = Tag { tagName :: Identifier }
  deriving (Eq, Ord, Generic, Show)
instance NFData Tag where rnf = genericRnf
instance IsString Tag where fromString = Tag . fromString

data ValLeaf
  =  VVar ValVar
  |  VGlobal Tag
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

data ValBody exp
  =  VApp {-# UNPACK #-}!(Apply exp)
  |  VAbs {-# UNPACK #-}!(Lam exp)
  |  VLet ValVar exp exp
  |  VGetField {-# UNPACK #-}!(GetField exp)
  |  VRecExtend Tag exp exp
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
             |  TCon String
             |  TApp Type Type
             |  TRecord RecordType
  deriving (Generic, Show)
instance NFData Type where rnf = genericRnf

eLet :: ValVar -> Val () -> Val () -> Val ()
eLet name e1 e2 = Val () $ VLet name e1 e2

eAbs :: ValVar -> Val () -> Val ()
eAbs name body = Val () $ VAbs $ Lam name body

eVar :: ValVar -> Val ()
eVar = Val () . VLeaf . VVar

eGlobal :: Tag -> Val ()
eGlobal = Val () . VLeaf . VGlobal

eLitInt :: Integer -> Val ()
eLitInt = Val () . VLeaf . VLiteralInteger

eRecEmpty :: Val ()
eRecEmpty = Val () $ VLeaf VRecEmpty

eApp :: Val () -> Val () -> Val ()
eApp f x = Val () $ VApp $ Apply f x

eRecExtend :: Tag -> Val () -> Val () -> Val ()
eRecExtend name typ rest = Val () $ VRecExtend name typ rest

eGetField :: Val () -> Tag -> Val ()
eGetField r n = Val () $ VGetField $ GetField r n
