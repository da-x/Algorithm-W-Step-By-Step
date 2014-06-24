{-# LANGUAGE DeriveGeneric, DeriveFunctor, DeriveFoldable, DeriveTraversable #-}
module Lamdu.Expr
  ( Lit(..)
  , ValLeaf(..)
  , ValBody(..)
  , Val(..), expPayload
  , Type(..)
  , TypeVar(..)
  , eLet, eAbs, eVar, eLit, eRecEmpty, eApp, eRecExtend, eGetField
  ) where

import Control.Applicative ((<$>))
import Control.DeepSeq (NFData(..))
import Control.DeepSeq.Generics (genericRnf)
import Control.Lens (Lens')
import Data.Foldable (Foldable)
import Data.Traversable (Traversable)
import GHC.Generics (Generic)

newtype TypeVar = TypeVar { tvName :: String }
  deriving (Eq, Ord, Generic, Show)
instance NFData TypeVar where rnf = genericRnf

data Lit     =  LInt Integer
             |  LChar Char
  deriving (Eq, Ord, Generic, Show)
instance NFData Lit where rnf = genericRnf

data ValLeaf
  =  VVar String
  |  VLit Lit
  |  VRecEmpty
  deriving (Eq, Ord, Generic, Show)
instance NFData ValLeaf where rnf = genericRnf

data ValBody exp
  =  VApp exp exp
  |  VAbs String exp
  |  VLet String exp exp
  |  VGetField exp String
  |  VRecExtend String exp exp
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

data Type    =  TVar TypeVar
             |  TFun Type Type
             |  TCon String
             |  TApp Type Type
             |  TRecExtend String Type Type
             |  TRecEmpty
  deriving (Generic, Show)
instance NFData Type where rnf = genericRnf

eLet :: String -> Val () -> Val () -> Val ()
eLet name e1 e2 = Val () $ VLet name e1 e2

eAbs :: String -> Val () -> Val ()
eAbs name body = Val () $ VAbs name body

eVar :: String -> Val ()
eVar = Val () . VLeaf . VVar

eLit :: Lit -> Val ()
eLit = Val () . VLeaf . VLit

eRecEmpty :: Val ()
eRecEmpty = Val () $ VLeaf VRecEmpty

eApp :: Val () -> Val () -> Val ()
eApp f x = Val () $ VApp f x

eRecExtend :: String -> Val () -> Val () -> Val ()
eRecExtend name typ rest = Val () $ VRecExtend name typ rest

eGetField :: Val () -> String -> Val ()
eGetField r n = Val () $ VGetField r n
