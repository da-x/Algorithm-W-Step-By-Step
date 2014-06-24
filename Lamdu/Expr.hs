{-# LANGUAGE DeriveGeneric, DeriveFunctor, DeriveFoldable, DeriveTraversable #-}
module Lamdu.Expr
  ( Lit(..)
  , ValLeaf(..)
  , ValBody(..)
  , Expr(..), expPayload
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

data Expr a = Expr
  { _expPayload :: a
  , valBody :: !(ValBody (Expr a))
  } deriving (Functor, Foldable, Traversable, Generic, Show)
instance NFData a => NFData (Expr a) where rnf = genericRnf

expPayload :: Lens' (Expr a) a
expPayload f (Expr pl body) = (`Expr` body) <$> f pl

data Type    =  TVar TypeVar
             |  TFun Type Type
             |  TCon String
             |  TApp Type Type
             |  TRecExtend String Type Type
             |  TRecEmpty
  deriving (Generic, Show)
instance NFData Type where rnf = genericRnf

eLet :: String -> Expr () -> Expr () -> Expr ()
eLet name e1 e2 = Expr () $ VLet name e1 e2

eAbs :: String -> Expr () -> Expr ()
eAbs name body = Expr () $ VAbs name body

eVar :: String -> Expr ()
eVar = Expr () . VLeaf . VVar

eLit :: Lit -> Expr ()
eLit = Expr () . VLeaf . VLit

eRecEmpty :: Expr ()
eRecEmpty = Expr () $ VLeaf VRecEmpty

eApp :: Expr () -> Expr () -> Expr ()
eApp f x = Expr () $ VApp f x

eRecExtend :: String -> Expr () -> Expr () -> Expr ()
eRecExtend name typ rest = Expr () $ VRecExtend name typ rest

eGetField :: Expr () -> String -> Expr ()
eGetField r n = Expr () $ VGetField r n
