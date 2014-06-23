{-# LANGUAGE DeriveGeneric, DeriveFunctor, DeriveFoldable, DeriveTraversable #-}
module Expr
  ( Lit(..)
  , Leaf(..)
  , Body(..)
  , Expr(..), expPayload
  , Scheme(..)
  , Type(..)
  , eLet, eAbs, eVar, eLit, eRecEmpty, eApp, eRecExtend, eGetField
  ) where

import Control.Applicative ((<$>))
import Control.DeepSeq (NFData(..))
import Control.DeepSeq.Generics (genericRnf)
import Control.Lens (Lens')
import Data.Foldable (Foldable)
import Data.Traversable (Traversable)
import GHC.Generics (Generic)

data Lit     =  LInt Integer
             |  LChar Char
  deriving (Eq, Ord, Generic, Show)
instance NFData Lit where rnf = genericRnf

data Leaf  =  EVar String
           |  ELit Lit
           |  ERecEmpty
  deriving (Eq, Ord, Generic, Show)
instance NFData Leaf where rnf = genericRnf

data Body exp  =  EApp exp exp
               |  EAbs String exp
               |  ELet String exp exp
               |  EGetField exp String
               |  ERecExtend String exp exp
               |  ELeaf Leaf
  deriving (Functor, Foldable, Traversable, Generic, Show)
instance NFData exp => NFData (Body exp) where rnf = genericRnf

data Expr a = Expr
  { _expPayload :: a
  , expBody :: !(Body (Expr a))
  } deriving (Functor, Foldable, Traversable, Generic, Show)
instance NFData a => NFData (Expr a) where rnf = genericRnf

expPayload :: Lens' (Expr a) a
expPayload f (Expr pl body) = (`Expr` body) <$> f pl

data Type    =  TVar String
             |  TFun Type Type
             |  TCon String
             |  TApp Type Type
             |  TRecExtend String Type Type
             |  TRecEmpty
  deriving (Generic, Show)
instance NFData Type where rnf = genericRnf

data Scheme  =  Scheme [String] Type

eLet :: String -> Expr () -> Expr () -> Expr ()
eLet name e1 e2 = Expr () $ ELet name e1 e2

eAbs :: String -> Expr () -> Expr ()
eAbs name body = Expr () $ EAbs name body

eVar :: String -> Expr ()
eVar = Expr () . ELeaf . EVar

eLit :: Lit -> Expr ()
eLit = Expr () . ELeaf . ELit

eRecEmpty :: Expr ()
eRecEmpty = Expr () $ ELeaf ERecEmpty

eApp :: Expr () -> Expr () -> Expr ()
eApp f x = Expr () $ EApp f x

eRecExtend :: String -> Expr () -> Expr () -> Expr ()
eRecExtend name typ rest = Expr () $ ERecExtend name typ rest

eGetField :: Expr () -> String -> Expr ()
eGetField r n = Expr () $ EGetField r n
