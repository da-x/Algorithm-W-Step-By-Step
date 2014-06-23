{-# LANGUAGE DeriveGeneric, DeriveFunctor, DeriveFoldable, DeriveTraversable #-}
module Expr
  ( Lit(..)
  , Leaf(..)
  , Body(..)
  , Exp(..), expPayload
  , Scheme(..)
  , Type(..)
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

data Exp a = Exp
  { _expPayload :: a
  , expBody :: !(Body (Exp a))
  } deriving (Functor, Foldable, Traversable, Generic, Show)
instance NFData a => NFData (Exp a) where rnf = genericRnf

expPayload :: Lens' (Exp a) a
expPayload f (Exp pl body) = (`Exp` body) <$> f pl

data Type    =  TVar String
             |  TFun Type Type
             |  TCon String
             |  TApp Type Type
             |  TRecExtend String Type Type
             |  TRecEmpty
  deriving (Generic, Show)
instance NFData Type where rnf = genericRnf

data Scheme  =  Scheme [String] Type
