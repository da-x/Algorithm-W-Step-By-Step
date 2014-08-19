module Lamdu.Infer.Internal.FlatComposite
  ( FlatComposite(..)
  , from
  , toRecordType
  ) where

import Control.Applicative ((<$>))
import Control.Lens (Lens')
import Control.Lens.Operators
import Data.Map (Map)
import Lamdu.Expr.Type (Type)
import qualified Data.Map as Map
import qualified Lamdu.Expr.Type as T

data FlatComposite p = FlatComposite
  { _fields :: Map T.Tag Type
  , _extension :: Maybe (T.TypeVar (T.CompositeType p)) -- TyVar of more possible fields
  } deriving (Show)

fields :: Lens' (FlatComposite p) (Map T.Tag Type)
fields f (FlatComposite fs ext) = (`FlatComposite` ext) <$> f fs

-- From a record type to a sorted list of fields
from :: T.CompositeType p -> FlatComposite p
from (T.CExtend name typ rest) = from rest & fields %~ Map.insert name typ
from T.CEmpty                  = FlatComposite Map.empty Nothing
from (T.CVar name)             = FlatComposite Map.empty (Just name)

toRecordType :: FlatComposite p -> T.CompositeType p
toRecordType (FlatComposite fs ext) =
  Map.foldWithKey T.CExtend (maybe T.CEmpty T.CVar ext) fs
