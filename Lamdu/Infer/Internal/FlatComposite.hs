module Lamdu.Infer.Internal.FlatComposite
  ( FlatComposite(..)
  , from
  , toRecordType
  ) where

import Control.Applicative ((<$>))
import Control.Lens (Lens')
import Control.Lens.Operators
import Data.Map (Map)
import qualified Data.Map as Map
import qualified Lamdu.Expr.Type as E

data FlatComposite p = FlatComposite
  { _fields :: Map E.Tag E.Type
  , _extension :: Maybe (E.TypeVar (E.CompositeType p)) -- TyVar of more possible fields
  } deriving (Show)

fields :: Lens' (FlatComposite p) (Map E.Tag E.Type)
fields f (FlatComposite fs ext) = (`FlatComposite` ext) <$> f fs

-- From a record type to a sorted list of fields
from :: E.CompositeType p -> FlatComposite p
from (E.CExtend name typ rest) = from rest & fields %~ Map.insert name typ
from E.CEmpty                  = FlatComposite Map.empty Nothing
from (E.CVar name)             = FlatComposite Map.empty (Just name)

toRecordType :: FlatComposite p -> E.CompositeType p
toRecordType (FlatComposite fs ext) =
  Map.foldWithKey E.CExtend (maybe E.CEmpty E.CVar ext) fs
