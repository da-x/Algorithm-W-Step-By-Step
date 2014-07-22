module Lamdu.Infer.Internal.FlatRecordType
  ( FlatRecordType(..)
  , from
  , toRecordType
  ) where

import Control.Applicative ((<$>))
import Control.Lens (Lens')
import Control.Lens.Operators
import Data.Map (Map)
import qualified Data.Map as Map
import qualified Lamdu.Expr as E

data FlatRecordType = FlatRecordType
  { _fields :: Map E.Tag E.Type
  , _extension :: Maybe E.RecordTypeVar -- TyVar of more possible fields
  } deriving (Show)

fields :: Lens' FlatRecordType (Map E.Tag E.Type)
fields f (FlatRecordType fs ext) = (`FlatRecordType` ext) <$> f fs

-- From a record type to a sorted list of fields
from :: E.CompositeType -> FlatRecordType
from (E.TRecExtend name typ rest) = from rest & fields %~ Map.insert name typ
from E.TRecEmpty                  = FlatRecordType Map.empty Nothing
from (E.TRecVar name)             = FlatRecordType Map.empty (Just name)

toRecordType :: FlatRecordType -> E.CompositeType
toRecordType (FlatRecordType fs ext) =
  Map.foldWithKey E.TRecExtend (maybe E.TRecEmpty E.TRecVar ext) fs
