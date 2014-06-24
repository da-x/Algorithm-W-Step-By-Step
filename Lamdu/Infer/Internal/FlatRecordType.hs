module Lamdu.Infer.Internal.FlatRecordType
  ( FlatRecordType(..)
  , from
  , toType
  ) where

import Control.Applicative ((<$>))
import Control.Lens (Lens')
import Control.Lens.Operators
import Data.Map (Map)
import qualified Data.Map as Map
import qualified Lamdu.Expr as E

data FlatRecordType = FlatRecordType
  { _fields :: Map String E.Type
  , _extension :: Maybe E.TypeVar -- TyVar of more possible fields
  } deriving (Show)

fields :: Lens' FlatRecordType (Map String E.Type)
fields f (FlatRecordType fs ext) = (`FlatRecordType` ext) <$> f fs

-- From a record type to a sorted list of fields
from :: E.Type -> Either String FlatRecordType
from (E.TRecExtend name typ rest) =
  from rest <&> fields %~ Map.insert name typ
from E.TRecEmpty = return $ FlatRecordType Map.empty Nothing
from (E.TVar name) = return $ FlatRecordType Map.empty (Just name)
from t = Left $ "TRecExtend on non-record: " ++ show t

toType :: FlatRecordType -> E.Type
toType (FlatRecordType fs ext) =
  Map.foldWithKey E.TRecExtend (maybe E.TRecEmpty E.TVar ext) fs
