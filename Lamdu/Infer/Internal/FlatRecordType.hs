module Lamdu.Infer.Internal.FlatRecordType
  ( FlatRecordType(..)
  , from
  , toType
  ) where

import Control.Applicative ((<$>))
import Control.Lens (Lens')
import Control.Lens.Operators
import Lamdu.Expr
import qualified Data.Map as Map

data FlatRecordType = FlatRecordType
  { _fields :: Map.Map String Type
  , _extension :: Maybe TypeVar -- TyVar of more possible fields
  } deriving (Show)

fields :: Lens' FlatRecordType (Map.Map String Type)
fields f (FlatRecordType fs ext) = (`FlatRecordType` ext) <$> f fs

-- From a record type to a sorted list of fields
from :: Type -> Either String FlatRecordType
from (TRecExtend name typ rest) =
  from rest <&> fields %~ Map.insert name typ
from TRecEmpty = return $ FlatRecordType Map.empty Nothing
from (TVar name) = return $ FlatRecordType Map.empty (Just name)
from t = Left $ "TRecExtend on non-record: " ++ show t

toType :: FlatRecordType -> Type
toType (FlatRecordType fs ext) =
  Map.foldWithKey TRecExtend (maybe TRecEmpty TVar ext) fs
