module FlatRecordType
  ( FlatRecordType(..)
  , flattenRec
  , flattenERec
  , recToType
  ) where

import Control.Applicative ((<$>))
import Control.Lens (Lens')
import Control.Lens.Operators
import Control.Lens.Tuple
import Expr
import qualified Data.Map as Map

data FlatRecordType = FlatRecordType
  { _frFields :: Map.Map String Type
  , _frExtension :: Maybe TypeVar -- TyVar of more possible fields
  } deriving (Show)

frFields :: Lens' FlatRecordType (Map.Map String Type)
frFields f (FlatRecordType fields ext) = (`FlatRecordType` ext) <$> f fields

-- From a record type to a sorted list of fields
flattenRec :: Type -> Either String FlatRecordType
flattenRec (TRecExtend name typ rest) =
  flattenRec rest
  <&> frFields %~ Map.insert name typ
flattenRec TRecEmpty = return $ FlatRecordType Map.empty Nothing
flattenRec (TVar name) = return $ FlatRecordType Map.empty (Just name)
flattenRec t = Left $ "TRecExtend on non-record: " ++ show t

flattenERec :: Expr a -> (Map.Map String (Expr a), Maybe (Expr a))
flattenERec (Expr _ (ERecExtend name val body)) =
  flattenERec body
  & _1 %~ Map.insert name val
flattenERec (Expr _ (ELeaf ERecEmpty)) = (Map.empty, Nothing)
flattenERec other = (Map.empty, Just other)

-- opposite of flatten
recToType :: FlatRecordType -> Type
recToType (FlatRecordType fields extension) =
  Map.foldWithKey TRecExtend (maybe TRecEmpty TVar extension) fields
