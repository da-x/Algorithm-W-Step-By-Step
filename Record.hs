module Record
  ( FlatRecord(..)
  , flattenRec
  , flattenERec
  ) where

import Control.Applicative ((<$>))
import Control.Lens (Lens')
import Control.Lens.Operators
import Control.Lens.Tuple
import Expr
import qualified Data.Map as Map

data FlatRecord = FlatRecord
  { _frFields :: Map.Map String Type
  , _frExtension :: Maybe String -- TyVar of more possible fields
  } deriving (Show)

frFields :: Lens' FlatRecord (Map.Map String Type)
frFields f (FlatRecord fields ext) = (`FlatRecord` ext) <$> f fields

-- From a record type to a sorted list of fields
flattenRec :: Type -> Either String FlatRecord
flattenRec (TRecExtend name typ rest) =
  flattenRec rest
  <&> frFields %~ Map.insert name typ
flattenRec TRecEmpty = return $ FlatRecord Map.empty Nothing
flattenRec (TVar name) = return $ FlatRecord Map.empty (Just name)
flattenRec t = Left $ "TRecExtend on non-record: " ++ show t

flattenERec :: Expr a -> (Map.Map String (Expr a), Maybe (Expr a))
flattenERec (Expr _ (ERecExtend name val body)) =
  flattenERec body
  & _1 %~ Map.insert name val
flattenERec (Expr _ (ELeaf ERecEmpty)) = (Map.empty, Nothing)
flattenERec other = (Map.empty, Just other)
