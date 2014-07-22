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
import qualified Lamdu.Expr as E

data FlatComposite = FlatComposite
  { _fields :: Map E.Tag E.Type
  , _extension :: Maybe E.RecordTypeVar -- TyVar of more possible fields
  } deriving (Show)

fields :: Lens' FlatComposite (Map E.Tag E.Type)
fields f (FlatComposite fs ext) = (`FlatComposite` ext) <$> f fs

-- From a record type to a sorted list of fields
from :: E.CompositeType E.RecordTypeVar -> FlatComposite
from (E.TRecExtend name typ rest) = from rest & fields %~ Map.insert name typ
from E.TRecEmpty                  = FlatComposite Map.empty Nothing
from (E.TRecVar name)             = FlatComposite Map.empty (Just name)

toRecordType :: FlatComposite -> E.CompositeType E.RecordTypeVar
toRecordType (FlatComposite fs ext) =
  Map.foldWithKey E.TRecExtend (maybe E.TRecEmpty E.TRecVar ext) fs
