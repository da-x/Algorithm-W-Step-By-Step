{-# LANGUAGE DeriveGeneric, DeriveFunctor, DeriveFoldable, DeriveTraversable, GeneralizedNewtypeDeriving, RecordWildCards #-}
module Lamdu.Expr.Val
    ( Leaf(..)
    , Body(..)
    , Apply(..), applyFunc, applyArg
    , GetField(..), getFieldRecord, getFieldTag
    , Inject(..), injectRecord, injectTag
    , Lam(..), lamParamId, lamResult
    , RecExtend(..), recTag, recFieldVal, recRest
    , Val(..), body, payload, alphaEq
    , Var(..)
    , GlobalId(..)
    , pPrintUnannotated
    ) where

import Control.Applicative ((<$), (<$>))
import Control.DeepSeq (NFData(..))
import Control.DeepSeq.Generics (genericRnf)
import Control.Lens (Lens')
import Control.Lens.Operators
import Data.Binary (Binary)
import Data.Foldable (Foldable)
import Data.Hashable (Hashable(..))
import Data.Hashable.Generic (gHashWithSalt)
import Data.Maybe (fromMaybe)
import Data.Monoid (Monoid(..))
import Data.String (IsString(..))
import Data.Traversable (Traversable)
import GHC.Generics (Generic)
import Lamdu.Expr.Identifier (Identifier)
import Lamdu.Expr.Type (Tag)
import Text.PrettyPrint ((<+>), (<>))
import Text.PrettyPrint.HughesPJClass (Pretty(..), PrettyLevel, prettyParen)
import qualified Data.Foldable as Foldable
import qualified Data.Map as Map
import qualified Text.PrettyPrint as PP

{-# ANN module "HLint: ignore Use const" #-}

newtype Var = Var { vvName :: Identifier }
    deriving (Eq, Ord, Show, NFData, IsString, Pretty, Binary, Hashable)

newtype GlobalId = GlobalId { globalId :: Identifier }
    deriving (Eq, Ord, Show, NFData, IsString, Pretty, Binary, Hashable)

data Leaf
    =  LVar Var
    |  LGlobal GlobalId
    |  LHole
    |  LLiteralInteger Integer
    |  LRecEmpty
    deriving (Generic, Show, Eq)
instance NFData Leaf where rnf = genericRnf
instance Binary Leaf
instance Hashable Leaf where hashWithSalt = gHashWithSalt

class Match f where
    match :: (a -> b -> c) -> f a -> f b -> Maybe (f c)

data Apply exp = Apply
    { _applyFunc :: exp
    , _applyArg :: exp
    } deriving (Functor, Foldable, Traversable, Generic, Show, Eq)
instance NFData exp => NFData (Apply exp) where rnf = genericRnf
instance Binary exp => Binary (Apply exp)
instance Hashable exp => Hashable (Apply exp) where hashWithSalt = gHashWithSalt
instance Match Apply where
        match f (Apply f0 a0) (Apply f1 a1) = Just $ Apply (f f0 f1) (f a0 a1)

applyFunc :: Lens' (Apply exp) exp
applyFunc f (Apply func arg) = (`Apply` arg) <$> f func

applyArg :: Lens' (Apply exp) exp
applyArg f (Apply func arg) = Apply func <$> f arg

data GetField exp = GetField
    { _getFieldRecord :: exp
    , _getFieldTag :: Tag
    } deriving (Functor, Foldable, Traversable, Generic, Show, Eq)
instance NFData exp => NFData (GetField exp) where rnf = genericRnf
instance Binary exp => Binary (GetField exp)
instance Hashable exp => Hashable (GetField exp) where hashWithSalt = gHashWithSalt
instance Match GetField where
        match f (GetField r0 t0) (GetField r1 t1)
            | t0 == t1 = Just $ GetField (f r0 r1) t0
            | otherwise = Nothing

getFieldRecord :: Lens' (GetField exp) exp
getFieldRecord f GetField {..} = f _getFieldRecord <&> \_getFieldRecord -> GetField {..}

getFieldTag :: Lens' (GetField exp) Tag
getFieldTag f GetField {..} = f _getFieldTag <&> \_getFieldTag -> GetField {..}

data Inject exp = Inject
    { _injectTag :: Tag
    , _injectVal :: exp
    } deriving (Functor, Foldable, Traversable, Generic, Show, Eq)
instance NFData exp => NFData (Inject exp) where rnf = genericRnf
instance Binary exp => Binary (Inject exp)
instance Hashable exp => Hashable (Inject exp) where hashWithSalt = gHashWithSalt
instance Match Inject where
        match f (Inject t0 r0) (Inject t1 r1)
            | t0 == t1 = Just $ Inject t0 (f r0 r1)
            | otherwise = Nothing

injectRecord :: Lens' (Inject exp) exp
injectRecord f Inject {..} = f _injectVal <&> \_injectVal -> Inject {..}

injectTag :: Lens' (Inject exp) Tag
injectTag f Inject {..} = f _injectTag <&> \_injectTag -> Inject {..}

data Lam exp = Lam
    { _lamParamId :: Var
    , _lamResult :: exp
    } deriving (Functor, Foldable, Traversable, Generic, Show, Eq)
instance NFData exp => NFData (Lam exp) where rnf = genericRnf
instance Hashable exp => Hashable (Lam exp) where hashWithSalt = gHashWithSalt
instance Binary exp => Binary (Lam exp)

lamParamId :: Lens' (Lam exp) Var
lamParamId f Lam {..} = f _lamParamId <&> \_lamParamId -> Lam {..}

lamResult :: Lens' (Lam exp) exp
lamResult f Lam {..} = f _lamResult <&> \_lamResult -> Lam {..}

data RecExtend exp = RecExtend
    { _recTag :: Tag
    , _recFieldVal :: exp
    , _recRest :: exp
    } deriving (Functor, Foldable, Traversable, Generic, Show, Eq)
instance NFData exp => NFData (RecExtend exp) where rnf = genericRnf
instance Binary exp => Binary (RecExtend exp)
instance Hashable exp => Hashable (RecExtend exp) where hashWithSalt = gHashWithSalt
instance Match RecExtend where
    match f (RecExtend t0 f0 r0) (RecExtend t1 f1 r1)
        | t0 == t1 = Just $ RecExtend t0 (f f0 f1) (f r0 r1)
        | otherwise = Nothing

recTag :: Lens' (RecExtend exp) Tag
recTag f RecExtend {..} = f _recTag <&> \_recTag -> RecExtend {..}

recFieldVal :: Lens' (RecExtend exp) exp
recFieldVal f RecExtend {..} = f _recFieldVal <&> \_recFieldVal -> RecExtend {..}

recRest :: Lens' (RecExtend exp) exp
recRest f RecExtend {..} = f _recRest <&> \_recRest -> RecExtend {..}

data Body exp
    =  BApp {-# UNPACK #-}!(Apply exp)
    |  BAbs {-# UNPACK #-}!(Lam exp)
    |  BGetField {-# UNPACK #-}!(GetField exp)
    |  BRecExtend {-# UNPACK #-}!(RecExtend exp)
    |  BInject {-# UNPACK #-}!(Inject exp)
    |  BLeaf Leaf
    deriving (Functor, Foldable, Traversable, Generic, Show, Eq)
-- NOTE: Careful of Eq, it's not alpha-eq!
instance NFData exp => NFData (Body exp) where rnf = genericRnf
instance Hashable exp => Hashable (Body exp) where hashWithSalt = gHashWithSalt
instance Binary exp => Binary (Body exp)

data Val a = Val
    { _valPayload :: a
    , _valBody :: !(Body (Val a))
    } deriving (Functor, Foldable, Traversable, Generic, Show, Eq)
instance NFData a => NFData (Val a) where rnf = genericRnf
instance Hashable a => Hashable (Val a) where hashWithSalt = gHashWithSalt
instance Binary a => Binary (Val a)

body :: Lens' (Val a) (Body (Val a))
body f (Val pl b) = Val pl <$> f b

payload :: Lens' (Val a) a
payload f (Val pl b) = (`Val` b) <$> f pl

pPrintPrecBody :: Pretty pl => PrettyLevel -> Rational -> Body (Val pl) -> PP.Doc
pPrintPrecBody lvl prec b =
    case b of
    BLeaf (LVar var)          -> pPrint var
    BLeaf (LGlobal tag)       -> pPrint tag
    BLeaf (LLiteralInteger i) -> pPrint i
    BLeaf LHole               -> PP.text "?"
    BApp (Apply e1 e2)        -> prettyParen (10 < prec) $
                                 pPrintPrec lvl 10 e1 <+> pPrintPrec lvl 11 e2
    BAbs (Lam n e)            -> prettyParen (0 < prec) $
                                 PP.char '\\' <> pPrint n <+>
                                 PP.text "->" <+>
                                 pPrint e
    BGetField (GetField e n)  -> prettyParen (12 < prec) $
                                 pPrintPrec lvl 12 e <> PP.char '.' <> pPrint n
    BInject (Inject n e)      -> prettyParen (12 < prec) $
                                 pPrint n <> PP.char '{' <> pPrintPrec lvl 12 e <> PP.char '}'
    BLeaf LRecEmpty           -> PP.text "V{}"
    BRecExtend (RecExtend tag val rest) ->
                                 PP.text "{" <+>
                                 prField <>
                                 PP.comma <+>
                                 pPrint rest <+>
                                 PP.text "}"
        where
            prField = pPrint tag <+> PP.text "=" <+> pPrint val

instance Pretty a => Pretty (Val a) where
    pPrintPrec lvl prec (Val pl b)
        | PP.isEmpty plDoc = pPrintPrecBody lvl prec b
        | otherwise =
            prettyParen (13 < prec) $ mconcat
            [ pPrintPrecBody lvl 14 b, PP.text "{", plDoc, PP.text "}" ]
        where
            plDoc = pPrintPrec lvl 0 pl

data EmptyDoc = EmptyDoc
instance Pretty EmptyDoc where
    pPrint _ = PP.empty

pPrintUnannotated :: Val a -> PP.Doc
pPrintUnannotated = pPrint . (EmptyDoc <$)

alphaEq :: Val () -> Val () -> Bool
alphaEq =
    go Map.empty
    where
        xToYConv xToY x =
            fromMaybe x $ Map.lookup x xToY
        go xToY (Val _ xBody) (Val _ yBody) =
            case (xBody, yBody) of
            (BAbs (Lam xvar xresult),
              BAbs (Lam yvar yresult)) ->
                go (Map.insert xvar yvar xToY) xresult yresult
            (BLeaf (LVar x), BLeaf (LVar y)) ->
                -- TODO: This is probably not 100% correct for various
                -- shadowing corner cases
                xToYConv xToY x == y
            (BLeaf x, BLeaf y) -> x == y
            (BApp x, BApp y) -> goRecurse x y
            (BGetField x, BGetField y) -> goRecurse x y
            (BRecExtend x, BRecExtend y) -> goRecurse x y
            (_, _) -> False
            where
                goRecurse x y = maybe False Foldable.and $ match (go xToY) x y
