module Lamdu.Infer.Internal.Subst
    ( HasVar(..), CompositeHasVar
    , Subst(..), intersect
    , CanSubst(..)
    ) where

import Prelude hiding (null)

import Control.Applicative ((<$>))
import Data.Map (Map)
import Data.Maybe (fromMaybe)
import Data.Monoid (Monoid(..))
import Data.Set (Set)
import Lamdu.Expr.Type (Type)
import Lamdu.Expr.TypeVars (TypeVars(..))
import Text.PrettyPrint (text, vcat, (<>))
import Text.PrettyPrint.HughesPJClass (Pretty(..))
import qualified Data.Map as Map
import qualified Lamdu.Expr.Type as T
import qualified Lamdu.Expr.TypeVars as TypeVars

type SubSubst t = Map (T.Var t) t

data Subst = Subst
    { substTypes :: SubSubst Type
    , substRecordTypes :: SubSubst (T.Composite T.Product)
    , substSumTypes :: SubSubst (T.Composite T.Sum)
    } deriving Show

null :: Subst -> Bool
null (Subst t r s) = Map.null t && Map.null r && Map.null s

unionDisjoint :: (Pretty a, Pretty k, Ord k) => Map k a -> Map k a -> Map k a
unionDisjoint m1 m2 =
    Map.unionWithKey collision m1 m2
    where
        collision k v0 v1 =
            error $ show $ vcat
            [ text "Given non-disjoint maps! Key=" <> pPrint k
            , text " V0=" <> pPrint v0
            , text " V1=" <> pPrint v1
            , text " in " <> pPrint (Map.toList m1)
            , text " vs " <> pPrint (Map.toList m2)
            ]

instance Monoid Subst where
    mempty = Subst Map.empty Map.empty Map.empty
    mappend subst0@(Subst t0 r0 s0) subst1@(Subst t1 r1 s1)
        | null subst1 = subst0
        | otherwise =
        Subst
        (t1 `unionDisjoint` Map.map (apply subst1) t0)
        (r1 `unionDisjoint` Map.map (apply subst1) r0)
        (s1 `unionDisjoint` Map.map (apply subst1) s0)

intersectMapSet :: Ord k => Set k -> Map k a -> Map k a
intersectMapSet s m = Map.intersection m $ Map.fromSet (const ()) s

intersect :: TypeVars -> Subst -> Subst
intersect (TypeVars tvs rtvs stvs) (Subst ts rs ss) =
    Subst (intersectMapSet tvs ts) (intersectMapSet rtvs rs) (intersectMapSet stvs ss)

class TypeVars.Free a => CanSubst a where
    apply   :: Subst -> a -> a

class (TypeVars.VarKind t, CanSubst t) => HasVar t where
    new :: T.Var t -> t -> Subst

class TypeVars.CompositeVarKind p => CompositeHasVar p where
    compositeNew :: SubSubst (T.Composite p) -> Subst
    compositeGet :: Subst -> SubSubst (T.Composite p)

instance CompositeHasVar p => CanSubst (T.Composite p) where
    apply _ T.CEmpty          = T.CEmpty
    apply s (T.CVar n)        = fromMaybe (T.CVar n) $ Map.lookup n (compositeGet s)
    apply s (T.CExtend n t r) = T.CExtend n (apply s t) (apply s r)

instance CanSubst Type where
    apply s (T.TVar n)      = fromMaybe (T.TVar n) $ Map.lookup n (substTypes s)
    apply s (T.TInst n p)   = T.TInst n $ apply s <$> p
    apply s (T.TFun t1 t2)  = T.TFun (apply s t1) (apply s t2)
    apply s (T.TRecord r)   = T.TRecord $ apply s r
    apply s (T.TSum r)      = T.TSum $ apply s r

instance HasVar Type where
    {-# INLINE new #-}
    new tv t = mempty { substTypes = Map.singleton tv t }

instance CompositeHasVar T.Product where
    compositeGet = substRecordTypes
    {-# INLINE compositeNew #-}
    compositeNew v = mempty { substRecordTypes = v }

instance CompositeHasVar T.Sum where
    compositeGet = substSumTypes
    {-# INLINE compositeNew #-}
    compositeNew v = mempty { substSumTypes = v }

instance CompositeHasVar p => HasVar (T.Composite p) where
    new tv t = compositeNew $ Map.singleton tv t
    {-# INLINE new #-}
