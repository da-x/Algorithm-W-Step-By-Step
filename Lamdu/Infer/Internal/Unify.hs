-- | Unify support for type ASTs
module Lamdu.Infer.Internal.Unify
    ( unifyUnsafe
    ) where

import           Control.Monad.Trans.Class (lift)
import           Control.Monad.Trans.State (StateT, evalStateT)
import qualified Control.Monad.Trans.State as State
import qualified Data.Foldable as Foldable
import           Data.Map (Map)
import qualified Data.Map as Map
import           Data.Monoid (Monoid(..))
import           Lamdu.Expr.FlatComposite (FlatComposite(..))
import qualified Lamdu.Expr.FlatComposite as FlatComposite
import           Lamdu.Expr.Type (Type)
import qualified Lamdu.Expr.Type as T
import qualified Lamdu.Expr.TypeVars as TypeVars
import qualified Lamdu.Infer.Error as Err
import           Lamdu.Infer.Internal.Monad (Infer)
import qualified Lamdu.Infer.Internal.Monad as M
import           Lamdu.Infer.Internal.Subst (Subst, CanSubst)
import qualified Lamdu.Infer.Internal.Subst as Subst
import           Text.PrettyPrint.HughesPJClass (Pretty(..))

{-# INLINE unifyUnsafe #-}
unifyUnsafe :: Type -> Type -> Infer ()
unifyUnsafe = unifyGeneric

class CanSubst t => Unify t where
    unifyGeneric :: t -> t -> Infer ()
    varBind :: T.Var t -> t -> Infer ()

closedRecord :: Map T.Tag Type -> T.Composite p
closedRecord fields = FlatComposite.toComposite (FlatComposite fields Nothing)

unifyFlatToPartial ::
    Subst.CompositeHasVar p =>
    Subst -> (Map T.Tag Type, T.Var (T.Composite p)) -> Map T.Tag Type ->
    Infer ()
unifyFlatToPartial s (tfields, tname) ufields
    | not (Map.null uniqueTFields) =
        M.throwError $
        Err.TypesDoNotUnity
        (pPrint (FlatComposite.toComposite (FlatComposite tfields (Just tname))))
        (pPrint (closedRecord ufields))
    | otherwise =
            varBind tname $
            Subst.apply s $
            FlatComposite.toComposite $ FlatComposite uniqueUFields Nothing
    where
        uniqueTFields = tfields `Map.difference` ufields
        uniqueUFields = ufields `Map.difference` tfields

unifyFlatPartials ::
    Subst.CompositeHasVar p =>
    Subst ->
    (Map T.Tag Type, T.Var (T.Composite p)) ->
    (Map T.Tag Type, T.Var (T.Composite p)) ->
    Infer ()
unifyFlatPartials s0 (tfields, tname) (ufields, uname) =
    do
        restTv <- M.freshInferredVar "r"
        ((), s1) <-
            M.listenSubst $ varBind tname $
            Subst.apply s0 $
            Map.foldWithKey T.CExtend restTv uniqueUFields
        varBind uname $ Subst.apply (mappend s0 s1) $
            Map.foldWithKey T.CExtend restTv uniqueTFields
    where
        uniqueTFields = tfields `Map.difference` ufields
        uniqueUFields = ufields `Map.difference` tfields

unifyFlatFulls ::
    Map T.Tag Type -> Map T.Tag Type -> Infer ()
unifyFlatFulls tfields ufields
    | Map.keys tfields /= Map.keys ufields =
        M.throwError $
        Err.TypesDoNotUnity
        (pPrint (closedRecord tfields))
        (pPrint (closedRecord ufields))
    | otherwise = return mempty

unifyChild :: Unify t => t -> t -> StateT Subst Infer ()
unifyChild t u =
    do
        old <- State.get
        ((), s) <- lift $ M.listenSubst $ unifyGeneric (Subst.apply old t) (Subst.apply old u)
        State.put (old `mappend` s)

unifyIntersection :: (Unify a, Ord k) => Map k a -> Map k a -> Infer ()
unifyIntersection tfields ufields =
    (`evalStateT` mempty) . Foldable.sequence_ $
    Map.intersectionWith unifyChild tfields ufields

unifyFlattened ::
    Subst.CompositeHasVar p => FlatComposite p -> FlatComposite p -> Infer ()
unifyFlattened
    (FlatComposite tfields tvar)
    (FlatComposite ufields uvar) =
        do
                ((), s) <- M.listenSubst $ unifyIntersection tfields ufields
                case (tvar, uvar) of
                        (Nothing   , Nothing   ) -> unifyFlatFulls tfields ufields
                        (Just tname, Just uname) -> unifyFlatPartials s (tfields, tname) (ufields, uname)
                        (Just tname, Nothing   ) -> unifyFlatToPartial s (tfields, tname) ufields
                        (Nothing   , Just uname) -> unifyFlatToPartial s (ufields, uname) tfields

dontUnify :: Pretty t => t -> t -> Infer ()
dontUnify x y =
    M.throwError $ Err.TypesDoNotUnity (pPrint x) (pPrint y)

checkOccurs ::
    (Pretty t, Subst.HasVar t) =>
    T.Var t -> t -> Infer () -> Infer ()
checkOccurs var typ act
    | var `TypeVars.member` TypeVars.free typ =
        M.throwError $ Err.OccursCheckFail (pPrint var) (pPrint typ)
    | otherwise =
        act

instance Unify Type where
    unifyGeneric (T.TFun l r) (T.TFun l' r') =
        do
            ((), s1) <- M.listenSubst $ unifyGeneric l l'
            unifyGeneric
                (Subst.apply s1 r)
                (Subst.apply s1 r')
    unifyGeneric (T.TInst c0 p0) (T.TInst c1 p1)
        | c0 == c1 && Map.keys p0 == Map.keys p1 = unifyIntersection p0 p1
    unifyGeneric (T.TVar u) t                =  varBind u t
    unifyGeneric t (T.TVar u)                =  varBind u t
    unifyGeneric (T.TRecord x) (T.TRecord y) =  unifyGeneric x y
    unifyGeneric (T.TSum x)    (T.TSum y)    =  unifyGeneric x y
    unifyGeneric T.TInt        T.TInt        =  return ()
    unifyGeneric t1 t2                       =  dontUnify t1 t2

    varBind u (T.TVar t) | t == u = return ()
    varBind u t = checkOccurs u t $ M.tellSubst u t

instance Subst.CompositeHasVar p => Unify (T.Composite p) where
    unifyGeneric T.CEmpty T.CEmpty       =  return ()
    unifyGeneric (T.CVar u) t            =  varBind u t
    unifyGeneric t (T.CVar u)            =  varBind u t
    unifyGeneric
        t@(T.CExtend f0 t0 r0)
        u@(T.CExtend f1 t1 r1)
        | f0 == f1 =
              do
                  ((), s) <- M.listenSubst $ unifyGeneric t0 t1
                  unifyGeneric (Subst.apply s r0) (Subst.apply s r1)
        | otherwise =
              unifyFlattened
              (FlatComposite.fromComposite t)
              (FlatComposite.fromComposite u)
    unifyGeneric t1 t2                   =  dontUnify t1 t2

    varBind u (T.CVar t) | t == u = return ()
    varBind u t = checkOccurs u t $ M.tellSubst u t
