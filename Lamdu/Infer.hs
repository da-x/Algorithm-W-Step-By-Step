{-# LANGUAGE DeriveDataTypeable, DeriveGeneric, OverloadedStrings #-}
module Lamdu.Infer
    ( makeScheme
    , TypeVars(..)
    , Loaded(..)
    , infer
    , Scope, emptyScope, Scope.scopeToTypeMap
    , Payload(..), plScope, plType
    , M.Context, M.initialContext
    , M.InferCtx(..), M.inferCtx, Infer
    , M.freshInferredVarName
    , M.freshInferredVar
    ) where

import           Control.Applicative (Applicative(..), (<$), (<$>))
import           Control.DeepSeq (NFData(..))
import           Control.DeepSeq.Generics (genericRnf)
import           Control.Lens (Lens')
import           Control.Lens.Operators
import           Control.Lens.Tuple
import           Control.Monad (unless, guard)
import           Data.Binary (Binary)
import           Data.Map (Map)
import qualified Data.Map as Map
import           Data.Monoid ((<>))
import qualified Data.Set as Set
import           Data.Traversable (sequenceA, traverse)
import           Data.Typeable (Typeable)
import           GHC.Generics (Generic)
import           Lamdu.Expr.Constraints (Constraints)
import qualified Lamdu.Expr.Constraints as Constraints
import qualified Lamdu.Expr.Lens as ExprLens
import           Lamdu.Expr.Nominal (Nominal(..))
import qualified Lamdu.Expr.Nominal as Nominal
import           Lamdu.Expr.Scheme (Scheme(..))
import           Lamdu.Expr.Type (Type)
import qualified Lamdu.Expr.Type as T
import           Lamdu.Expr.TypeVars (TypeVars(..))
import qualified Lamdu.Expr.TypeVars as TV
import           Lamdu.Expr.Val (Val(..))
import qualified Lamdu.Expr.Val as V
import qualified Lamdu.Infer.Error as Err
import           Lamdu.Infer.Internal.Monad (Infer)
import qualified Lamdu.Infer.Internal.Monad as M
import           Lamdu.Infer.Internal.Scheme (makeScheme, generalize)
import qualified Lamdu.Infer.Internal.Scheme as Scheme
import           Lamdu.Infer.Internal.Scope (Scope, emptyScope)
import qualified Lamdu.Infer.Internal.Scope as Scope
import           Lamdu.Infer.Internal.Subst (Subst, CanSubst(..))
import qualified Lamdu.Infer.Internal.Subst as Subst
import           Lamdu.Infer.Internal.Unify (unifyUnsafe)
import           Text.PrettyPrint.HughesPJClass (Pretty(..))

data Payload = Payload
    { _plType :: Type
    , _plScope :: Scope
    } deriving (Generic, Typeable, Show)
instance NFData Payload where rnf = genericRnf
instance Binary Payload

plType :: Lens' Payload Type
plType f pl = (\t' -> pl { _plType = t' }) <$> f (_plType pl)
{-# INLINE plType #-}

plScope :: Lens' Payload Scope
plScope f pl = (\t' -> pl { _plScope = t' }) <$> f (_plScope pl)
{-# INLINE plScope #-}

instance TV.Free Payload where
    free (Payload typ scope) =
        TV.free typ <> TV.free scope

instance CanSubst Payload where
    apply s (Payload typ scope) =
        Payload (Subst.apply s typ) (Subst.apply s scope)

data Loaded = Loaded
    { loadedGlobalTypes :: Map V.GlobalId Scheme
    , loadedNominals :: Map T.Id Nominal
    }

inferSubst :: Loaded -> Scope -> Val a -> Infer (Scope, Val (Payload, a))
inferSubst loaded rootScope val =
    do
        prevSubst <- M.getSubst
        let rootScope' = Subst.apply prevSubst rootScope
        (inferredVal, s) <- M.listenSubst $ inferInternal mkPayload loaded rootScope' val
        return (rootScope', inferredVal <&> _1 %~ Subst.apply s)
    where
        mkPayload typ scope dat = (Payload typ scope, dat)

-- All accessed global IDs are supposed to be extracted from the
-- expression to build this global scope. This is slightly hacky but
-- much faster than a polymorphic monad underlying the InferCtx monad
-- allowing global access.
-- Use loadInfer for a safer interface
infer :: Loaded -> Scope -> Val a -> Infer (Val (Payload, a))
infer loaded scope val =
    do
        ((scope', val'), results) <- M.listenNoTell $ inferSubst loaded scope val
        M.tell $ results & M.subst %~ Subst.intersect (TV.free scope')
        return val'

data CompositeHasTag p = HasTag | DoesNotHaveTag | MayHaveTag (T.Var (T.Composite p))

hasTag :: T.Tag -> T.Composite p -> CompositeHasTag p
hasTag _ T.CEmpty   = DoesNotHaveTag
hasTag _ (T.CVar v) = MayHaveTag v
hasTag tag (T.CExtend t _ r)
    | tag == t  = HasTag
    | otherwise = hasTag tag r

type InferHandler a b =
    (Scope -> a -> Infer (Type, b)) -> Scope ->
    M.Infer (V.Body b, Type)

{-# INLINE inferLeaf #-}
inferLeaf :: Map V.GlobalId Scheme -> V.Leaf -> InferHandler a b
inferLeaf globals leaf = \_go locals ->
    case leaf of
    V.LHole -> M.freshInferredVar "h"
    V.LVar n ->
        case Scope.lookupTypeOf n locals of
        Nothing      -> M.throwError $ Err.UnboundVariable n
        Just t       -> return t
    V.LGlobal n ->
        case Map.lookup n globals of
        Nothing      -> M.throwError $ Err.MissingGlobal n
        Just sigma   -> Scheme.instantiate sigma
    V.LLiteralInteger _ -> return T.TInt
    V.LRecEmpty -> return $ T.TRecord T.CEmpty
    V.LAbsurd ->
        do
            tv <- M.freshInferredVar "a"
            return $ T.TFun (T.TSum T.CEmpty) tv
    <&> (,) (V.BLeaf leaf)

{-# INLINE inferAbs #-}
inferAbs :: V.Lam a -> InferHandler a b
inferAbs (V.Lam n e) = \go locals ->
    do
        tv <- M.freshInferredVar "a"
        let locals' = Scope.insertTypeOf n tv locals
        ((t1, e'), s1) <- M.listenSubst $ go locals' e
        return (V.BAbs (V.Lam n e'), T.TFun (Subst.apply s1 tv) t1)

{-# INLINE inferApply #-}
inferApply :: V.Apply a -> InferHandler a b
inferApply (V.Apply e1 e2) = \go locals ->
    do
        ((p1_t1, e1'), p1_s) <- M.listenSubst $ go locals e1
        let p1 = Subst.apply p1_s

        ((p2_t2, e2'), p2_s) <- M.listenSubst $ go (p1 locals) e2
        let p2_t1 = Subst.apply p2_s p1_t1
        p2_tv <- M.freshInferredVar "a"

        ((), p3_s) <- M.listenSubst $ unifyUnsafe p2_t1 (T.TFun p2_t2 p2_tv)
        let p3_tv = Subst.apply p3_s p2_tv
        return (V.BApp (V.Apply e1' e2'), p3_tv)

{-# INLINE inferGetField #-}
inferGetField :: V.GetField a -> InferHandler a b
inferGetField (V.GetField e name) = \go locals ->
    do
        (p1_t, e') <- go locals e
        p1_tv <- M.freshInferredVar "a"
        p1_tvRecName <- M.freshInferredVarName "r"
        M.tellProductConstraint p1_tvRecName name

        ((), p2_s) <-
            M.listenSubst $ unifyUnsafe p1_t $
            T.TRecord $ T.CExtend name p1_tv $ TV.lift p1_tvRecName
        let p2_tv = Subst.apply p2_s p1_tv
        return (V.BGetField (V.GetField e' name), p2_tv)

{-# INLINE inferInject #-}
inferInject :: V.Inject a -> InferHandler a b
inferInject (V.Inject name e) = \go locals ->
    do
        (t, e') <- go locals e
        tvSumName <- M.freshInferredVarName "s"
        M.tellSumConstraint tvSumName name
        return
            ( V.BInject (V.Inject name e')
            , T.TSum $ T.CExtend name t $ TV.lift tvSumName
            )

{-# INLINE inferCase #-}
inferCase :: V.Case a -> InferHandler a b
inferCase (V.Case name m mm) = \go locals ->
    do
        ((p1_tm, m'), p1_s) <- M.listenSubst $ go locals m
        let p1 = Subst.apply p1_s
        -- p1
        ((p2_tmm, mm'), p2_s) <- M.listenSubst $ go (p1 locals) mm
        let p2 = Subst.apply p2_s
            p2_tm = p2 p1_tm
        -- p2
        p2_tv <- M.freshInferredVar "a"
        p2_tvRes <- M.freshInferredVar "res"
        -- type(match) `unify` a->res
        ((), p3_s) <-
            M.listenSubst $ unifyUnsafe p2_tm $ T.TFun p2_tv p2_tvRes
        let p3 x = Subst.apply p3_s x
            p3_tv    = p3 p2_tv
            p3_tvRes = p3 p2_tvRes
            p3_tmm   = p3 p2_tmm
        -- p3
        -- new sum type var "s":
        tvSumName <- M.freshInferredVarName "s"
        M.tellSumConstraint tvSumName name
        let p3_tvSum = TV.lift tvSumName
        -- type(mismatch) `unify` [ s ]->res
        ((), p4_s) <-
            M.listenSubst $ unifyUnsafe p3_tmm $
            T.TFun (T.TSum p3_tvSum) p3_tvRes
        let p4 x = Subst.apply p4_s x
            p4_tvSum = p4 p3_tvSum
            p4_tvRes = p4 p3_tvRes
            p4_tv    = p4 p3_tv
        -- p4
        return
            ( V.BCase (V.Case name m' mm')
            , T.TFun (T.TSum (T.CExtend name p4_tv p4_tvSum)) p4_tvRes
            )

{-# INLINE inferRecExtend #-}
inferRecExtend :: V.RecExtend a -> InferHandler a b
inferRecExtend (V.RecExtend name e1 e2) = \go locals ->
    do
        ((t1, e1'), s1) <- M.listenSubst $ go locals e1
        ((t2, e2'), s2) <- M.listenSubst $ go (Subst.apply s1 locals) e2
        (rest, s3) <-
            M.listenSubst $
            case t2 of
            T.TRecord x ->
                -- In case t2 is already inferred as a TRecord,
                -- verify it doesn't already have this field,
                -- and avoid unnecessary unify from other case
                case hasTag name x of
                HasTag -> M.throwError $ Err.DuplicateField name x
                DoesNotHaveTag -> return x
                MayHaveTag var -> x <$ M.tellProductConstraint var name
            _ -> do
                tv <- M.freshInferredVarName "r"
                M.tellProductConstraint tv name
                let tve = TV.lift tv
                ((), s) <- M.listenSubst $ unifyUnsafe t2 $ T.TRecord tve
                return $ Subst.apply s tve
        let t1' = Subst.apply s3 $ Subst.apply s2 t1
        return
            ( V.BRecExtend (V.RecExtend name e1' e2')
            , T.TRecord $ T.CExtend name t1' rest
            )

getNominal :: Map T.Id Nominal -> T.Id -> M.Infer Nominal
getNominal nominals name =
    case Map.lookup name nominals of
    Nothing -> M.throwError $ Err.MissingNominal name
    Just nominal -> return nominal

nomTypes :: Map T.Id Nominal -> T.Id -> M.Infer (Type, Scheme)
nomTypes nominals name =
    do
        nominal <- getNominal nominals name
        p1_paramVals <-
            nParams nominal
            & Map.keysSet & Map.fromSet (const (M.freshInferredVar "n"))
            & sequenceA
        return (T.TInst name p1_paramVals, Nominal.apply p1_paramVals nominal)

{-# INLINE inferFromNom #-}
inferFromNom :: Map T.Id Nominal -> V.Nom a -> InferHandler a b
inferFromNom nominals (V.Nom name val) = \go locals ->
    do
        (p1_t, val') <- go locals val
        (p1_outerType, p1_innerScheme) <- nomTypes nominals name
        p1_innerType <- Scheme.instantiate p1_innerScheme
        ((), p2_s) <- M.listenSubst $ unifyUnsafe p1_t p1_outerType
        let p2_innerType = Subst.apply p2_s p1_innerType
        return
            ( V.BFromNom (V.Nom name val')
            , p2_innerType
            )

verifySchemeConstraints :: M.Results -> Scheme -> TypeVars -> Constraints -> Infer ()
verifySchemeConstraints results skolemScheme skolems oldConstraints =
    unless (Constraints.null unexpectedConstraints) $
    M.throwError $ Err.UnexpectedConstraints skolemScheme unexpectedConstraints
    where
        newConstraints = Constraints.intersect skolems $ M._constraints results
        unexpectedConstraints = newConstraints `Constraints.difference` oldConstraints

-- We allow the skolems to be renamed to new type vars only, and
-- remain an independent set
substSkolems :: Subst -> TV.Renames -> Maybe TV.Renames
substSkolems subst (TV.Renames tvs rtvs stvs) =
    TV.Renames
    <$> onRenames ExprLens._TVar tvs
    <*> onRenames ExprLens._CVar rtvs
    <*> onRenames ExprLens._CVar stvs
    where
        onTv prism tv =
            case Subst.lookup tv subst of
            Nothing -> Just tv
            Just typ -> typ ^? prism
        onRenames prism oldRenames =
            oldRenames
            & traverse (onTv prism)
            >>= verify oldRenames
        uniqueElemCount m = Set.size . Set.fromList $ Map.elems m
        verify oldRenames newRenames =
            newRenames <$
            guard (uniqueElemCount newRenames == uniqueElemCount oldRenames)

verifyNoSkolemEscape :: TypeVars -> Scheme -> TypeVars -> M.Infer ()
verifyNoSkolemEscape outerVars skolemScheme skolems =
    unless (TV.null escapedSkolems) $ M.throwError $
    Err.SkolemEscapes skolemScheme $ pPrint escapedSkolems
    where
        escapedSkolems = skolems `TV.intersection` outerVars

verifySkolems :: Scope -> Scheme -> Scheme -> TV.Renames -> Constraints -> M.Results -> Type -> Type -> Infer ()
verifySkolems locals originalExpectedScheme unifiedScheme oldSkolemRenames skolemConstraints results typ outerType =
    do
        let throwErr =
                M.throwError $ Err.ExpectedPolymorphicType originalExpectedScheme $
                generalize (TV.free locals) (M._constraints results) typ
        newSkolemRenames <- substSkolems subst oldSkolemRenames & maybe throwErr return

        let skolemUnifiedScheme = Scheme.applyRenames newSkolemRenames unifiedScheme
        let skolemOriginalExpectedScheme = Scheme.applyRenames newSkolemRenames originalExpectedScheme
        let newSkolems = TV.renameDest newSkolemRenames

        verifySchemeConstraints results skolemUnifiedScheme newSkolems skolemConstraints
        verifyNoSkolemEscape (TV.free locals <> TV.free outerType) skolemOriginalExpectedScheme newSkolems
    where
        subst = M._subst results

{-# INLINE inferToNom #-}
inferToNom :: Map T.Id Nominal -> V.Nom a -> InferHandler a b
inferToNom nominals (V.Nom name val) = \go p0_locals ->
    do
        (mkRes, p1_results) <-
            M.listen $
            do
                ((p1_typ, val'), p1_s) <- go p0_locals val & M.listenSubst
                let p1_locals = Subst.apply p1_s p0_locals
                (p1_outerType, p1_nomScheme) <- nomTypes nominals name
                ((p1_skolemRenames, p1_expectedType), p1_instantiateResults) <-
                    Scheme.instantiateWithRenames p1_nomScheme & M.listen
                let p1_skolemConstraints = M._constraints p1_instantiateResults
                -- The unification here may learn things about skolems, but
                -- not everything is valid to learn! See below.
                ((), p2_s) <- unifyUnsafe p1_expectedType p1_typ & M.listenSubst
                let p2_outerType = Subst.apply p2_s p1_outerType
                if TV.nullRenames p1_skolemRenames
                    then
                    return $
                    \_total_results -> return (V.BToNom (V.Nom name val'), p2_outerType)
                    else do
                        p2_skolemConstraints <- M.applySubstConstraints p2_s p1_skolemConstraints
                        let p2_locals = Subst.apply p2_s p1_locals
                        let p2_nomScheme = Subst.apply p2_s p1_nomScheme
                        let p2_typ = Subst.apply p2_s p1_typ
                        return $ \total_results ->
                            do
                                -- NOTE: p1_skolems is intentionally not
                                -- updated, the update happens within
                                -- verifySkolems
                                verifySkolems p2_locals p1_nomScheme p2_nomScheme p1_skolemRenames p2_skolemConstraints total_results p2_typ p2_outerType
                                return (V.BToNom (V.Nom name val'), p2_outerType)
        mkRes p1_results

inferInternal ::
    (Type -> Scope -> a -> b) ->
    Loaded -> Scope -> Val a -> Infer (Val b)
inferInternal f loaded =
    (fmap . fmap) snd . go
    where
        go locals (Val pl body) =
            ( case body of
              V.BLeaf leaf -> inferLeaf (loadedGlobalTypes loaded) leaf
              V.BAbs lam -> inferAbs lam
              V.BApp app -> inferApply app
              V.BGetField getField -> inferGetField getField
              V.BInject inject -> inferInject inject
              V.BCase case_ -> inferCase case_
              V.BRecExtend recExtend -> inferRecExtend recExtend
              V.BFromNom nom -> inferFromNom (loadedNominals loaded) nom
              V.BToNom nom -> inferToNom (loadedNominals loaded) nom
            ) go locals
            <&> \(body', typ) -> (typ, Val (f typ locals pl) body')
