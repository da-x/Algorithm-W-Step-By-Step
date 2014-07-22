{-# LANGUAGE FlexibleContexts, FlexibleInstances, OverloadedStrings #-}

module Lamdu.Infer
  ( Constraints(..), Scheme(..), TypeVars(..), typeInference
  , pPrintPureVal, pPrintValUnannotated
  ) where

import Control.Applicative ((<$), (<$>))
import Control.Lens (mapped)
import Control.Lens.Operators
import Control.Lens.Tuple
import Control.Monad.Except (catchError, throwError)
import Control.Monad.State (StateT, evalStateT)
import Control.Monad.Trans (lift)
import Data.Map (Map)
import Data.Monoid (Monoid(..))
import Lamdu.Infer.Internal.Constraints (Constraints(..))
import Lamdu.Infer.Internal.FlatComposite (FlatComposite(..))
import Lamdu.Infer.Internal.Monad (Infer)
import Lamdu.Infer.Internal.Scheme (Scheme)
import Lamdu.Infer.Internal.Scope (Scope)
import Lamdu.Infer.Internal.TypeVars (TypeVars(..), HasVar(..), FreeTypeVars(..), CompositeHasVar)
import Lamdu.Pretty (pPrintPureVal, pPrintValUnannotated)
import Text.PrettyPrint ((<+>))
import Text.PrettyPrint.HughesPJClass (Pretty(..))
import qualified Control.Monad.State as State
import qualified Data.Foldable as Foldable
import qualified Data.Map as M
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Lamdu.Expr as E
import qualified Lamdu.Infer.Internal.FlatComposite as FlatComposite
import qualified Lamdu.Infer.Internal.Monad as M
import qualified Lamdu.Infer.Internal.Scheme as Scheme
import qualified Lamdu.Infer.Internal.Scope as Scope
import qualified Lamdu.Infer.Internal.TypeVars as TypeVars
import qualified Text.PrettyPrint as PP

closedRecord :: Map E.Tag E.Type -> E.CompositeType p
closedRecord fields = FlatComposite.toRecordType (FlatComposite fields Nothing)

withSubst :: Infer a -> Infer (a, TypeVars.Subst)
withSubst x = M.listen x <&> _2 %~ M.subst

unifyFlatToPartial ::
  Unify (E.CompositeType p) =>
  (Map E.Tag E.Type, E.TypeVar (E.CompositeType p)) -> Map E.Tag E.Type ->
  Infer ()
unifyFlatToPartial (tfields, tname) ufields
  | not (Map.null uniqueTFields) =
    throwError $ show $
    PP.text "Incompatible record types:" <+>
    pPrint (FlatComposite.toRecordType (FlatComposite tfields (Just tname))) <+>
    PP.text " vs. " <+>
    pPrint (closedRecord ufields)
  | otherwise = varBind tname $ FlatComposite.toRecordType $ FlatComposite uniqueUFields Nothing
  where
    uniqueTFields = tfields `Map.difference` ufields
    uniqueUFields = ufields `Map.difference` tfields

unifyFlatPartials ::
  (CompositeHasVar p, Unify (E.CompositeType p)) =>
  (Map E.Tag E.Type, E.TypeVar (E.CompositeType p)) ->
  (Map E.Tag E.Type, E.TypeVar (E.CompositeType p)) ->
  Infer ()
unifyFlatPartials (tfields, tname) (ufields, uname) =
  do  restTv <- M.newInferredVar "r"
      ((), s1) <-
        withSubst $ varBind tname $
        Map.foldWithKey E.CExtend restTv uniqueUFields
      varBind uname $ TypeVars.applySubst s1 $
        Map.foldWithKey E.CExtend restTv uniqueTFields
  where
    uniqueTFields = tfields `Map.difference` ufields
    uniqueUFields = ufields `Map.difference` tfields

unifyFlatFulls ::
  Map E.Tag E.Type -> Map E.Tag E.Type -> Infer ()
unifyFlatFulls tfields ufields
  | Map.keys tfields /= Map.keys ufields =
    throwError $ show $
    PP.text "Incompatible record types:" <+>
    pPrint (closedRecord tfields) <+>
    PP.text "vs." <+>
    pPrint (closedRecord ufields)
  | otherwise = return mempty

unifyChild :: Unify t => t -> t -> StateT TypeVars.Subst Infer ()
unifyChild t u =
    do  old <- State.get
        ((), s) <- lift $ withSubst $ unify (TypeVars.applySubst old t) (TypeVars.applySubst old u)
        State.put (old `mappend` s)

unifyFlattened ::
  (CompositeHasVar p, Unify (E.CompositeType p)) =>
  FlatComposite p -> FlatComposite p -> Infer ()
unifyFlattened
  (FlatComposite tfields tvar)
  (FlatComposite ufields uvar) =
    do
        (`evalStateT` mempty) . Foldable.sequence_ $ Map.intersectionWith unifyChild tfields ufields
        case (tvar, uvar) of
            (Nothing   , Nothing   ) -> unifyFlatFulls tfields ufields
            (Just tname, Just uname) -> unifyFlatPartials (tfields, tname) (ufields, uname)
            (Just tname, Nothing   ) -> unifyFlatToPartial (tfields, tname) ufields
            (Nothing   , Just uname) -> unifyFlatToPartial (ufields, uname) tfields

dontUnify :: Pretty t => t -> t -> Infer ()
dontUnify x y =
  throwError $ show $
  PP.text "types do not unify: " <+> pPrint x <+>
  PP.text "vs." <+> pPrint y

class FreeTypeVars t => Unify t where
  unify :: t -> t -> Infer ()
  varBind :: E.TypeVar t -> t -> Infer ()

checkOccurs ::
  (Pretty t, TypeVars.HasVar t, FreeTypeVars t) =>
  E.TypeVar t -> t -> Infer () -> Infer ()
checkOccurs var typ act
  | var `Set.member` TypeVars.getVars (freeTypeVars typ) =
    throwError $ show $
    PP.text "occurs check fails:" <+>
    pPrint var <+> PP.text "vs." <+> pPrint typ
  | otherwise =
    act

instance Unify E.Type where
  unify (E.TFun l r) (E.TFun l' r') =
    do
      ((), s1) <- withSubst $ unify l l'
      unify
        (TypeVars.applySubst s1 r)
        (TypeVars.applySubst s1 r')
  unify (E.TInst c0 p0) (E.TInst c1 p1)
    | c0 == c1
      && Map.keys p0 == Map.keys p1 = (`evalStateT` mempty) . Foldable.sequence_ $
                                      Map.intersectionWith unifyChild p0 p1
  unify (E.TVar u) t                =  varBind u t
  unify t (E.TVar u)                =  varBind u t
  unify (E.TRecord x) (E.TRecord y) =  unify x y
  unify t1 t2                       =  dontUnify t1 t2

  varBind u (E.TVar t) | t == u = return ()
  varBind u t = checkOccurs u t $ M.tellSubst u t

instance Unify E.ProductType where
  unify E.CEmpty E.CEmpty       =  return ()
  unify (E.CVar u) t            =  varBind u t
  unify t (E.CVar u)            =  varBind u t
  unify t@(E.CExtend f0 t0 r0)
        u@(E.CExtend f1 t1 r1)
        | f0 == f1              =  do  ((), s) <- withSubst $ unify t0 t1
                                       unify (TypeVars.applySubst s r0)
                                             (TypeVars.applySubst s r1)
        | otherwise             =  unifyFlattened
                                   (FlatComposite.from t)
                                   (FlatComposite.from u)
  unify t1 t2                   =  dontUnify t1 t2

  varBind u (E.CVar t) | t == u = return ()
  varBind u t = checkOccurs u t $ M.tellSubst u t

typeInference :: Map E.GlobalId Scheme -> E.Val a -> Either String (Scheme, E.Val (E.Type, a))
typeInference globals rootVal =
  do  ((_, topScheme, val), s) <-
        M.run $ Scheme.generalize Scope.empty $ infer (,) globals Scope.empty rootVal
      return (topScheme, val & mapped . _1 %~ TypeVars.applySubst (M.subst s))

data CompositeHasTag p = HasTag | DoesNotHaveTag | MayHaveTag (E.TypeVar (E.CompositeType p))

hasTag :: E.Tag -> E.CompositeType p -> CompositeHasTag p
hasTag _ E.CEmpty   = DoesNotHaveTag
hasTag _ (E.CVar v) = MayHaveTag v
hasTag tag (E.CExtend t _ r)
  | tag == t  = HasTag
  | otherwise = hasTag tag r

infer :: (E.Type -> a -> b) -> Map E.GlobalId Scheme -> Scope -> E.Val a -> Infer (E.Type, E.Val b)
infer f globals = go
  where
    go locals expr@(E.Val pl body) =
      case body of
      E.VLeaf leaf ->
        mkResult (E.VLeaf leaf) <$>
        case leaf of
        E.VHole -> M.newInferredVar "h"
        E.VVar n ->
            case Scope.lookupTypeOf n locals of
               Nothing      -> throwError $ show $
                               PP.text "unbound variable:" <+> pPrint n
               Just t       -> return t
        E.VGlobal n ->
            case M.lookup n globals of
               Nothing      -> throwError $ show $
                               PP.text "missing global:" <+> pPrint n
               Just sigma   -> Scheme.instantiate sigma
        E.VLiteralInteger _ -> return (E.TInst "Int" mempty)
        E.VRecEmpty -> return $ E.TRecord E.CEmpty
      E.VAbs (E.Lam n e) ->
        do  tv <- M.newInferredVar "a"
            let locals' = Scope.insertTypeOf n tv locals
            ((t1, e'), s1) <- withSubst $ go locals' e
            return $ mkResult (E.VAbs (E.Lam n e')) $ E.TFun (TypeVars.applySubst s1 tv) t1
      E.VApp (E.Apply e1 e2) ->
        do  tv <- M.newInferredVar "a"
            ((t1, e1'), s1) <- withSubst $ go locals e1
            ((t2, e2'), s2) <- withSubst $ go (TypeVars.applySubst s1 locals) e2
            ((), s3) <- withSubst $ unify (TypeVars.applySubst s2 t1) (E.TFun t2 tv)
            return $ mkResult (E.VApp (E.Apply e1' e2')) $ TypeVars.applySubst s3 tv
        `catchError`
        \e -> throwError $ e ++ "\n in " ++ show (pPrintValUnannotated expr)
      E.VGetField (E.GetField e name) ->
        do  tv <- M.newInferredVar "a"
            tvRecName <- M.newInferredVarName "r"
            M.tellConstraint tvRecName name
            ((t, e'), s) <- withSubst $ go locals e
            ((), su) <-
              withSubst $ unify (TypeVars.applySubst s t) $
              E.TRecord $ E.CExtend name tv $ liftVar tvRecName
            return $ mkResult (E.VGetField (E.GetField e' name)) $ TypeVars.applySubst su tv
      E.VRecExtend (E.RecExtend name e1 e2) ->
        do  ((t1, e1'), s1) <- withSubst $ go locals e1
            ((t2, e2'), _) <- withSubst $ go (TypeVars.applySubst s1 locals) e2
            rest <-
              case t2 of
              E.TRecord x ->
                -- In case t2 is already inferred as a TRecord,
                -- verify it doesn't already have this field,
                -- and avoid unnecessary unify from other case
                case hasTag name x of
                HasTag ->
                  throwError $ show $
                  PP.text "Added field already in record:" <+>
                  pPrint name <+>
                  PP.text " added to " <+>
                  pPrint x
                DoesNotHaveTag -> return x
                MayHaveTag var -> x <$ M.tellConstraint var name
              _ -> do
                tv <- M.newInferredVarName "r"
                M.tellConstraint tv name
                let tve = liftVar tv
                ((), s) <- withSubst $ unify t2 $ E.TRecord tve
                return $ TypeVars.applySubst s tve
            return $ mkResult (E.VRecExtend (E.RecExtend name e1' e2')) $
              E.TRecord $ E.CExtend name t1 rest
      where
        mkResult body' typ = (typ, E.Val (f typ pl) body')
