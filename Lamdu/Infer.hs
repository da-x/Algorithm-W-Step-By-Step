{-# LANGUAGE OverloadedStrings #-}

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
import Lamdu.Infer.Internal.FlatRecordType (FlatRecordType(..))
import Lamdu.Infer.Internal.FreeTypeVars (FreeTypeVars(..))
import Lamdu.Infer.Internal.Monad (Infer)
import Lamdu.Infer.Internal.Scheme (Scheme)
import Lamdu.Infer.Internal.Scope (Scope)
import Lamdu.Infer.Internal.TypeVars (TypeVars(..))
import Lamdu.Pretty (pPrintPureVal, pPrintValUnannotated)
import Text.PrettyPrint ((<+>))
import Text.PrettyPrint.HughesPJClass (Pretty(..))
import qualified Control.Monad.State as State
import qualified Data.Map as M
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Lamdu.Expr as E
import qualified Lamdu.Infer.Internal.FlatRecordType as FlatRecordType
import qualified Lamdu.Infer.Internal.FreeTypeVars as FreeTypeVars
import qualified Lamdu.Infer.Internal.Monad as M
import qualified Lamdu.Infer.Internal.Scheme as Scheme
import qualified Lamdu.Infer.Internal.Scope as Scope
import qualified Lamdu.Infer.Internal.TypeVars as TypeVars
import qualified Text.PrettyPrint as PP

withSubst :: Infer a -> Infer (a, FreeTypeVars.Subst)
withSubst x = M.listen x <&> _2 %~ M.subst

unifyRecToPartial ::
  (Map E.Tag E.Type, E.RecordTypeVar) -> Map E.Tag E.Type ->
  Infer ()
unifyRecToPartial (tfields, tname) ufields
  | not (Map.null uniqueTFields) =
    throwError $ show $
    PP.text "Incompatible record types:" <+>
    pPrint (FlatRecordType.toRecordType (FlatRecordType tfields (Just tname))) <+>
    PP.text " vs. " <+>
    pPrint (FlatRecordType.toRecordType (FlatRecordType ufields Nothing))
  | otherwise = varBind tname $ FlatRecordType.toRecordType $ FlatRecordType uniqueUFields Nothing
  where
    uniqueTFields = tfields `Map.difference` ufields
    uniqueUFields = ufields `Map.difference` tfields

unifyRecPartials ::
  (Map E.Tag E.Type, E.RecordTypeVar) -> (Map E.Tag E.Type, E.RecordTypeVar) ->
  Infer ()
unifyRecPartials (tfields, tname) (ufields, uname) =
  do  restTv <- M.newInferredVar "r"
      ((), s1) <-
        withSubst $ varBind tname $
        Map.foldWithKey E.TRecExtend restTv uniqueUFields
      varBind uname $ FreeTypeVars.applySubst s1 $
        Map.foldWithKey E.TRecExtend restTv uniqueTFields
  where
    uniqueTFields = tfields `Map.difference` ufields
    uniqueUFields = ufields `Map.difference` tfields

unifyRecFulls ::
  Map E.Tag E.Type -> Map E.Tag E.Type -> Infer ()
unifyRecFulls tfields ufields
  | Map.keys tfields /= Map.keys ufields =
    throwError $ show $
    PP.text "Incompatible record types:" <+>
    pPrint (FlatRecordType.toRecordType (FlatRecordType tfields Nothing)) <+>
    PP.text "vs." <+>
    pPrint (FlatRecordType.toRecordType (FlatRecordType ufields Nothing))
  | otherwise = return mempty

unifyChild :: Unify t => t -> t -> StateT FreeTypeVars.Subst Infer ()
unifyChild t u =
    do  old <- State.get
        ((), s) <- lift $ withSubst $ unify (FreeTypeVars.applySubst old t) (FreeTypeVars.applySubst old u)
        State.put (old `mappend` s)

unifyFlattenedRecs :: FlatRecordType -> FlatRecordType -> Infer ()
unifyFlattenedRecs
  (FlatRecordType tfields tvar)
  (FlatRecordType ufields uvar) =
    do
        (`evalStateT` mempty) . sequence_ . Map.elems $ Map.intersectionWith unifyChild tfields ufields
        case (tvar, uvar) of
            (Nothing   , Nothing   ) -> unifyRecFulls tfields ufields
            (Just tname, Just uname) -> unifyRecPartials (tfields, tname) (ufields, uname)
            (Just tname, Nothing   ) -> unifyRecToPartial (tfields, tname) ufields
            (Nothing   , Just uname) -> unifyRecToPartial (ufields, uname) tfields

dontUnify :: Pretty t => t -> t -> Infer ()
dontUnify x y =
  throwError $ show $
  PP.text "types do not unify: " <+> pPrint x <+>
  PP.text "vs." <+> pPrint y

class FreeTypeVars t => Unify t where
  unify :: t -> t -> Infer ()
  varBind :: E.VarOf t -> t -> Infer ()

checkOccurs ::
  (Pretty v, Pretty t, Ord v, TypeVars.Var v, FreeTypeVars t) =>
  v -> t -> Infer () -> Infer ()
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
        (FreeTypeVars.applySubst s1 r)
        (FreeTypeVars.applySubst s1 r')
  unify (E.TInst c0 p0) (E.TInst c1 p1)
    | c0 == c1
      && Map.keys p0 == Map.keys p1 = (`evalStateT` mempty) . sequence_ . Map.elems $
                                      Map.intersectionWith unifyChild p0 p1
  unify (E.TVar u) t                =  varBind u t
  unify t (E.TVar u)                =  varBind u t
  unify (E.TRecord x) (E.TRecord y) =  unify x y
  unify t1 t2                       =  dontUnify t1 t2

  varBind u (E.TVar t) | t == u = return ()
  varBind u t = checkOccurs u t $ M.tellSubst u t

instance Unify E.RecordType where
  unify E.TRecEmpty E.TRecEmpty =  return ()
  unify (E.TRecVar u) t         =  varBind u t
  unify t (E.TRecVar u)         =  varBind u t
  unify t@(E.TRecExtend f0 t0 r0)
        u@(E.TRecExtend f1 t1 r1)
        | f0 == f1              =  do  ((), s) <- withSubst $ unify t0 t1
                                       unify (FreeTypeVars.applySubst s r0)
                                             (FreeTypeVars.applySubst s r1)
        | otherwise             =  unifyFlattenedRecs
                                   (FlatRecordType.from t)
                                   (FlatRecordType.from u)
  unify t1 t2                   =  dontUnify t1 t2

  varBind u (E.TRecVar t) | t == u = return ()
  varBind u t = checkOccurs u t $ M.tellSubst u t

typeInference :: Map E.GlobalId Scheme -> E.Val a -> Either String (Scheme, E.Val (E.Type, a))
typeInference globals rootVal =
  do  ((_, topScheme, val), s) <-
        M.run $ Scheme.generalize Scope.empty $ infer (,) globals Scope.empty rootVal
      return (topScheme, val & mapped . _1 %~ FreeTypeVars.applySubst (M.subst s))

data RecordHasField = HasField | DoesNotHaveField | MayHaveField E.RecordTypeVar

hasField :: E.Tag -> E.RecordType -> RecordHasField
hasField _ E.TRecEmpty   = DoesNotHaveField
hasField _ (E.TRecVar v) = MayHaveField v
hasField tag (E.TRecExtend t _ r)
  | tag == t  = HasField
  | otherwise = hasField tag r

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
        E.VRecEmpty -> return $ E.TRecord E.TRecEmpty
      E.VAbs (E.Lam n e) ->
        do  tv <- M.newInferredVar "a"
            let locals' = Scope.insertTypeOf n tv locals
            ((t1, e'), s1) <- withSubst $ go locals' e
            return $ mkResult (E.VAbs (E.Lam n e')) $ E.TFun (FreeTypeVars.applySubst s1 tv) t1
      E.VApp (E.Apply e1 e2) ->
        do  tv <- M.newInferredVar "a"
            ((t1, e1'), s1) <- withSubst $ go locals e1
            ((t2, e2'), s2) <- withSubst $ go (FreeTypeVars.applySubst s1 locals) e2
            ((), s3) <- withSubst $ unify (FreeTypeVars.applySubst s2 t1) (E.TFun t2 tv)
            return $ mkResult (E.VApp (E.Apply e1' e2')) $ FreeTypeVars.applySubst s3 tv
        `catchError`
        \e -> throwError $ e ++ "\n in " ++ show (pPrintValUnannotated expr)
      E.VGetField (E.GetField e name) ->
        do  tv <- M.newInferredVar "a"
            tvRecName <- M.newInferredVarName "r"
            M.tellConstraint tvRecName name
            ((t, e'), s) <- withSubst $ go locals e
            ((), su) <-
              withSubst $ unify (FreeTypeVars.applySubst s t) $
              E.TRecord $ E.TRecExtend name tv $ E.liftVar tvRecName
            return $ mkResult (E.VGetField (E.GetField e' name)) $ FreeTypeVars.applySubst su tv
      E.VRecExtend (E.RecExtend name e1 e2) ->
        do  ((t1, e1'), s1) <- withSubst $ go locals e1
            ((t2, e2'), _) <- withSubst $ go (FreeTypeVars.applySubst s1 locals) e2
            rest <-
              case t2 of
              E.TRecord x ->
                -- In case t2 is already inferred as a TRecord,
                -- verify it doesn't already have this field,
                -- and avoid unnecessary unify from other case
                case hasField name x of
                HasField ->
                  throwError $ show $
                  PP.text "Added field already in record:" <+>
                  pPrint name <+>
                  PP.text " added to " <+>
                  pPrint x
                DoesNotHaveField -> return x
                MayHaveField var -> x <$ M.tellConstraint var name
              _ -> do
                tv <- M.newInferredVarName "r"
                M.tellConstraint tv name
                let tve = E.liftVar tv
                ((), s) <- withSubst $ unify t2 $ E.TRecord tve
                return $ FreeTypeVars.applySubst s tve
            return $ mkResult (E.VRecExtend (E.RecExtend name e1' e2')) $
              E.TRecord $ E.TRecExtend name t1 rest
      where
        mkResult body' typ = (typ, E.Val (f typ pl) body')
