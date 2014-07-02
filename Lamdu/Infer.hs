module Lamdu.Infer
  ( typeInference
  ) where

import Control.Applicative ((<$>))
import Control.Lens (mapped)
import Control.Lens.Operators
import Control.Lens.Tuple
import Control.Monad (void)
import Control.Monad.Except (catchError, throwError)
import Control.Monad.State (evalStateT)
import Control.Monad.Trans (lift)
import Data.Map (Map)
import Data.Monoid (Monoid(..))
import Lamdu.Infer.Internal.FlatRecordType (FlatRecordType(..))
import Lamdu.Infer.Internal.FreeTypeVars (FreeTypeVars(..))
import Lamdu.Infer.Internal.Monad (Infer)
import Lamdu.Infer.Internal.Scope (Scope)
import Lamdu.Infer.Scheme (Scheme)
import Lamdu.Pretty ()
import Text.PrettyPrint ((<+>))
import Text.PrettyPrint.HughesPJClass (Pretty(..))
import qualified Control.Monad.State as State
import qualified Control.Monad.Writer as Writer
import qualified Data.Map as M
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Lamdu.Expr as E
import qualified Lamdu.Infer.Internal.FlatRecordType as FlatRecordType
import qualified Lamdu.Infer.Internal.FreeTypeVars as FreeTypeVars
import qualified Lamdu.Infer.Internal.Monad as InferMonad
import qualified Lamdu.Infer.Internal.Scope as Scope
import qualified Lamdu.Infer.Scheme as Scheme
import qualified Lamdu.Infer.TypeVars as TypeVars
import qualified Text.PrettyPrint as PP

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
  do  restTv <- InferMonad.newInferredVar "r"
      ((), s1) <-
        Writer.listen $ varBind tname $
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

unifyRecs :: FlatRecordType -> FlatRecordType -> Infer ()
unifyRecs (FlatRecordType tfields tvar)
          (FlatRecordType ufields uvar) =
  do  let unifyField t u =
              do  old <- State.get
                  ((), s) <- lift $ Writer.listen $ unify (FreeTypeVars.applySubst old t) (FreeTypeVars.applySubst old u)
                  State.put (old `mappend` s)
      (`evalStateT` mempty) . sequence_ . Map.elems $ Map.intersectionWith unifyField tfields ufields
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

class Unify t where
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
      ((), s1) <- Writer.listen $ unify l l'
      unify
        (FreeTypeVars.applySubst s1 r)
        (FreeTypeVars.applySubst s1 r')
  unify (E.TApp l r) (E.TApp l' r') =
    do
      ((), s1) <- Writer.listen $ unify l l'
      unify
        (FreeTypeVars.applySubst s1 r)
        (FreeTypeVars.applySubst s1 r')
  unify (E.TVar u) t                =  varBind u t
  unify t (E.TVar u)                =  varBind u t
  unify (E.TCon t) (E.TCon u)
    | t == u                        =  return ()
  unify (E.TRecord x) (E.TRecord y) =  unify x y
  unify t1 t2                       =  dontUnify t1 t2

  varBind u (E.TVar t) | t == u = return ()
  varBind u t =
    checkOccurs u t $
    Writer.tell $ FreeTypeVars.Subst (Map.singleton u t) mempty

instance Unify E.RecordType where
  unify E.TRecEmpty E.TRecEmpty =  return ()
  unify (E.TRecVar u) t         =  varBind u t
  unify t (E.TRecVar u)         =  varBind u t
  unify t@E.TRecExtend {}
        u@E.TRecExtend {}       =  unifyRecs
                                   (FlatRecordType.from t)
                                   (FlatRecordType.from u)
  unify t1 t2                   =  dontUnify t1 t2

  varBind u (E.TRecVar t) | t == u = return ()
  varBind u t =
    checkOccurs u t $
    Writer.tell $ FreeTypeVars.Subst mempty (Map.singleton u t)

typeInference :: Map E.Tag Scheme -> E.Val a -> Either String (E.Val (E.Type, a))
typeInference globals rootVal =
    do  ((_, t), s) <- InferMonad.run $ infer (,) globals Scope.empty rootVal
        return (t & mapped . _1 %~ FreeTypeVars.applySubst s)

hasField :: E.Tag -> E.RecordType -> Either E.RecordTypeVar Bool
hasField _ E.TRecEmpty   = Right False
hasField _ (E.TRecVar v) = Left v
hasField tag (E.TRecExtend t _ r)
  | tag == t  = Right True
  | otherwise = hasField tag r

infer :: (E.Type -> a -> b) -> Map E.Tag Scheme -> Scope -> E.Val a -> Infer (E.Type, E.Val b)
infer f globals = go
  where
    go locals expr@(E.Val pl body) =
      case body of
      E.VLeaf leaf ->
        mkResult (E.VLeaf leaf) <$>
        case leaf of
        E.VHole -> InferMonad.newInferredVar "h"
        E.VVar n ->
            case Scope.lookupTypeOf n locals of
               Nothing      -> throwError $ show $
                               PP.text "unbound variable:" <+> pPrint n
               Just sigma   -> Scheme.instantiate sigma
        E.VGlobal n ->
            case M.lookup n globals of
               Nothing      -> throwError $ show $
                               PP.text "missing global:" <+> pPrint n
               Just sigma   -> Scheme.instantiate sigma
        E.VLiteralInteger _ -> return (E.TCon "Int")
        E.VRecEmpty -> return $ E.TRecord E.TRecEmpty
      E.VAbs (E.Lam n e) ->
        do  tv <- InferMonad.newInferredVar "a"
            let locals' = Scope.insertTypeOf n (Scheme.specific tv) locals
            ((t1, e'), s1) <- Writer.listen $ go locals' e
            return $ mkResult (E.VAbs (E.Lam n e')) $ E.TFun (FreeTypeVars.applySubst s1 tv) t1
      E.VApp (E.Apply e1 e2) ->
        do  tv <- InferMonad.newInferredVar "a"
            ((t1, e1'), s1) <- Writer.listen $ go locals e1
            ((t2, e2'), s2) <- Writer.listen $ go (FreeTypeVars.applySubst s1 locals) e2
            ((), s3) <- Writer.listen $ unify (FreeTypeVars.applySubst s2 t1) (E.TFun t2 tv)
            return $ mkResult (E.VApp (E.Apply e1' e2')) $ FreeTypeVars.applySubst s3 tv
        `catchError`
        \e -> throwError $ e ++ "\n in " ++ show (pPrint (void expr))
      E.VLet x e1 e2 ->
        do  ((t1, e1'), s1) <- Writer.listen $ go locals e1
            -- TODO: (freeTypeVars (FreeTypeVars.applySubst s1 locals)) makes no sense performance-wise
            -- improve it
            let t' = Scheme.generalize (freeTypeVars (FreeTypeVars.applySubst s1 locals)) t1
                locals' = Scope.insertTypeOf x t' locals
            (t2, e2') <- go (FreeTypeVars.applySubst s1 locals') e2
            return $ mkResult (E.VLet x e1' e2') t2
      E.VGetField (E.GetField e name) ->
        do  tv <- InferMonad.newInferredVar "a"
            tvRec <- InferMonad.newInferredVar "r"
            ((t, e'), s) <- Writer.listen $ go locals e
            ((), su) <- Writer.listen $ unify (FreeTypeVars.applySubst s t) $ E.TRecord $ E.TRecExtend name tv tvRec
            return $ mkResult (E.VGetField (E.GetField e' name)) $ FreeTypeVars.applySubst su tv
      E.VRecExtend name e1 e2 ->
        do  ((t1, e1'), s1) <- Writer.listen $ go locals e1
            ((t2, e2'), _) <- Writer.listen $ go (FreeTypeVars.applySubst s1 locals) e2
            rest <-
              case t2 of
              E.TRecord x ->
                -- In case t2 is already inferred as a TRecord,
                -- verify it doesn't already have this field,
                -- and avoid unnecessary unify from other case
                case hasField name x of
                Right True ->
                  throwError $ show $
                  PP.text "Added field already in record:" <+>
                  pPrint name <+>
                  PP.text " added to " <+>
                  pPrint x
                _ -> return x
              _ -> do
                tv <- InferMonad.newInferredVar "r"
                ((), s) <- Writer.listen $ unify t2 $ E.TRecord tv
                return $ FreeTypeVars.applySubst s tv
            return $ mkResult (E.VRecExtend name e1' e2') $ E.TRecord $ E.TRecExtend name t1 rest
      where
        mkResult body' typ = (typ, E.Val (f typ pl) body')
