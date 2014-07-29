module Lamdu.Infer.Internal.Unify
  ( unify
  ) where

import Control.Monad.Except (throwError)
import Control.Monad.State (StateT, evalStateT)
import Control.Monad.Trans (lift)
import Data.Map (Map)
import Data.Monoid (Monoid(..))
import Lamdu.Infer.Internal.FlatComposite (FlatComposite(..))
import Lamdu.Infer.Internal.Monad (Infer)
import Lamdu.Infer.Internal.TypeVars (CompositeHasVar)
import Text.PrettyPrint ((<+>))
import Text.PrettyPrint.HughesPJClass (Pretty(..))
import qualified Control.Monad.State as State
import qualified Data.Foldable as Foldable
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Lamdu.Expr as E
import qualified Lamdu.Infer.Internal.FlatComposite as FlatComposite
import qualified Lamdu.Infer.Internal.Monad as M
import qualified Lamdu.Infer.Internal.TypeVars as TypeVars
import qualified Text.PrettyPrint as PP

class TypeVars.Free t => Unify t where
  unify :: t -> t -> Infer ()
  varBind :: E.TypeVar t -> t -> Infer ()

closedRecord :: Map E.Tag E.Type -> E.CompositeType p
closedRecord fields = FlatComposite.toRecordType (FlatComposite fields Nothing)

unifyFlatToPartial ::
  CompositeHasVar p =>
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
  CompositeHasVar p =>
  (Map E.Tag E.Type, E.TypeVar (E.CompositeType p)) ->
  (Map E.Tag E.Type, E.TypeVar (E.CompositeType p)) ->
  Infer ()
unifyFlatPartials (tfields, tname) (ufields, uname) =
  do  restTv <- M.newInferredVar "r"
      ((), s1) <-
        M.listenSubst $ varBind tname $
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
        ((), s) <- lift $ M.listenSubst $ unify (TypeVars.applySubst old t) (TypeVars.applySubst old u)
        State.put (old `mappend` s)

unifyFlattened ::
  CompositeHasVar p => FlatComposite p -> FlatComposite p -> Infer ()
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

checkOccurs ::
  (Pretty t, TypeVars.HasVar t, TypeVars.Free t) =>
  E.TypeVar t -> t -> Infer () -> Infer ()
checkOccurs var typ act
  | var `Set.member` TypeVars.getVars (TypeVars.free typ) =
    throwError $ show $
    PP.text "occurs check fails:" <+>
    pPrint var <+> PP.text "vs." <+> pPrint typ
  | otherwise =
    act

instance Unify E.Type where
  unify (E.TFun l r) (E.TFun l' r') =
    do
      ((), s1) <- M.listenSubst $ unify l l'
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

instance TypeVars.CompositeHasVar p => Unify (E.CompositeType p) where
  unify E.CEmpty E.CEmpty       =  return ()
  unify (E.CVar u) t            =  varBind u t
  unify t (E.CVar u)            =  varBind u t
  unify t@(E.CExtend f0 t0 r0)
        u@(E.CExtend f1 t1 r1)
        | f0 == f1              =  do  ((), s) <- M.listenSubst $ unify t0 t1
                                       unify (TypeVars.applySubst s r0)
                                             (TypeVars.applySubst s r1)
        | otherwise             =  unifyFlattened
                                   (FlatComposite.from t)
                                   (FlatComposite.from u)
  unify t1 t2                   =  dontUnify t1 t2

  varBind u (E.CVar t) | t == u = return ()
  varBind u t = checkOccurs u t $ M.tellSubst u t
