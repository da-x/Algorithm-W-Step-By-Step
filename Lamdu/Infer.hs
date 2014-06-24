module Lamdu.Infer
  ( typeInference
  ) where

import Control.Applicative ((<$>), Applicative(..))
import Control.Lens (mapped)
import Control.Lens.Operators
import Control.Lens.Tuple
import Control.Monad (void, join)
import Control.Monad.Error (throwError, catchError)
import Control.Monad.State (evalStateT)
import Control.Monad.Trans (lift)
import Data.Map (Map)
import Data.Monoid (Monoid(..))
import Lamdu.Infer.Internal.FlatRecordType (FlatRecordType(..))
import Lamdu.Infer.Internal.FreeTypeVars (freeTypeVars)
import Lamdu.Infer.Internal.Monad (InferW)
import Lamdu.Infer.Scope (Scope)
import Lamdu.Pretty ()
import Text.PrettyPrint ((<+>))
import Text.PrettyPrint.HughesPJClass (Pretty(..))
import qualified Control.Monad.State as State
import qualified Control.Monad.Writer as Writer
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Lamdu.Expr as E
import qualified Lamdu.Infer.Internal.FlatRecordType as FlatRecordType
import qualified Lamdu.Infer.Internal.FreeTypeVars as FreeTypeVars
import qualified Lamdu.Infer.Internal.Monad as InferMonad
import qualified Lamdu.Infer.Scheme as Scheme
import qualified Lamdu.Infer.Scope as Scope
import qualified Text.PrettyPrint as PP

varBind :: E.TypeVar -> E.Type -> InferW ()
varBind u (E.TVar t) | t == u          =  return ()
varBind u t
  | u `Set.member` freeTypeVars t  =
    throwError $ show $
    PP.text "occurs check fails:" <+>
    pPrint u <+> PP.text "vs." <+> pPrint t
  | otherwise = Writer.tell $ FreeTypeVars.substFromList [(u, t)]

unifyRecToPartial ::
  (Map String E.Type, E.TypeVar) -> Map String E.Type ->
  InferW ()
unifyRecToPartial (tfields, tname) ufields
  | not (Map.null uniqueTFields) =
    throwError $ show $
    PP.text "Incompatible record types:" <+>
    pPrint (FlatRecordType tfields (Just tname)) <+>
    PP.text " vs. " <+>
    pPrint (FlatRecordType ufields Nothing)
  | otherwise = varBind tname $ FlatRecordType.toType $ FlatRecordType uniqueUFields Nothing
  where
    uniqueTFields = tfields `Map.difference` ufields
    uniqueUFields = ufields `Map.difference` tfields

unifyRecPartials ::
  (Map String E.Type, E.TypeVar) -> (Map String E.Type, E.TypeVar) ->
  InferW ()
unifyRecPartials (tfields, tname) (ufields, uname) =
  do  restTv <- lift $ InferMonad.newTyVar "r"
      ((), s1) <-
        Writer.listen $ varBind tname $
        Map.foldWithKey E.TRecExtend restTv uniqueUFields
      varBind uname $ FreeTypeVars.applySubst s1 (Map.foldWithKey E.TRecExtend restTv uniqueTFields)
  where
    uniqueTFields = tfields `Map.difference` ufields
    uniqueUFields = ufields `Map.difference` tfields

unifyRecFulls ::
  Map String E.Type -> Map String E.Type -> InferW ()
unifyRecFulls tfields ufields
  | Map.keys tfields /= Map.keys ufields =
    throwError $ show $
    PP.text "Incompatible record types:" <+>
    pPrint (FlatRecordType tfields Nothing) <+>
    PP.text "vs." <+>
    pPrint (FlatRecordType ufields Nothing)
  | otherwise = return mempty

unifyRecs :: FlatRecordType -> FlatRecordType -> InferW ()
unifyRecs (FlatRecordType tfields tvar)
          (FlatRecordType ufields uvar) =
  do  let unifyField t u =
              do  old <- State.get
                  ((), s) <- lift $ Writer.listen $ mgu (FreeTypeVars.applySubst old t) (FreeTypeVars.applySubst old u)
                  State.put (old `mappend` s)
      (`evalStateT` mempty) . sequence_ . Map.elems $ Map.intersectionWith unifyField tfields ufields
      case (tvar, uvar) of
          (Nothing   , Nothing   ) -> unifyRecFulls tfields ufields
          (Just tname, Just uname) -> unifyRecPartials (tfields, tname) (ufields, uname)
          (Just tname, Nothing   ) -> unifyRecToPartial (tfields, tname) ufields
          (Nothing   , Just uname) -> unifyRecToPartial (ufields, uname) tfields

mgu :: E.Type -> E.Type -> InferW ()
mgu (E.TFun l r) (E.TFun l' r')  =
  do
    ((), s1) <- Writer.listen $ mgu l l'
    mgu
      (FreeTypeVars.applySubst s1 r)
      (FreeTypeVars.applySubst s1 r')
mgu (E.TApp l r) (E.TApp l' r')  =
  do
    ((), s1) <- Writer.listen $ mgu l l'
    mgu
      (FreeTypeVars.applySubst s1 r)
      (FreeTypeVars.applySubst s1 r')
mgu (E.TVar u) t               =  varBind u t
mgu t (E.TVar u)               =  varBind u t
mgu (E.TCon t) (E.TCon u)
  | t == u                   =  return mempty
mgu E.TRecEmpty E.TRecEmpty      =  return mempty
mgu t@E.TRecExtend {}
    u@E.TRecExtend {}          =  join $ either throwError return $
                                unifyRecs <$>
                                FlatRecordType.from t <*>
                                FlatRecordType.from u
mgu t1 t2                    =  throwError $ show $
                                PP.text "types do not unify: " <+> pPrint t1 <+>
                                PP.text "vs." <+> pPrint t2
typeInference :: Scope -> E.Val a -> Either String (E.Val (E.Type, a))
typeInference rootScope rootVal =
    InferMonad.run $
    do  ((_, t), s) <- InferMonad.runW $ infer (,) rootScope rootVal
        return (t & mapped . _1 %~ FreeTypeVars.applySubst s)

infer :: (E.Type -> a -> b) -> Scope -> E.Val a -> InferW (E.Type, E.Val b)
infer f scope expr@(E.Val pl body) = case body of
  E.VLeaf leaf ->
    mkResult (E.VLeaf leaf) <$>
    case leaf of
    E.VVar n ->
        case Scope.lookupTypeOf n scope of
           Nothing     -> throwError $ show $
                          PP.text "unbound variable:" <+> pPrint n
           Just sigma  -> lift (Scheme.instantiate sigma)
    E.VLit (E.LInt _) -> return (E.TCon "Int")
    E.VLit (E.LChar _) -> return (E.TCon "Char")
    E.VRecEmpty -> return E.TRecEmpty
  E.VAbs n e ->
    do  tv <- lift $ InferMonad.newTyVar "a"
        let scope' = Scope.insertTypeOf n (Scheme.specific tv) scope
        ((t1, e'), s1) <- Writer.listen $ infer f scope' e
        return $ mkResult (E.VAbs n e') $ E.TFun (FreeTypeVars.applySubst s1 tv) t1
  E.VApp e1 e2 ->
    do  tv <- lift $ InferMonad.newTyVar "a"
        ((t1, e1'), s1) <- Writer.listen $ infer f scope e1
        ((t2, e2'), s2) <- Writer.listen $ infer f (FreeTypeVars.applySubst s1 scope) e2
        ((), s3) <- Writer.listen $ mgu (FreeTypeVars.applySubst s2 t1) (E.TFun t2 tv)
        return $ mkResult (E.VApp e1' e2') $ FreeTypeVars.applySubst s3 tv
    `catchError`
    \e -> throwError $ e ++ "\n in " ++ show (pPrint (void expr))
  E.VLet x e1 e2 ->
    do  ((t1, e1'), s1) <- Writer.listen $ infer f scope e1
        -- TODO: (freeTypeVars (FreeTypeVars.applySubst s1 scope)) makes no sense performance-wise
        -- improve it
        let t' = Scheme.generalize (freeTypeVars (FreeTypeVars.applySubst s1 scope)) t1
            scope' = Scope.insertTypeOf x t' scope
        (t2, e2') <- infer f (FreeTypeVars.applySubst s1 scope') e2
        return $ mkResult (E.VLet x e1' e2') $ t2
  E.VGetField e name ->
    do  tv <- lift $ InferMonad.newTyVar "a"
        tvRec <- lift $ InferMonad.newTyVar "r"
        ((t, e'), s) <- Writer.listen $ infer f scope e
        ((), su) <- Writer.listen $ mgu (FreeTypeVars.applySubst s t) (E.TRecExtend name tv tvRec)
        return $ mkResult (E.VGetField e' name) $ FreeTypeVars.applySubst su tv
  E.VRecExtend name e1 e2 ->
    do  ((t1, e1'), s1) <- Writer.listen $ infer f scope e1
        (t2, e2') <- infer f (FreeTypeVars.applySubst s1 scope) e2
        return $ mkResult (E.VRecExtend name e1' e2') $ E.TRecExtend name t1 t2
  where
    mkResult body' typ = (typ, E.Val (f typ pl) body')

