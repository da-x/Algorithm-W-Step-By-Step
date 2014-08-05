{-# LANGUAGE DeriveFunctor, DeriveFoldable, DeriveTraversable, DeriveGeneric, OverloadedStrings #-}
module Lamdu.Infer
  ( makeScheme
  , TypeVars(..)
  , infer
  , Scope, emptyScope
  , Payload(..), plScope, plType
  , M.Context, M.initialContext
  , Infer(..)
  ) where

import Control.Applicative ((<$), (<$>))
import Control.DeepSeq (NFData(..))
import Control.DeepSeq.Generics (genericRnf)
import Control.Lens (Lens')
import Data.Foldable (Foldable)
import Data.Map (Map)
import Data.Monoid (Monoid(..), (<>))
import Data.Traversable (Traversable)
import GHC.Generics (Generic)
import Lamdu.Expr.Scheme (Scheme)
import Lamdu.Expr.TypeVars (TypeVars(..))
import Lamdu.Infer.Internal.Monad (Infer(..))
import Lamdu.Infer.Internal.Scheme (makeScheme)
import Lamdu.Infer.Internal.Scope (Scope, emptyScope)
import Lamdu.Infer.Internal.Subst (CanSubst(..))
import Lamdu.Infer.Unify (unify)
import qualified Data.Map as Map
import qualified Lamdu.Expr as E
import qualified Lamdu.Expr.TypeVars as TypeVars
import qualified Lamdu.Infer.Error as Err
import qualified Lamdu.Infer.Internal.Monad as M
import qualified Lamdu.Infer.Internal.Scheme as Scheme
import qualified Lamdu.Infer.Internal.Scope as Scope
import qualified Lamdu.Infer.Internal.Subst as Subst

data Payload a = Payload
  { _plType :: E.Type
  , _plScope :: Scope
  , _plData :: a
  } deriving (Generic, Show, Functor, Foldable, Traversable)
instance NFData a => NFData (Payload a) where rnf = genericRnf

plType :: Lens' (Payload a) E.Type
plType f pl = (\t' -> pl { _plType = t' }) <$> f (_plType pl)
{-# INLINE plType #-}

plScope :: Lens' (Payload a) Scope
plScope f pl = (\t' -> pl { _plScope = t' }) <$> f (_plScope pl)
{-# INLINE plScope #-}

instance CanSubst (Payload a) where
  freeVars (Payload typ scope _dat) =
    Subst.freeVars typ <> Subst.freeVars scope
  apply s (Payload typ scope dat) =
    Payload (Subst.apply s typ) (Subst.apply s scope) dat

censorInfer :: TypeVars -> Infer a -> Infer a
censorInfer typeVars act =
  do  (val, results) <- M.listenNoTell act
      M.tell $ M.intersectResults typeVars results
      return val

inferSubst ::
  Map E.GlobalId Scheme -> Scope -> E.Val a -> Infer (E.Val (Payload a))
inferSubst globals scope val =
  do  prevSubst <- M.getSubst
      (inferredVal, newResults) <- M.listen $ inferInternal Payload globals (Subst.apply prevSubst scope) val
      return $ Subst.apply (M.subst newResults) <$> inferredVal

-- All accessed global IDs are supposed to be extracted from the
-- expression to build this global scope. This is slightly hacky but
-- much faster than a polymorphic monad underlying the Infer monad
-- allowing global access.
infer ::
  Map E.GlobalId Scheme -> Scope -> E.Val a -> Infer (E.Val (Payload a))
infer globals scope =
  censorInfer (Subst.freeVars scope) . inferSubst globals scope

data CompositeHasTag p = HasTag | DoesNotHaveTag | MayHaveTag (E.TypeVar (E.CompositeType p))

hasTag :: E.Tag -> E.CompositeType p -> CompositeHasTag p
hasTag _ E.CEmpty   = DoesNotHaveTag
hasTag _ (E.CVar v) = MayHaveTag v
hasTag tag (E.CExtend t _ r)
  | tag == t  = HasTag
  | otherwise = hasTag tag r

inferInternal ::
  (E.Type -> Scope -> a -> b) ->
  Map E.GlobalId Scheme -> Scope -> E.Val a ->
  Infer (E.Val b)
inferInternal f globals =
  (fmap . fmap) snd . go
  where
    go locals (E.Val pl body) =
      case body of
      E.VLeaf leaf ->
        mkResult (E.VLeaf leaf) <$>
        case leaf of
        E.VHole -> M.newInferredVar "h"
        E.VVar n ->
            case Scope.lookupTypeOf n locals of
               Nothing      -> M.throwError $ Err.UnboundVariable n
               Just t       -> return t
        E.VGlobal n ->
            case Map.lookup n globals of
               Nothing      -> M.throwError $ Err.MissingGlobal n
               Just sigma   -> Scheme.instantiate sigma
        E.VLiteralInteger _ -> return (E.TInst "Int" mempty)
        E.VRecEmpty -> return $ E.TRecord E.CEmpty
      E.VAbs (E.Lam n e) ->
        do  tv <- M.newInferredVar "a"
            let locals' = Scope.insertTypeOf n tv locals
            ((t1, e'), s1) <- M.listenSubst $ go locals' e
            return $ mkResult (E.VAbs (E.Lam n e')) $ E.TFun (Subst.apply s1 tv) t1
      E.VApp (E.Apply e1 e2) ->
        do  tv <- M.newInferredVar "a"
            ((t1, e1'), s1) <- M.listenSubst $ go locals e1
            ((t2, e2'), s2) <- M.listenSubst $ go (Subst.apply s1 locals) e2
            ((), s3) <- M.listenSubst $ unify (Subst.apply s2 t1) (E.TFun t2 tv)
            return $ mkResult (E.VApp (E.Apply e1' e2')) $ Subst.apply s3 tv
      E.VGetField (E.GetField e name) ->
        do  tv <- M.newInferredVar "a"
            tvRecName <- M.newInferredVarName "r"
            M.tellConstraint tvRecName name
            ((t, e'), s) <- M.listenSubst $ go locals e
            ((), su) <-
              M.listenSubst $ unify (Subst.apply s t) $
              E.TRecord $ E.CExtend name tv $ TypeVars.liftVar tvRecName
            return $ mkResult (E.VGetField (E.GetField e' name)) $ Subst.apply su tv
      E.VRecExtend (E.RecExtend name e1 e2) ->
        do  ((t1, e1'), s1) <- M.listenSubst $ go locals e1
            ((t2, e2'), _) <- M.listenSubst $ go (Subst.apply s1 locals) e2
            rest <-
              case t2 of
              E.TRecord x ->
                -- In case t2 is already inferred as a TRecord,
                -- verify it doesn't already have this field,
                -- and avoid unnecessary unify from other case
                case hasTag name x of
                HasTag -> M.throwError $ Err.FieldAlreadyInRecord name x
                DoesNotHaveTag -> return x
                MayHaveTag var -> x <$ M.tellConstraint var name
              _ -> do
                tv <- M.newInferredVarName "r"
                M.tellConstraint tv name
                let tve = TypeVars.liftVar tv
                ((), s) <- M.listenSubst $ unify t2 $ E.TRecord tve
                return $ Subst.apply s tve
            return $ mkResult (E.VRecExtend (E.RecExtend name e1' e2')) $
              E.TRecord $ E.CExtend name t1 rest
      where
        mkResult body' typ = (typ, E.Val (f typ locals pl) body')
