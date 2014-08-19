{-# LANGUAGE DeriveFunctor, DeriveFoldable, DeriveTraversable, DeriveGeneric, OverloadedStrings #-}
module Lamdu.Infer
  ( makeScheme
  , TypeVars(..)
  , infer
  , Scope, emptyScope
  , Payload(..), plScope, plType, plData
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
import Lamdu.Expr.Type (Type)
import Lamdu.Expr.TypeVars (TypeVars(..))
import Lamdu.Expr.Val (Val(..))
import Lamdu.Infer.Internal.Monad (Infer(..))
import Lamdu.Infer.Internal.Scheme (makeScheme)
import Lamdu.Infer.Internal.Scope (Scope, emptyScope)
import Lamdu.Infer.Internal.Subst (CanSubst(..))
import Lamdu.Infer.Unify (unify)
import qualified Data.Map as Map
import qualified Lamdu.Expr.Type as T
import qualified Lamdu.Expr.TypeVars as TypeVars
import qualified Lamdu.Expr.Val as V
import qualified Lamdu.Infer.Error as Err
import qualified Lamdu.Infer.Internal.Monad as M
import qualified Lamdu.Infer.Internal.Scheme as Scheme
import qualified Lamdu.Infer.Internal.Scope as Scope
import qualified Lamdu.Infer.Internal.Subst as Subst

data Payload a = Payload
  { _plType :: Type
  , _plScope :: Scope
  , _plData :: a
  } deriving (Generic, Show, Functor, Foldable, Traversable)
instance NFData a => NFData (Payload a) where rnf = genericRnf

plType :: Lens' (Payload a) Type
plType f pl = (\t' -> pl { _plType = t' }) <$> f (_plType pl)
{-# INLINE plType #-}

plScope :: Lens' (Payload a) Scope
plScope f pl = (\t' -> pl { _plScope = t' }) <$> f (_plScope pl)
{-# INLINE plScope #-}

plData :: Lens' (Payload a) a
plData f pl = (\t' -> pl { _plData = t' }) <$> f (_plData pl)
{-# INLINE plData #-}

instance CanSubst (Payload a) where
  freeVars (Payload typ scope _dat) =
    Subst.freeVars typ <> Subst.freeVars scope
  apply s (Payload typ scope dat) =
    Payload (Subst.apply s typ) (Subst.apply s scope) dat

inferSubst ::
  Map V.GlobalId Scheme -> Scope -> Val a -> Infer (Scope, Val (Payload a))
inferSubst globals scope val =
  do  prevSubst <- M.getSubst
      let scope' = Subst.apply prevSubst scope
      (inferredVal, newResults) <- M.listen $ inferInternal Payload globals scope' val
      return (scope', Subst.apply (M.subst newResults) <$> inferredVal)

-- All accessed global IDs are supposed to be extracted from the
-- expression to build this global scope. This is slightly hacky but
-- much faster than a polymorphic monad underlying the Infer monad
-- allowing global access.
-- Use loadInfer for a safer interface
infer ::
  Map V.GlobalId Scheme -> Scope -> Val a -> Infer (Val (Payload a))
infer globals scope val =
  do  ((scope', val'), results) <- M.listenNoTell $ inferSubst globals scope val
      M.tell $ results
        { M.subst = Subst.intersect (Subst.freeVars scope') $ M.subst results }
      return val'

data CompositeHasTag p = HasTag | DoesNotHaveTag | MayHaveTag (T.Var (T.Composite p))

hasTag :: T.Tag -> T.Composite p -> CompositeHasTag p
hasTag _ T.CEmpty   = DoesNotHaveTag
hasTag _ (T.CVar v) = MayHaveTag v
hasTag tag (T.CExtend t _ r)
  | tag == t  = HasTag
  | otherwise = hasTag tag r

inferInternal ::
  (Type -> Scope -> a -> b) ->
  Map V.GlobalId Scheme -> Scope -> Val a ->
  Infer (Val b)
inferInternal f globals =
  (fmap . fmap) snd . go
  where
    go locals (Val pl body) =
      case body of
      V.BLeaf leaf ->
        mkResult (V.BLeaf leaf) <$>
        case leaf of
        V.LHole -> M.newInferredVar "h"
        V.LVar n ->
            case Scope.lookupTypeOf n locals of
               Nothing      -> M.throwError $ Err.UnboundVariable n
               Just t       -> return t
        V.LGlobal n ->
            case Map.lookup n globals of
               Nothing      -> M.throwError $ Err.MissingGlobal n
               Just sigma   -> Scheme.instantiate sigma
        V.LLiteralInteger _ -> return (T.TInst "Int" mempty)
        V.LRecEmpty -> return $ T.TRecord T.CEmpty
      V.BAbs (V.Lam n paramTypeTemplate e) ->
        do  tv <- Scheme.instantiate paramTypeTemplate
            let locals' = Scope.insertTypeOf n tv locals
            ((t1, e'), s1) <- M.listenSubst $ go locals' e
            return $ mkResult (V.BAbs (V.Lam n paramTypeTemplate e')) $ T.TFun (Subst.apply s1 tv) t1
      V.BApp (V.Apply e1 e2) ->
        do  tv <- M.newInferredVar "a"
            ((t1, e1'), s1) <- M.listenSubst $ go locals e1
            ((t2, e2'), s2) <- M.listenSubst $ go (Subst.apply s1 locals) e2
            ((), s3) <- M.listenSubst $ unify (Subst.apply s2 t1) (T.TFun t2 tv)
            return $ mkResult (V.BApp (V.Apply e1' e2')) $ Subst.apply s3 tv
      V.BGetField (V.GetField e name) ->
        do  tv <- M.newInferredVar "a"
            tvRecName <- M.newInferredVarName "r"
            M.tellConstraint tvRecName name
            ((t, e'), s) <- M.listenSubst $ go locals e
            ((), su) <-
              M.listenSubst $ unify (Subst.apply s t) $
              T.TRecord $ T.CExtend name tv $ TypeVars.liftVar tvRecName
            return $ mkResult (V.BGetField (V.GetField e' name)) $ Subst.apply su tv
      V.BRecExtend (V.RecExtend name e1 e2) ->
        do  ((t1, e1'), s1) <- M.listenSubst $ go locals e1
            ((t2, e2'), _) <- M.listenSubst $ go (Subst.apply s1 locals) e2
            rest <-
              case t2 of
              T.TRecord x ->
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
                ((), s) <- M.listenSubst $ unify t2 $ T.TRecord tve
                return $ Subst.apply s tve
            return $ mkResult (V.BRecExtend (V.RecExtend name e1' e2')) $
              T.TRecord $ T.CExtend name t1 rest
      where
        mkResult body' typ = (typ, Val (f typ locals pl) body')
