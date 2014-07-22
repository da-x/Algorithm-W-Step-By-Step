{-# LANGUAGE DeriveFunctor, DeriveFoldable, DeriveTraversable, DeriveGeneric, OverloadedStrings #-}
module Lamdu.Infer
  ( Constraints(..), Scheme(..), TypeVars(..), typeInference
  , Payload(..), plType
  , pPrintPureVal, pPrintValUnannotated
  ) where

import Control.Applicative ((<$), (<$>))
import Control.DeepSeq (NFData(..))
import Control.DeepSeq.Generics (genericRnf)
import Control.Lens (Lens')
import Control.Lens.Operators
import Control.Monad.Except (catchError, throwError)
import Data.Foldable (Foldable)
import Data.Map (Map)
import Data.Monoid (Monoid(..), (<>))
import Data.Traversable (Traversable)
import GHC.Generics (Generic)
import Lamdu.Infer.Internal.Constraints (Constraints(..))
import Lamdu.Infer.Internal.Monad (Infer)
import Lamdu.Infer.Internal.Scheme (Scheme)
import Lamdu.Infer.Internal.Scope (Scope)
import Lamdu.Infer.Internal.TypeVars (TypeVars(..), HasVar(..))
import Lamdu.Infer.Internal.Unify (unify)
import Lamdu.Pretty (pPrintPureVal, pPrintValUnannotated)
import Text.PrettyPrint ((<+>))
import Text.PrettyPrint.HughesPJClass (Pretty(..))
import qualified Data.Map as Map
import qualified Lamdu.Expr as E
import qualified Lamdu.Infer.Internal.Monad as M
import qualified Lamdu.Infer.Internal.Scheme as Scheme
import qualified Lamdu.Infer.Internal.Scope as Scope
import qualified Lamdu.Infer.Internal.TypeVars as TypeVars
import qualified Text.PrettyPrint as PP

data Payload a = Payload
  { _plType :: E.Type
  , _plScope :: Scope
  , _plData :: a
  } deriving (Generic, Show, Functor, Foldable, Traversable)
instance NFData a => NFData (Payload a) where rnf = genericRnf

plType :: Lens' (Payload a) E.Type
plType f pl = (\t' -> pl { _plType = t' }) <$> f (_plType pl)
{-# INLINE plType #-}

instance TypeVars.FreeTypeVars (Payload a) where
  freeTypeVars (Payload typ scope _dat) =
    TypeVars.freeTypeVars typ <> TypeVars.freeTypeVars scope
  applySubst s (Payload typ scope dat) =
    Payload (TypeVars.applySubst s typ) (TypeVars.applySubst s scope) dat

typeInference :: Map E.GlobalId Scheme -> E.Val a -> Either String (Scheme, E.Val (Payload a))
typeInference globals rootVal =
  do  ((_, topScheme, val), s) <-
        M.run M.emptyContext $ Scheme.generalize Scope.empty $ infer Payload globals Scope.empty rootVal
      return (topScheme, val <&> TypeVars.applySubst (M.subst (M.ctxResults s)))

data CompositeHasTag p = HasTag | DoesNotHaveTag | MayHaveTag (E.TypeVar (E.CompositeType p))

hasTag :: E.Tag -> E.CompositeType p -> CompositeHasTag p
hasTag _ E.CEmpty   = DoesNotHaveTag
hasTag _ (E.CVar v) = MayHaveTag v
hasTag tag (E.CExtend t _ r)
  | tag == t  = HasTag
  | otherwise = hasTag tag r

infer ::
  (E.Type -> Scope -> a -> b) ->
  Map E.GlobalId Scheme -> Scope -> E.Val a ->
  Infer (E.Type, E.Val b)
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
            case Map.lookup n globals of
               Nothing      -> throwError $ show $
                               PP.text "missing global:" <+> pPrint n
               Just sigma   -> Scheme.instantiate sigma
        E.VLiteralInteger _ -> return (E.TInst "Int" mempty)
        E.VRecEmpty -> return $ E.TRecord E.CEmpty
      E.VAbs (E.Lam n e) ->
        do  tv <- M.newInferredVar "a"
            let locals' = Scope.insertTypeOf n tv locals
            ((t1, e'), s1) <- M.listenSubst $ go locals' e
            return $ mkResult (E.VAbs (E.Lam n e')) $ E.TFun (TypeVars.applySubst s1 tv) t1
      E.VApp (E.Apply e1 e2) ->
        do  tv <- M.newInferredVar "a"
            ((t1, e1'), s1) <- M.listenSubst $ go locals e1
            ((t2, e2'), s2) <- M.listenSubst $ go (TypeVars.applySubst s1 locals) e2
            ((), s3) <- M.listenSubst $ unify (TypeVars.applySubst s2 t1) (E.TFun t2 tv)
            return $ mkResult (E.VApp (E.Apply e1' e2')) $ TypeVars.applySubst s3 tv
        `catchError`
        \e -> throwError $ e ++ "\n in " ++ show (pPrintValUnannotated expr)
      E.VGetField (E.GetField e name) ->
        do  tv <- M.newInferredVar "a"
            tvRecName <- M.newInferredVarName "r"
            M.tellConstraint tvRecName name
            ((t, e'), s) <- M.listenSubst $ go locals e
            ((), su) <-
              M.listenSubst $ unify (TypeVars.applySubst s t) $
              E.TRecord $ E.CExtend name tv $ liftVar tvRecName
            return $ mkResult (E.VGetField (E.GetField e' name)) $ TypeVars.applySubst su tv
      E.VRecExtend (E.RecExtend name e1 e2) ->
        do  ((t1, e1'), s1) <- M.listenSubst $ go locals e1
            ((t2, e2'), _) <- M.listenSubst $ go (TypeVars.applySubst s1 locals) e2
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
                ((), s) <- M.listenSubst $ unify t2 $ E.TRecord tve
                return $ TypeVars.applySubst s tve
            return $ mkResult (E.VRecExtend (E.RecExtend name e1' e2')) $
              E.TRecord $ E.CExtend name t1 rest
      where
        mkResult body' typ = (typ, E.Val (f typ locals pl) body')
