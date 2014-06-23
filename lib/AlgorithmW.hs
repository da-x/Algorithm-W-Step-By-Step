module AlgorithmW
  ( module Expr
  , typeInference
  ) where

import Control.Applicative
import Control.Lens
import Control.Monad.Error
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Trans.Either
import Control.Monad.Writer hiding ((<>))
import Expr
import Pretty
import Record
import TVs
import qualified Data.Map as Map
import qualified Data.Set as Set
import Text.PrettyPrint ((<+>))
import qualified Text.PrettyPrint as PP

newtype TypeEnv = TypeEnv (Map.Map String Scheme)
instance TVs TypeEnv where
    ftv (TypeEnv env)      =  ftv (Map.elems env)
    apply s (TypeEnv env)  =  TypeEnv (Map.map (apply s) env)

generalize        ::  TypeEnv -> Type -> Scheme
generalize env t  =   Scheme vars t
  where vars = Set.toList $ ftv t `Set.difference` ftv env

data TIEnv = TIEnv  {}

data TIState = TIState { tiSupply :: Int }

type TI = EitherT String (ReaderT TIEnv (State TIState))
type TIW = WriterT Subst TI

runTI :: TI a -> Either String a
runTI t = evalState (runReaderT (runEitherT t) initTIEnv) initTIState
  where initTIEnv = TIEnv
        initTIState = TIState{tiSupply = 0}

newTyVarName :: String -> TI String
newTyVarName prefix =
    do  s <- get
        put s{tiSupply = tiSupply s + 1}
        return (prefix ++ show (tiSupply s))

newTyVar :: String -> TI Type
newTyVar prefix = TVar <$> newTyVarName prefix

instantiate :: Scheme -> TI Type
instantiate (Scheme vars t) = do  nvars <- mapM (\ _ -> newTyVar "a") vars
                                  let s = substFromList (zip vars nvars)
                                  return $ apply s t
varBind :: String -> Type -> TIW ()
varBind u (TVar t) | t == u          =  return ()
varBind u t  | u `Set.member` ftv t  =  throwError $ show $
                                        PP.text "occurs check fails:" <+>
                                        PP.text u <+> PP.text "vs." <+> prType t
             | otherwise             =  tell $ substFromList [(u, t)]

-- opposite of flatten
recToType :: FlatRecord -> Type
recToType (FlatRecord fields extension) =
  Map.foldWithKey TRecExtend (maybe TRecEmpty TVar extension) fields

unifyRecToPartial ::
  (Map.Map String Type, String) -> Map.Map String Type ->
  TIW ()
unifyRecToPartial (tfields, tname) ufields
  | not (Map.null uniqueTFields) =
    throwError $ show $
    PP.text "Incompatible record types:" <+>
    prFlatRecord (FlatRecord tfields (Just tname)) <+>
    PP.text " vs. " <+>
    prFlatRecord (FlatRecord ufields Nothing)
  | otherwise = varBind tname $ recToType $ FlatRecord uniqueUFields Nothing
  where
    uniqueTFields = tfields `Map.difference` ufields
    uniqueUFields = ufields `Map.difference` tfields

unifyRecPartials ::
  (Map.Map String Type, String) -> (Map.Map String Type, String) ->
  TIW ()
unifyRecPartials (tfields, tname) (ufields, uname) =
  do  restTv <- lift $ newTyVar "r"
      ((), s1) <- listen $ varBind tname $ Map.foldWithKey TRecExtend restTv uniqueUFields
      varBind uname $ apply s1 (Map.foldWithKey TRecExtend restTv uniqueTFields)
  where
    uniqueTFields = tfields `Map.difference` ufields
    uniqueUFields = ufields `Map.difference` tfields

unifyRecFulls ::
  Map.Map String Type -> Map.Map String Type -> TIW ()
unifyRecFulls tfields ufields
  | Map.keys tfields /= Map.keys ufields =
    throwError $ show $
    PP.text "Incompatible record types:" <+>
    prFlatRecord (FlatRecord tfields Nothing) <+>
    PP.text "vs." <+>
    prFlatRecord (FlatRecord ufields Nothing)
  | otherwise = return mempty

unifyRecs :: FlatRecord -> FlatRecord -> TIW ()
unifyRecs (FlatRecord tfields tvar)
          (FlatRecord ufields uvar) =
  do  let unifyField t u =
              do  old <- get
                  ((), s) <- lift $ listen $ mgu (apply old t) (apply old u)
                  put (old `mappend` s)
      (`evalStateT` mempty) . sequence_ . Map.elems $ Map.intersectionWith unifyField tfields ufields
      case (tvar, uvar) of
          (Nothing   , Nothing   ) -> unifyRecFulls tfields ufields
          (Just tname, Just uname) -> unifyRecPartials (tfields, tname) (ufields, uname)
          (Just tname, Nothing   ) -> unifyRecToPartial (tfields, tname) ufields
          (Nothing   , Just uname) -> unifyRecToPartial (ufields, uname) tfields

mgu :: Type -> Type -> TIW ()
mgu (TFun l r) (TFun l' r')  =  do  ((), s1) <- listen $ mgu l l'
                                    mgu (apply s1 r) (apply s1 r')
mgu (TApp l r) (TApp l' r')  =  do  ((), s1) <- listen $ mgu l l'
                                    mgu (apply s1 r) (apply s1 r')
mgu (TVar u) t               =  varBind u t
mgu t (TVar u)               =  varBind u t
mgu (TCon t) (TCon u)
  | t == u                   =  return mempty
mgu TRecEmpty TRecEmpty      =  return mempty
mgu t@TRecExtend {}
    u@TRecExtend {}          =  join $ either throwError return $ unifyRecs <$> flattenRec t <*> flattenRec u
mgu t1 t2                    =  throwError $ show $
                                PP.text "types do not unify: " <+> prType t1 <+>
                                PP.text "vs." <+> prType t2
typeInference :: Map.Map String Scheme -> Expr a -> Either String (Expr (Type, a))
typeInference rootEnv rootExpr =
    runTI $
    do  ((_, t), s) <- runWriterT $ ti (,) (TypeEnv rootEnv) rootExpr
        return (t & mapped . _1 %~ apply s)

envLookup :: String -> TypeEnv -> Maybe Scheme
envLookup key (TypeEnv env) = Map.lookup key env

envInsert :: String -> Scheme -> TypeEnv -> TypeEnv
envInsert key scheme (TypeEnv env) = TypeEnv (Map.insert key scheme env)

ti :: (Type -> a -> b) -> TypeEnv -> Expr a -> TIW (Type, Expr b)
ti f env expr@(Expr pl body) = case body of
  ELeaf leaf ->
    mkResult (ELeaf leaf) <$>
    case leaf of
    EVar n ->
        case envLookup n env of
           Nothing     -> throwError $ "unbound variable: " ++ n
           Just sigma  -> lift (instantiate sigma)
    ELit l -> tiLit l
    ERecEmpty -> return TRecEmpty
  EAbs n e ->
    do  tv <- lift $ newTyVar "a"
        let env' = envInsert n (Scheme [] tv) env
        ((t1, e'), s1) <- listen $ ti f env' e
        return $ mkResult (EAbs n e') $ TFun (apply s1 tv) t1
  EApp e1 e2 ->
    do  tv <- lift $ newTyVar "a"
        ((t1, e1'), s1) <- listen $ ti f env e1
        ((t2, e2'), s2) <- listen $ ti f (apply s1 env) e2
        ((), s3) <- listen $ mgu (apply s2 t1) (TFun t2 tv)
        return $ mkResult (EApp e1' e2') $ apply s3 tv
    `catchError`
    \e -> throwError $ e ++ "\n in " ++ show (prExp expr)
  ELet x e1 e2 ->
    do  ((t1, e1'), s1) <- listen $ ti f env e1
        let t' = generalize (apply s1 env) t1
            env' = envInsert x t' env
        (t2, e2') <- ti f (apply s1 env') e2
        return $ mkResult (ELet x e1' e2') $ t2
  EGetField e name ->
    do  tv <- lift $ newTyVar "a"
        tvRec <- lift $ newTyVar "r"
        ((t, e'), s) <- listen $ ti f env e
        ((), su) <- listen $ mgu (apply s t) (TRecExtend name tv tvRec)
        return $ mkResult (EGetField e' name) $ apply su tv
  ERecExtend name e1 e2 ->
    do  ((t1, e1'), s1) <- listen $ ti f env e1
        (t2, e2') <- ti f (apply s1 env) e2
        return $ mkResult (ERecExtend name e1' e2') $ TRecExtend name t1 t2
  where
    mkResult body' typ = (typ, Expr (f typ pl) body')


tiLit :: Lit -> TIW Type
tiLit (LInt _)   =  return (TCon "Int")
tiLit (LChar _)  =  return (TCon "Char")

