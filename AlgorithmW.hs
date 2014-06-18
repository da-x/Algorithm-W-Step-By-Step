{-# LANGUAGE StandaloneDeriving, DeriveFunctor, DeriveFoldable, DeriveTraversable, DeriveGeneric #-}
module AlgorithmW where

import Control.Applicative
import Control.DeepSeq
import Control.DeepSeq.Generics (genericRnf)
import Control.Lens
import Control.Monad.Error
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Trans.Either
import Control.Monad.Writer hiding ((<>))
import Data.Foldable (Foldable)
import Data.List
import GHC.Generics
import Text.PrettyPrint ((<+>), (<>))
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Text.PrettyPrint as PP

data Lit     =  LInt Integer
             |  LChar Char
  deriving (Eq, Ord, Generic)
instance NFData Lit where rnf = genericRnf

data Leaf  =  EVar String
           |  ELit Lit
           |  ERecEmpty
  deriving (Eq, Ord, Generic)
instance NFData Leaf where rnf = genericRnf

data Body exp  =  EApp exp exp
               |  EAbs String exp
               |  ELet String exp exp
               |  EGetField exp String
               |  ERecExtend String exp exp
               |  ELeaf Leaf
  deriving (Functor, Foldable, Traversable, Generic)
instance NFData exp => NFData (Body exp) where rnf = genericRnf

data Exp a = Exp
  { _expPayload :: a
  , expBody :: !(Body (Exp a))
  } deriving (Functor, Foldable, Traversable, Generic)
instance NFData a => NFData (Exp a) where rnf = genericRnf

expPayload :: Lens' (Exp a) a
expPayload f (Exp pl body) = (`Exp` body) <$> f pl

data Type    =  TVar String
             |  TFun Type Type
             |  TCon String
             |  TApp Type Type
             |  TRecExtend String Type Type
             |  TRecEmpty
  deriving (Eq, Ord, Generic)
instance NFData Type where rnf = genericRnf

data Scheme  =  Scheme [String] Type

class Types a where
    ftv    ::  a -> Set.Set String
    apply  ::  Subst -> a -> a

instance Types Type where
    ftv (TVar n)      =  Set.singleton n
    ftv (TCon _)      =  Set.empty
    ftv (TFun t1 t2)  =  ftv t1 `Set.union` ftv t2
    ftv (TApp t1 t2)  =  ftv t1 `Set.union` ftv t2
    ftv TRecEmpty     =  Set.empty
    ftv (TRecExtend _ t1 t2) = ftv t1 `Set.union` ftv t2

    apply s (TVar n)      =  case substLookup n s of
                               Nothing  -> TVar n
                               Just t   -> t
    apply s (TFun t1 t2)  = TFun (apply s t1) (apply s t2)
    apply s (TApp t1 t2)  = TApp (apply s t1) (apply s t2)
    apply _s (TCon t)     = TCon t
    apply _s TRecEmpty = TRecEmpty
    apply s (TRecExtend name typ rest) =
      TRecExtend name (apply s typ) $ apply s rest
instance Types Scheme where
    ftv (Scheme vars t)      =  (ftv t) `Set.difference` (Set.fromList vars)

    apply s (Scheme vars t)  =  Scheme vars (apply (foldr substDelete s vars) t)
instance Types a => Types [a] where
    apply s  =  map (apply s)
    ftv l    =  foldr Set.union Set.empty (map ftv l)

newtype Subst = Subst (Map.Map String Type)
instance Monoid Subst where
  mempty = Subst Map.empty
  mappend (Subst s1) (Subst s2) = Subst (s2 `Map.union` (Map.map (apply (Subst s2)) s1))

substLookup :: String -> Subst -> Maybe Type
substLookup name (Subst s) = Map.lookup name s

substDelete :: String -> Subst -> Subst
substDelete name (Subst s) = Subst (Map.delete name s)

newtype TypeEnv = TypeEnv (Map.Map String Scheme)
instance Types TypeEnv where
    ftv (TypeEnv env)      =  ftv (Map.elems env)
    apply s (TypeEnv env)  =  TypeEnv (Map.map (apply s) env)

generalize        ::  TypeEnv -> Type -> Scheme
generalize env t  =   Scheme vars t
  where vars = Set.toList ((ftv t) `Set.difference` (ftv env))

data TIEnv = TIEnv  {}

data TIState = TIState { tiSupply :: Int }

type TI m = EitherT String (ReaderT TIEnv (StateT TIState m))
type TIW m = WriterT Subst (TI m)

runTI :: TI IO a -> IO (Either String a)
runTI t = evalStateT (runReaderT (runEitherT t) initTIEnv) initTIState
  where initTIEnv = TIEnv
        initTIState = TIState{tiSupply = 0}

newTyVarName :: Monad m => String -> TI m String
newTyVarName prefix =
    do  s <- get
        put s{tiSupply = tiSupply s + 1}
        return (prefix ++ show (tiSupply s))

newTyVar :: Monad m => String -> TI m Type
newTyVar prefix = TVar <$> newTyVarName prefix

instantiate :: Monad m => Scheme -> TI m Type
instantiate (Scheme vars t) = do  nvars <- mapM (\ _ -> newTyVar "a") vars
                                  let s = Subst (Map.fromList (zip vars nvars))
                                  return $ apply s t
varBind :: Monad m => String -> Type -> TIW m ()
varBind u t  | t == TVar u           =  return ()
             | u `Set.member` ftv t  =  throwError $ "occurs check fails: " ++ u ++
                                         " vs. " ++ show t
             | otherwise             =  tell $ Subst $ Map.singleton u t

data FlatRecord = FlatRecord
  { _frFields :: Map.Map String Type
  , _frExtension :: Maybe String -- TyVar of more possible fields
  }

frFields :: Lens' FlatRecord (Map.Map String Type)
frFields f (FlatRecord fields ext) = (`FlatRecord` ext) <$> f fields

-- From a record type to a sorted list of fields
flattenRec :: Type -> Either String FlatRecord
flattenRec (TRecExtend name typ rest) =
  flattenRec rest
  <&> frFields %~ Map.insert name typ
flattenRec TRecEmpty = return $ FlatRecord Map.empty Nothing
flattenRec (TVar name) = return $ FlatRecord Map.empty (Just name)
flattenRec t = Left $ "TRecExtend on non-record: " ++ show t

-- opposite of flatten
recToType :: FlatRecord -> Type
recToType (FlatRecord fields extension) =
  Map.foldWithKey TRecExtend (maybe TRecEmpty TVar extension) fields

unifyRecToPartial ::
  Monad m => (Map.Map String Type, String) -> Map.Map String Type ->
  TIW m ()
unifyRecToPartial (tfields, tname) ufields
  | not (Map.null uniqueTFields) =
    throwError $ "Incompatible record types: " ++
    show (FlatRecord tfields (Just tname)) ++ " vs. " ++ show (FlatRecord ufields Nothing)
  | otherwise = varBind tname $ recToType $ FlatRecord uniqueUFields Nothing
  where
    uniqueTFields = tfields `Map.difference` ufields
    uniqueUFields = ufields `Map.difference` tfields

unifyRecPartials ::
  Monad m => (Map.Map String Type, String) -> (Map.Map String Type, String) ->
  TIW m ()
unifyRecPartials (tfields, tname) (ufields, uname) =
  do  restTv <- lift $ newTyVar "r"
      ((), s1) <- listen $ varBind tname $ Map.foldWithKey TRecExtend restTv uniqueUFields
      varBind uname $ apply s1 (Map.foldWithKey TRecExtend restTv uniqueTFields)
  where
    uniqueTFields = tfields `Map.difference` ufields
    uniqueUFields = ufields `Map.difference` tfields

unifyRecFulls ::
  Monad m => Map.Map String Type -> Map.Map String Type -> TIW m ()
unifyRecFulls tfields ufields
  | Map.keys tfields /= Map.keys ufields =
    throwError $
    "Incompatible record types: " ++
    show (FlatRecord tfields Nothing) ++ " vs. " ++
    show (FlatRecord ufields Nothing)
  | otherwise = return mempty

unifyRecs :: Monad m => FlatRecord -> FlatRecord -> TIW m ()
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

mgu :: Monad m => Type -> Type -> TIW m ()
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
mgu t1 t2                    =  throwError $ "types do not unify: " ++ show t1 ++
                                " vs. " ++ show t2
typeInference :: Monad m => Map.Map String Scheme -> Exp a -> TI m (Exp (Type, a))
typeInference rootEnv rootExpr =
    do  ((_, t), s) <- runWriterT $ ti (,) (TypeEnv rootEnv) rootExpr
        return (t & mapped . _1 %~ apply s)

envLookup :: String -> TypeEnv -> Maybe Scheme
envLookup key (TypeEnv env) = Map.lookup key env

envInsert :: String -> Scheme -> TypeEnv -> TypeEnv
envInsert key scheme (TypeEnv env) = TypeEnv (Map.insert key scheme env)

ti :: Monad m => (Type -> a -> b) -> TypeEnv -> Exp a -> TIW m (Type, Exp b)
ti f env expr@(Exp pl body) = case body of
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
    \e -> throwError $ e ++ "\n in " ++ show expr
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
    mkResult body' typ = (typ, Exp (f typ pl) body')


tiLit :: Monad m => Lit -> TIW m Type
tiLit (LInt _)   =  return (TCon "Int")
tiLit (LChar _)  =  return (TCon "Char")

eLet :: String -> Exp () -> Exp () -> Exp ()
eLet name e1 e2 = Exp () $ ELet name e1 e2

eAbs :: String -> Exp () -> Exp ()
eAbs name body = Exp () $ EAbs name body

eVar :: String -> Exp ()
eVar = Exp () . ELeaf . EVar

eLit :: Lit -> Exp ()
eLit = Exp () . ELeaf . ELit

eRecEmpty :: Exp ()
eRecEmpty = Exp () $ ELeaf ERecEmpty

eApp :: Exp () -> Exp () -> Exp ()
eApp f x = Exp () $ EApp f x

eRecExtend :: String -> Exp () -> Exp () -> Exp ()
eRecExtend name typ rest = Exp () $ ERecExtend name typ rest

eGetField :: Exp () -> String -> Exp ()
eGetField r n = Exp () $ EGetField r n

exp0 :: Exp ()
exp0  =  eLet "id" (eAbs "x" (eVar "x"))
          (eVar "id")

exp1 :: Exp ()
exp1  =  eLet "id" (eAbs "x" (eVar "x"))
          (eApp (eVar "id") (eVar "id"))

exp2 :: Exp ()
exp2  =  eLet "id" (eAbs "x" (eLet "y" (eVar "x") (eVar "y")))
          (eApp (eVar "id") (eVar "id"))

exp3 :: Exp ()
exp3  =  eLet "id" (eAbs "x" (eLet "y" (eVar "x") (eVar "y")))
          (eApp (eApp (eVar "id") (eVar "id")) (eLit (LInt 2)))

exp4 :: Exp ()
exp4  =  eLet "id" (eAbs "x" (eApp (eVar "x") (eVar "x")))
          (eVar "id")

exp5 :: Exp ()
exp5  =  eAbs "m" (eLet "y" (eVar "m")
                   (eLet "x" (eApp (eVar "y") (eLit (LChar 'x')))
                         (eVar "x")))

exp6 :: Exp ()
exp6  =  eApp (eLit (LInt 2)) (eLit (LInt 2))

exp7 :: Exp ()
exp7  =  eAbs "vec" $
         eRecExtend "newX" (eGetField (eVar "vec") "x") $
         eRecExtend "newY" (eGetField (eVar "vec") "y") $
         eRecEmpty

exp8 :: Exp ()
exp8  =  eLet
         "vec" ( eRecExtend "x" (eLit (LInt 5)) $
                 eRecExtend "y" (eLit (LInt 7)) $
                 eRecEmpty ) $
         eGetField (eVar "vec") "x"

exp9 :: Exp ()
exp9  =  eLet
         "vec" ( eRecExtend "x" (eLit (LInt 5)) $
                 eRecExtend "y" (eLit (LInt 7)) $
                 eRecEmpty ) $
         eGetField (eVar "vec") "z"

test :: Exp a -> IO ()
test e =
    do  res <- runTI (typeInference Map.empty e)
        case res of
          Left err               ->  putStrLn $ show e ++ "\n " ++ err ++ "\n"
          Right (Exp (t, _) _)   ->  putStrLn $ show e ++ " :: " ++ show t ++ "\n"

main :: IO ()
main = mapM_ test [exp0, exp1, exp2, exp3, exp4, exp5, exp6, exp7, exp8, exp9]

instance Show Type where
    showsPrec _ x = shows (prType x)

instance Show FlatRecord where
    showsPrec _ r = shows $ prFlatRecord r

prFlatRecord :: FlatRecord -> PP.Doc
prFlatRecord (FlatRecord fields varName) =
    PP.text "T{" <+>
      mconcat (intersperse (PP.text ", ") (map prField (Map.toList fields))) <>
      moreFields <+>
    PP.text "}"
    where
      prField (name, typ) = PP.text name <+> PP.text ":" <+> prType typ
      moreFields =
        case varName of
        Nothing -> PP.empty
        Just name -> PP.comma <+> PP.text name <> PP.text "..."

prType             ::  Type -> PP.Doc
prType (TVar n)    =   PP.text n
prType (TCon s)    =   PP.text s
prType (TFun t s)  =   prParenType t <+> PP.text "->" <+> prType s
prType (TApp t s)  =   prParenType t <+> prType s
prType r@(TRecExtend name typ rest) = case flattenRec r of
  Left _ -> -- Fall back to nested record presentation:
    PP.text "T{" <+>
      PP.text name <+> PP.text ":" <+> prType typ <+>
      PP.text "**" <+> prType rest <+>
    PP.text "}"
  Right flatRecord -> prFlatRecord flatRecord
prType TRecEmpty   =   PP.text "T{}"

prParenType     ::  Type -> PP.Doc
prParenType  t  =   case t of
                      TFun _ _  -> PP.parens (prType t)
                      _         -> prType t

instance Show (Exp a) where
    showsPrec _ x = shows (prExp x)

flattenERec :: Exp a -> (Map.Map String (Exp a), Maybe (Exp a))
flattenERec (Exp _ (ERecExtend name val body)) =
  flattenERec body
  & _1 %~ Map.insert name val
flattenERec (Exp _ (ELeaf ERecEmpty)) = (Map.empty, Nothing)
flattenERec other = (Map.empty, Just other)

prExp                  ::  Exp a -> PP.Doc
prExp expr =
    case expBody expr of
    ELeaf (EVar name) ->   PP.text name
    ELeaf (ELit lit)  ->   prLit lit
    ELet x b body     ->   PP.text "let" <+>
                           PP.text x <+> PP.text "=" <+>
                           prExp b <+> PP.text "in" PP.$$
                           PP.nest 2 (prExp body)
    EApp e1 e2        ->   prExp e1 <+> prParenExp e2
    EAbs n e          ->   PP.char '\\' <> PP.text n <+>
                           PP.text "->" <+>
                           prExp e
    EGetField e n     ->   prParenExp e <> PP.char '.' <> PP.text n
    ELeaf ERecEmpty   ->   PP.text "{}"
    ERecExtend {}     ->
        PP.text "V{" <+>
            mconcat (intersperse (PP.text ", ") (map prField (Map.toList fields))) <>
            moreFields <+>
        PP.text "}"
      where
        prField (name, val) = PP.text name <+> PP.text "=" <+> prExp val
        moreFields =
          case mRest of
          Nothing -> PP.empty
          Just rest -> PP.comma <+> PP.text "{" <+> prExp rest <+> PP.text "}"
        (fields, mRest) = flattenERec expr

prParenExp    ::  Exp a -> PP.Doc
prParenExp t  =   case expBody t of
                    ELet _ _ _  -> PP.parens (prExp t)
                    EApp _ _    -> PP.parens (prExp t)
                    EAbs _ _    -> PP.parens (prExp t)
                    _           -> prExp t

instance Show Lit where
    showsPrec _ x = shows (prLit x)

prLit            ::  Lit -> PP.Doc
prLit (LInt i)   =   PP.integer i
prLit (LChar c)  =   PP.text (show c)

instance Show Scheme where
    showsPrec _ x = shows (prScheme x)

prScheme                  ::  Scheme -> PP.Doc
prScheme (Scheme vars t)  =   PP.text "All" <+>
                              PP.hcat
                                (PP.punctuate PP.comma (map PP.text vars))
                              <> PP.text "." <+> prType t
