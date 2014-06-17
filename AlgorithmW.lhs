\documentclass[a4paper,11pt]{article}

\usepackage[margin=2.5cm]{geometry}
\usepackage{hyperref}

%include polycode.fmt
%format alpha = "\alpha"
%format Set.empty = "\emptyset"
%format `Set.union` = "\cup"
%format `Set.difference` = "~\backslash~"
%format Set.singleton n = "\{" n "\}"
%format <+> = "\left<+\right>"

\title{\bf Algorithm W Step by Step}
\author{Martin Grabm{\"u}ller}
\date{}

\begin{document}
\maketitle

\begin{abstract}\noindent
In this paper we develop a complete implementation of the classic
algorithm W for Hindley-Milner polymorphic type inference in Haskell.
\end{abstract}

\section{Introduction}

Type inference is a tricky business, and it is even harder to learn
the basics, because most publications are about very advanced topics
like rank-N polymorphism, predicative/impredicative type systems,
universal and existential types and so on.  Since I learn best by
actually developing the solution to a problem, I decided to write a
basic tutorial on type inference, implementing one of the most basic
type inference algorithms which has nevertheless practical uses as the
basis of the type checkers of languages like ML or Haskell.

The type inference algorithm studied here is the classic Algoritm W
proposed by Milner \cite{Milner1978Theory}.  For a very readable
presentation of this algorithm and possible variations and extensions
read also \cite{Heeren2002GeneralizingHM}.  Several aspects of this
tutorial are also inspired by \cite{Jones1999THiH}.
\footnote{Copied from
  \url{http://www.grabmueller.de/martin/www/pub/AlgorithmW.en.html} and edited by
  Wei Hu.  Unfortunately the bibliography is missing.
}
\footnote{The most helpful references are
  \url{http://www.cs.uu.nl/research/techreps/repo/CS-2002/2002-031.pdf}
\emph{Generalizing Hindley-Milner Type Inference Algorithms}, and
Chapter 22 of \emph{TAPL}.}
\footnote{A Cornell course touchs on this topic and gives an OCaml implementation.
            \url{http://www.cs.cornell.edu/Courses/cs3110/2009fa/lectures/lec26a.htm}}

\section{Algorithm W}

We start with the necessary imports.  For representing environments
(also called contexts in the literature) and substitutions, we import
module |Data.Map|.  Sets of type variables etc. will be represented as
sets from module |Data.Set|.

\begin{code}
{-# LANGUAGE StandaloneDeriving #-}
module AlgorithmW where

import qualified Data.Map as Map
import qualified Data.Set as Set
\end{code}

Since we will also make use of various monad transformers, several
modules from the monad template library are imported as well.
\begin{code}
import Control.Applicative
import Control.Lens
import Control.Monad.Error
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Trans.Either
import Control.Monad.Writer hiding ((<>))
import Data.List
\end{code}

The module |Text.PrettyPrint| provides data types and functions for
nicely formatted and indented output.
\begin{code}
import qualified Text.PrettyPrint as PP
import Text.PrettyPrint ((<+>), (<>))
\end{code}


\subsection{Preliminaries}

We start by defining the abstract syntax for both \emph{expressions}
(of type |Exp|), \emph{types} (|Type|) and \emph{type schemes}
(|Scheme|).  A type scheme $\forall a_1,...,a_n.t$ is a type in which a number of
polymorphic type variables are bound to a universal quantifier.

\begin{code}
data Exp     =  EVar String
             |  ELit Lit
             |  EApp Exp Exp
             |  EAbs String Exp
             |  ELet String Exp Exp
             |  EGetField Exp String
             |  ERecExtend String Exp Exp
             |  ERecEmpty
             deriving (Eq, Ord)

data Lit     =  LInt Integer
             |  LChar Char
             deriving (Eq, Ord)

data Type    =  TVar String
             |  TFun Type Type
             |  TCon String
             |  TApp Type Type
             |  TRecExtend String Type Type
             |  TRecEmpty
             deriving (Eq, Ord)

data Scheme  =  Scheme [String] Type
\end{code}
%
In order to provide readable output and error messages, we define
several pretty-printing functions for the abstract syntax.  These are
shown in Appendix~\ref{sec:pretty-printing}.

We will need to determine the free type variables of a type.  Function
|ftv| implements this operation, which we implement in the type class
|Types| because it will also be needed for type environments (to be
defined below).  Another useful operation on types, type schemes and
the like is that of applying a substitution.  A substitution only
replaces free type variables, so the quantified type variables in a
type scheme are not affected by a substitution.
\begin{code}
class Types a where
    ftv    ::  a -> Set.Set String
    apply  ::  Subst -> a -> a
\end{code}

\begin{code}
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
\end{code}

\begin{code}
instance Types Scheme where
    ftv (Scheme vars t)      =  (ftv t) `Set.difference` (Set.fromList vars)

    apply s (Scheme vars t)  =  Scheme vars (apply (foldr substDelete s vars) t)
\end{code}

It will occasionally be useful to extend the |Types| methods to lists.
\begin{code}
instance Types a => Types [a] where
    apply s  =  map (apply s)
    ftv l    =  foldr Set.union Set.empty (map ftv l)
\end{code}
%
Now we define substitutions, which are finite mappings from type
variables to types.
%
\begin{code}
newtype Subst = Subst (Map.Map String Type)

instance Monoid Subst where
  mempty = Subst Map.empty
  mappend (Subst s1) (Subst s2) = Subst ((Map.map (apply (Subst s1)) s2) `Map.union` s1)

substLookup :: String -> Subst -> Maybe Type
substLookup name (Subst s) = Map.lookup name s

substDelete :: String -> Subst -> Subst
substDelete name (Subst s) = Subst (Map.delete name s)
\end{code}
%
Type environments, called $\Gamma$ in the text, are mappings from term
variables to their respective type schemes.
%
\begin{code}
newtype TypeEnv = TypeEnv (Map.Map String Scheme)
\end{code}
%
We define several functions on type environments.  The operation
$\Gamma\backslash x$ removes the binding for $x$ from $\Gamma$ and is
called |remove|.
%
\begin{code}
instance Types TypeEnv where
    ftv (TypeEnv env)      =  ftv (Map.elems env)
    apply s (TypeEnv env)  =  TypeEnv (Map.map (apply s) env)
\end{code}
%
The function |generalize| abstracts a type over all type variables
which are free in the type but not free in the given type environment.
%
\begin{code}
generalize        ::  TypeEnv -> Type -> Scheme
generalize env t  =   Scheme vars t
  where vars = Set.toList ((ftv t) `Set.difference` (ftv env))
\end{code}

Several operations, for example type scheme instantiation, require
fresh names for newly introduced type variables.  This is implemented
by using an appropriate monad which takes care of generating fresh
names.  It is also capable of passing a dynamically scoped
environment, error handling and performing I/O, but we will not go
into details here.
\begin{code}
data TIEnv = TIEnv  {}

data TIState = TIState { tiSupply :: Int }

type TI m = EitherT String (ReaderT TIEnv (StateT TIState m))

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
\end{code}
%
The instantiation function replaces all bound type variables in a type
scheme with fresh type variables.
%
\begin{code}
instantiate :: Monad m => Scheme -> TI m Type
instantiate (Scheme vars t) = do  nvars <- mapM (\ _ -> newTyVar "a") vars
                                  let s = Subst (Map.fromList (zip vars nvars))
                                  return $ apply s t
\end{code}
%
This is the unification function for types.  For two types $t_1$ and
$t_2$, $mgu(t_1, t_2)$ returns the most general unifier.  A unifier is
a substitution $S$ such that $S(t_1) \equiv S(t_2)$.  The function |varBind| attempts to bind a type
variable to a type and return that binding as a subsitution, but
avoids binding a variable to itself and performs the occurs check,
which is responsible for circularity type errors.
%
\begin{code}
varBind :: Monad m => String -> Type -> TI m Subst
varBind u t  | t == TVar u           =  return mempty
             | u `Set.member` ftv t  =  throwError $ "occurs check fails: " ++ u ++
                                         " vs. " ++ show t
             | otherwise             =  return (Subst (Map.singleton u t))

data FlatRecord = FlatRecord
  { _frFields :: Map.Map String Type
  , _frExtension :: Maybe String -- TyVar of more possible fields
  }
frFields :: Functor f => (Map.Map String Type -> f (Map.Map String Type)) -> (FlatRecord -> f FlatRecord)
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

unifyRecToPartial :: Monad m => (Map.Map String Type, String) -> Map.Map String Type -> TI m Subst
unifyRecToPartial (tfields, tname) ufields
  | not (Map.null uniqueTFields) =
    throwError $ "Incompatible record types: " ++
    show (FlatRecord tfields (Just tname)) ++ " vs. " ++ show (FlatRecord ufields Nothing)
  | otherwise = varBind tname $ recToType $ FlatRecord uniqueUFields Nothing
  where
    uniqueTFields = tfields `Map.difference` ufields
    uniqueUFields = ufields `Map.difference` tfields

unifyRecPartials :: Monad m => (Map.Map String Type, String) -> (Map.Map String Type, String) -> TI m Subst
unifyRecPartials (tfields, tname) (ufields, uname) =
  do  restTv <- newTyVar "r"
      s1 <- varBind tname $ Map.foldWithKey TRecExtend restTv uniqueUFields
      s2 <- varBind uname $ apply s1 (Map.foldWithKey TRecExtend restTv uniqueTFields)
      return $ s1 `mappend` s2
  where
    uniqueTFields = tfields `Map.difference` ufields
    uniqueUFields = ufields `Map.difference` tfields

unifyRecFulls :: Monad m => Map.Map String Type -> Map.Map String Type -> TI m Subst
unifyRecFulls tfields ufields
  | Map.keys tfields /= Map.keys ufields =
    throwError $
    "Incompatible record types: " ++
    show (FlatRecord tfields Nothing) ++ " vs. " ++
    show (FlatRecord ufields Nothing)
  | otherwise = return mempty

unifyRecs :: Monad m => FlatRecord -> FlatRecord -> TI m Subst
unifyRecs (FlatRecord tfields tvar)
          (FlatRecord ufields uvar) =
  do  let unifyField t u =
              do  old <- get
                  s <- lift $ mgu (apply old t) (apply old u)
                  put (old `mappend` s)
      s1 <- (`execStateT` mempty) . sequence . Map.elems $ Map.intersectionWith unifyField tfields ufields
      s2 <-
          case (tvar, uvar) of
          (Nothing   , Nothing   ) -> unifyRecFulls tfields ufields
          (Just tname, Just uname) -> unifyRecPartials (tfields, tname) (ufields, uname)
          (Just tname, Nothing   ) -> unifyRecToPartial (tfields, tname) ufields
          (Nothing   , Just uname) -> unifyRecToPartial (ufields, uname) tfields
      return $ s1 `mappend` s2

mgu :: Monad m => Type -> Type -> TI m Subst
mgu (TFun l r) (TFun l' r')  =  do  s1 <- mgu l l'
                                    s2 <- mgu (apply s1 r) (apply s1 r')
                                    return (s1 `mappend` s2)
mgu (TApp l r) (TApp l' r')  =  do  s1 <- mgu l l'
                                    s2 <- mgu (apply s1 r) (apply s1 r')
                                    return (s1 `mappend` s2)
mgu (TVar u) t               =  varBind u t
mgu t (TVar u)               =  varBind u t
mgu (TCon t) (TCon u)
  | t == u                   =  return mempty
mgu TRecEmpty TRecEmpty      =  return mempty
mgu t@TRecExtend {}
    u@TRecExtend {}          =  join $ either throwError return $ unifyRecs <$> flattenRec t <*> flattenRec u
mgu t1 t2                    =  throwError $ "types do not unify: " ++ show t1 ++
                                " vs. " ++ show t2
\end{code}

\subsection{Main type inference function}
%
The function |ti| infers the types for expressions.  The type
environment must contain bindings for all free variables of the
expressions.  The returned substitution records the type constraints
imposed on type variables by the expression, and the returned type is
the type of the expression.
%
\begin{code}
typeInference :: Monad m => Map.Map String Scheme -> Exp -> TI m Type
typeInference rootEnv rootExpr =
    do  (t, s) <- runWriterT $ ti (TypeEnv rootEnv) rootExpr
        return (apply s t)

envLookup :: String -> TypeEnv -> Maybe Scheme
envLookup key (TypeEnv env) = Map.lookup key env

envInsert :: String -> Scheme -> TypeEnv -> TypeEnv
envInsert key scheme (TypeEnv env) = TypeEnv (Map.insert key scheme env)

ti :: Monad m => TypeEnv -> Exp -> WriterT Subst (TI m) Type
ti env expr = case expr of
  EVar n ->
    case envLookup n env of
       Nothing     ->  throwError $ "unbound variable: " ++ n
       Just sigma  ->  lift $ instantiate sigma
  ELit l -> tiLit l
  EAbs n e ->
    do  tv <- lift $ newTyVar "a"
        let env' = envInsert n (Scheme [] tv) env
        (t1, s1) <- listen $ ti env' e
        return (TFun (apply s1 tv) t1)
  EApp e1 e2 ->
    do  tv <- lift $ newTyVar "a"
        (t1, s1) <- listen $ ti env e1
        (t2, s2) <- listen $ ti (apply s1 env) e2
        s3 <- lift $ mgu (apply s2 t1) (TFun t2 tv)
        tell s3
        return (apply s3 tv)
    `catchError`
    \e -> throwError $ e ++ "\n in " ++ show expr
  ELet x e1 e2 ->
    do  (t1, s1) <- listen $ ti env e1
        let t' = generalize (apply s1 env) t1
            env' = envInsert x t' env
        ti (apply s1 env') e2
  EGetField e name ->
    do  tv <- lift $ newTyVar "a"
        tvRec <- lift $ newTyVar "r"
        (t, s) <- listen $ ti env e
        su <- lift $ mgu (apply s t) (TRecExtend name tv tvRec)
        tell su
        return (apply su tv)
  ERecEmpty -> return TRecEmpty
  ERecExtend name e1 e2 ->
    do  (t1, s1) <- listen $ ti env e1
        t2 <- ti (apply s1 env) e2
        return (TRecExtend name t1 t2)

tiLit :: Monad m => Lit -> WriterT Subst (TI m) Type
tiLit (LInt _)   =  return (TCon "Int")
tiLit (LChar _)  =  return (TCon "Char")
\end{code}

\subsection{Tests}
\label{sec:example-expressions}

The following simple expressions (partly taken from
\cite{Heeren2002GeneralizingHM}) are provided for testing the type
inference function.
%
\begin{code}
exp0 :: Exp
exp0  =  ELet "id" (EAbs "x" (EVar "x"))
          (EVar "id")

exp1 :: Exp
exp1  =  ELet "id" (EAbs "x" (EVar "x"))
          (EApp (EVar "id") (EVar "id"))

exp2 :: Exp
exp2  =  ELet "id" (EAbs "x" (ELet "y" (EVar "x") (EVar "y")))
          (EApp (EVar "id") (EVar "id"))

exp3 :: Exp
exp3  =  ELet "id" (EAbs "x" (ELet "y" (EVar "x") (EVar "y")))
          (EApp (EApp (EVar "id") (EVar "id")) (ELit (LInt 2)))

exp4 :: Exp
exp4  =  ELet "id" (EAbs "x" (EApp (EVar "x") (EVar "x")))
          (EVar "id")

exp5 :: Exp
exp5  =  EAbs "m" (ELet "y" (EVar "m")
                   (ELet "x" (EApp (EVar "y") (ELit (LChar 'x')))
                         (EVar "x")))

exp6 :: Exp
exp6  =  EApp (ELit (LInt 2)) (ELit (LInt 2))

exp7 :: Exp
exp7  =  EAbs "vec" $
         ERecExtend "newX" (EGetField (EVar "vec") "x") $
         ERecExtend "newY" (EGetField (EVar "vec") "y") $
         ERecEmpty

exp8 :: Exp
exp8  =  ELet
         "vec" ( ERecExtend "x" (ELit (LInt 5)) $
                 ERecExtend "y" (ELit (LInt 7)) $
                 ERecEmpty ) $
         EGetField (EVar "vec") "x"

exp9 :: Exp
exp9  =  ELet
         "vec" ( ERecExtend "x" (ELit (LInt 5)) $
                 ERecExtend "y" (ELit (LInt 7)) $
                 ERecEmpty ) $
         EGetField (EVar "vec") "z"

\end{code}
%
This simple test function tries to infer the type for the given
expression.  If successful, it prints the expression together with its
type, otherwise, it prints the error message.
%
\begin{code}
test :: Exp -> IO ()
test e =
    do  res <- runTI (typeInference Map.empty e)
        case res of
          Left err  ->  putStrLn $ show e ++ "\n " ++ err ++ "\n"
          Right t   ->  putStrLn $ show e ++ " :: " ++ show t ++ "\n"
\end{code}

\subsection{Main Program}

The main program simply infers the types for all the example
expression given in Section~\ref{sec:example-expressions} and prints
them together with their inferred types, or prints an error message if
type inference fails.

\begin{code}
main :: IO ()
main = mapM_ test [exp0, exp1, exp2, exp3, exp4, exp5, exp6, exp7, exp8, exp9]
-- |Collecting Constraints|
-- |main = mapM_ test' [exp0, exp1, exp2, exp3, exp4, exp5]|
\end{code}
%
This completes the implementation of the type inference algorithm.

\bibliographystyle{plain}
\bibliography{bibliography}

\appendix

\section{Pretty-printing}
\label{sec:pretty-printing}

This appendix defines pretty-printing functions and instances for
|Show| for all interesting type definitions.

%
\begin{code}
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

instance Show Exp where
    showsPrec _ x = shows (prExp x)

flattenERec :: Exp -> (Map.Map String Exp, Maybe Exp)
flattenERec (ERecExtend name val body) =
  flattenERec body
  & _1 %~ Map.insert name val
flattenERec ERecEmpty = (Map.empty, Nothing)
flattenERec other = (Map.empty, Just other)

prExp                  ::  Exp -> PP.Doc
prExp (EVar name)      =   PP.text name
prExp (ELit lit)       =   prLit lit
prExp (ELet x b body)  =   PP.text "let" <+>
                           PP.text x <+> PP.text "=" <+>
                           prExp b <+> PP.text "in" PP.$$
                           PP.nest 2 (prExp body)
prExp (EApp e1 e2)     =   prExp e1 <+> prParenExp e2
prExp (EAbs n e)       =   PP.char '\\' <> PP.text n <+>
                           PP.text "->" <+>
                           prExp e
prExp (EGetField e n)  =   prExp e <> PP.char '.' <> PP.text n
prExp ERecEmpty        =   PP.text "{}"
prExp x@ERecExtend {}  =
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
      (fields, mRest) = flattenERec x


prParenExp    ::  Exp -> PP.Doc
prParenExp t  =   case t of
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
\end{code}

\end{document}

\begin{code}
-- test' :: Exp -> IO ()
-- test' e =
--     do res <- runTI (bu Set.empty e)
--        case res of
--          Left err -> putStrLn $ "error: " ++ err
--          Right t  -> putStrLn $ show e ++ " :: " ++ show t

data Constraint = CEquivalent Type Type
                | CExplicitInstance Type Scheme
                | CImplicitInstance Type (Set.Set String) Type

instance Show Constraint where
    showsPrec _ x = shows (prConstraint x)

prConstraint :: Constraint -> PP.Doc
prConstraint (CEquivalent t1 t2) = PP.hsep [prType t1, PP.text "=", prType t2]
prConstraint (CExplicitInstance t s) =
    PP.hsep [prType t, PP.text "<~", prScheme s]
prConstraint (CImplicitInstance t1 m t2) =
    PP.hsep [prType t1,
             PP.text "<=" <>
               PP.parens (PP.hcat (PP.punctuate PP.comma (map PP.text (Set.toList m)))),
             prType t2]

type Assum = [(String, Type)]
type CSet = [Constraint]

-- bu :: Monad m => Set.Set String -> Exp -> TI m (Assum, CSet, Type)
-- bu _m (EVar n) = do b <- newTyVar "b"
--                     return ([(n, b)], [], b)
-- bu _m (ELit (LInt _)) = do b <- newTyVar "b"
--                            return ([], [CEquivalent b (TCon "Int")], b)
-- bu _m (ELit (LChar _)) = do b <- newTyVar "b"
--                             return ([], [CEquivalent b (TCon "Char")], b)
-- bu m (EApp e1 e2) =
--     do (a1, c1, t1) <- bu m e1
--        (a2, c2, t2) <- bu m e2
--        b <- newTyVar "b"
--        return (a1 ++ a2, c1 ++ c2 ++ [CEquivalent t1 (TFun t2 b)],
--                b)
-- bu m (EAbs x body) =
--     do b@(TVar vn) <- newTyVar "b"
--        (a, c, t) <- bu (vn `Set.insert` m) body
--        return (a `removeAssum` x, c ++ [CEquivalent t' b | (x', t') <- a,
--                                         x == x'], TFun b t)
-- bu m (ELet x e1 e2) =
--     do (a1, c1, t1) <- bu m e1
--        (a2, c2, t2) <- bu (x `Set.delete` m) e2
--        return (a1 ++ removeAssum a2 x,
--                c1 ++ c2 ++ [CImplicitInstance t' m t1 |
--                             (x', t') <- a2, x' == x], t2)

-- removeAssum :: Eq a => [(a, t)] -> a -> [(a, t)]
-- removeAssum [] _ = []
-- removeAssum ((n', _) : as) n | n == n' = removeAssum as n
-- removeAssum (a:as) n = a : removeAssum as n
\end{code}

\bibliographystyle{plain}
\bibliography{bibliography}

\end{document}

% Local Variables:
% mode: latex
% mmm-classes: literate-haskell-latex
% End:
