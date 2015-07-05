module Lamdu.Suggest
    ( loadSuggest
    ) where

import           Control.Applicative (Applicative(..), (<$>))
import           Control.Compose ((:.)(..), unO)
import qualified Control.Lens as Lens
import           Control.Lens.Operators
import           Control.Monad (join)
import           Data.Map (Map)
import qualified Data.Map as Map
import qualified Lamdu.Expr.Lens as ExprLens
import           Lamdu.Expr.Nominal (Nominal)
import qualified Lamdu.Expr.Nominal as Nominal
import           Lamdu.Expr.Pure (($$))
import qualified Lamdu.Expr.Pure as P
import qualified Lamdu.Expr.Scheme as Scheme
import           Lamdu.Expr.Type (Type)
import qualified Lamdu.Expr.Type as T
import           Lamdu.Expr.Val (Val(..))
import qualified Lamdu.Expr.Val as V
import qualified Lamdu.Infer as Infer

data Env f = Env
    { envNominals :: Map T.Id Nominal
    , envMakeVar :: f V.Var
    , envContext :: Infer.Context
    }

loadSuggest ::
    (Applicative f, Applicative m) =>
    (T.Id -> m Nominal) -> Type ->
    m (Infer.Context -> f V.Var -> [f (Val ())])
loadSuggest loadNominal typ =
    typ ^.. ExprLens.typeTIds
    <&> join (,)
    & Map.fromList
    & Lens.traverse loadNominal
    <&>
    \nominals context makeVar ->
    value Env
    { envNominals = nominals
    , envMakeVar = makeVar
    , envContext = context
    } typ

value :: Applicative f => Env f -> Type -> [f (Val ())]
value _ T.TVar{} = [pure P.hole]
value _ T.TInst{} = [pure P.hole]
value env (T.TSum (T.CExtend f t sumType)) =
    (value env t <&> Lens.mapped %~ P.inject f) ++
    value env (T.TSum sumType)
value _ (T.TSum T.CEmpty) = [] -- Void value uninhabitable
value _ (T.TSum T.CVar {}) = [pure P.hole]
value _ T.TInt = [pure P.hole]
value env (T.TRecord composite) = record env composite
value env (T.TFun arg res) = function env arg res

function :: Applicative f => Env f -> Type -> Type -> [f (Val ())]
function env (T.TSum composite) res = _case env composite res
function env (T.TInst tId _) res =
    case Map.lookup tId (envNominals env) of
    Nothing -> error $ "Suggest of unloaded T.Id: " ++ show tId
    Just nominal ->
        case Nominal.nScheme nominal ^. Scheme.schemeType of
        T.TSum composite ->
            unO $ f <$> O [envMakeVar env] <*> O (_case env composite res)
            where
                f varName lamCase =
                    P.lambda varName $ \var -> lamCase $$ P.fromNom tId var
        _ -> [pure P.hole]
function env _ res = unO $ P.abs <$> O [envMakeVar env] <*> O (value env res)

record :: Applicative f => Env f -> T.Product -> [f (Val ())]
record _ T.CVar{} = [pure P.hole]
record _ T.CEmpty = [pure P.recEmpty]
record env (T.CExtend f t r) =
    unO $ P.recExtend f <$> O (value env t) <*> O (record env r)

_case :: Applicative f => Env f -> T.Sum -> Type -> [f (Val ())]
_case _ T.CVar{} _ = [pure P.hole]
_case _ T.CEmpty _ = [pure P.absurd]
_case env (T.CExtend f t r) res =
    unO $ P._case f
    <$> O (value env (T.TFun t res))
    <*> O (_case env r res)
