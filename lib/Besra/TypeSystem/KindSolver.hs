
module Besra.TypeSystem.KindSolver
  ( KindEnv
  , PredEnv
  , Env(..)
  , KindError(..)
  , Assump
  , Constraint(..)
  , Subst
  , Substitutable(..)
  , Infer
  , IKind(..)
  , runInfer
  , infer
  , solve
  , addKnownConstraint
  , sameVarConstraints
  , normalizeKind
  , normalizeIKind
  , mkKindEnv
  ) where

import Protolude hiding ( Constraint, Type, show )
import Prelude ( Show(..) )
import Unsafe ( unsafeHead )
import Control.Monad.RWS.Strict
import Besra.Types.IR2 ( Type(..), Tyvar(..), Tycon(..) )
import Besra.Types.Kind
import Besra.Types.Span
import Besra.Types.Ann
import Besra.Types.Id
import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Text as T


-- | Data type used internally for representing equations of kinds.
--   Compared to the normal kind type, this also contains a variable that
--   is used during the unification algorithm.
--   TODO: Use "Maybe Span" or add filename to Span for more clear errors
data IKind = IStar Span
           | IKArr Span IKind IKind
           | IKVar Span Id
           deriving (Eq, Ord, Show)

(===) :: IKind -> IKind -> Bool
IStar _ === IStar _ = True
IKVar _ kv === IKVar _ kv' = kv == kv'
IKArr _ k1 k2 === IKArr _ k1' k2' =
  k1 === k1' && k2 === k2'
_ === _ = False

type Assump = (Id, IKind)

data Constraint = Constraint IKind IKind
  deriving (Eq, Show)

type Counter = Int

type KindEnv = Map Id IKind

-- | Data type for keeping track of mapping for traits and the corresponding
--   kinds for each of the type variables in a trait. It is only used here
--   to keep all information relevant to kinds in 1 place, but it's not used
--   directly in this file, but it is used in the InferKinds pass.
type PredEnv = Map Id [IKind]

data Env = Env
         { kEnv :: KindEnv
         , kPredEnv :: PredEnv
         } deriving (Eq, Show)

data KindError
  = UnificationFail IKind IKind
  | InfiniteKind Id IKind
  deriving (Eq, Show)

type Infer = RWST Env () Counter (Except KindError)

newtype Subst = Subst (Map Id IKind)
  deriving (Eq, Show)

instance Semigroup Subst where
  s1 <> s2 = substitute s1 s2 `union` s1
    where union (Subst a) (Subst b) = Subst (a `Map.union` b)

instance Monoid Subst where
  mempty = Subst mempty


class Substitutable a where
  substitute :: Subst -> a -> a

instance Substitutable a => Substitutable [a] where
  substitute = map . substitute

instance Substitutable Subst where
  substitute s1 (Subst m) = Subst $ map (substitute s1) m

instance Substitutable IKind where
  substitute s@(Subst subst) = \case
    IStar sp -> IStar sp
    IKArr sp k1 k2 -> IKArr sp (substitute s k1) (substitute s k2)
    k@(IKVar _ kv) -> Map.findWithDefault k kv subst

instance Substitutable Constraint where
  substitute s (Constraint k1 k2) =
    Constraint (substitute s k1) (substitute s k2)


runInfer :: Infer a -> Env -> Either KindError a
runInfer m env = fst <$> runExcept (evalRWST m env 0)

-- | Infers the kind of an expression at the type level
infer :: Type Parsed -> Infer ([Assump], [Constraint], IKind)
infer = \case
  TVar (Tyvar sp varName) -> do
    kv <- IKVar sp <$> fresh
    pure ([(varName, kv)], mempty, kv)
  TCon (Tycon sp con) -> do
    kv <- IKVar sp <$> fresh
    maybeK <- asks (Map.lookup con . kEnv)
    let cs = maybe mempty (\k -> [Constraint kv k]) maybeK
    pure ([(con, kv)], cs, kv)
  t@(TApp f arg) -> do
    (as1, cs1, k1) <- infer f
    (as2, cs2, k2) <- infer arg
    let sp = span t
    kv <- IKVar sp <$> fresh
    let cs = cs1 <> cs2 <> [Constraint k1 (IKArr sp k2 kv)]
    pure (as1 <> as2, cs, kv)

addKnownConstraint :: Span -> IKind -> Id -> Infer ([Assump], [Constraint])
addKnownConstraint sp k var = do
  kv <- IKVar sp <$> fresh
  let cs = [Constraint kv k]
  pure ([(var, kv)], cs)

fresh :: Infer Id
fresh = do
  ctr <- get
  modify (+ 1)
  pure . Id $ "k" <> T.pack (show ctr)

solve :: [Constraint] -> Infer Subst
solve [] = pure mempty
solve (Constraint k1 k2 : cs) = do
  su1 <- unify k1 k2
  su2 <- solve $ substitute su1 cs
  pure $ su2 <> su1

unify :: IKind -> IKind -> Infer Subst
unify k1 k2 | k1 === k2 = pure mempty
unify (IKVar _ v) k = v `bindTo` k
unify k (IKVar _ v) = v `bindTo` k
unify (IKArr _ k1 k2) (IKArr _ k3 k4) = do
  su1 <- unify k1 k3
  su2 <- unify (substitute su1 k2) (substitute su1 k4)
  pure $ su2 <> su1
unify k1 k2 = throwError $ UnificationFail k1 k2

bindTo :: Id -> IKind -> Infer Subst
bindTo kv (IKVar _ k) | k == kv = pure mempty
bindTo kv k
  | occursCheck kv k = throwError $ InfiniteKind kv k
  | otherwise = pure . Subst $ Map.fromList [(kv, k)]

occursCheck :: Id -> IKind -> Bool
occursCheck kv k = kv `elem` kindVars where
  kindVars = getVars k
  getVars = \case
    IKArr _ k1 k2 -> getVars k1 ++ getVars k2
    IKVar _ v -> [v]
    IStar _ -> []


mkKindEnv :: [Assump] -> Subst -> KindEnv
mkKindEnv as subst =
  let trimmedAs = uniq as
      filledInAs = [(var, substitute subst k) | (var, k) <- trimmedAs]
  in Map.fromList filledInAs

uniq :: Ord a => [a] -> [a]
uniq = map unsafeHead . group . sort

-- | Since we can have multiple equations all with different variables,
--   generate extra constraints making them equal
sameVarConstraints :: [Assump] -> [Constraint]
sameVarConstraints as =
  let groupById = List.groupBy ((==) `on` fst) . List.sortOn fst
      groupedAs = groupById as
      groupedEquations = map (map snd) groupedAs
      getCombinations (x:xs) = [(x, y) | y <- xs, x /= y]
      getCombinations _ = []
      groupedCs = map getCombinations groupedEquations
   in uncurry Constraint <$> mconcat groupedCs

-- | Function for getting the actual kind from an inferred kind.
--   This defaults kinds of remaining phantom type variables to *
normalizeKind :: IKind -> Kind
normalizeKind = \case
  IStar _ -> Star
  IKArr _ k1 k2 -> KArr (normalizeKind k1) (normalizeKind k2)
  IKVar _ _ -> Star

normalizeIKind :: IKind -> IKind
normalizeIKind = \case
  IStar sp -> IStar sp
  IKArr sp k1 k2 -> IKArr sp (normalizeIKind k1) (normalizeIKind k2)
  IKVar sp _ -> IStar sp

