
module Besra.Pass.InferKinds ( pass ) where

import Protolude hiding ( Type, pass, show )
import Prelude ( Show(..) )
import Control.Monad.RWS hiding ( pass )
import qualified Data.List as List
import Data.Maybe ( fromJust )
import Data.Graph
import Besra.Types.IR2
import Besra.Types.Ann
import Besra.Types.Id
import Besra.Types.Kind
import Data.Map ( Map )
import qualified Data.Map as Map
import qualified Data.Text as T


-- The complete documentation for this algorithm can be found here:
-- https://github.com/luc-tielen/besra-lang/blob/master/docs/algorithms/kind_inference.md


-- TODO consistent naming of types?
-- TODO move to other file
data CompilerState (ph :: Phase)
  = CompilerState [ADT ph] [Trait ph] [Impl ph] KindEnv

pass :: MonadError KindError m
     => CompilerState 'Parsed
     -> Module 'Parsed
     -> m (Module 'KindInferred, CompilerState 'KindInferred)
pass (CompilerState adts traits impls kindEnv) ast =
  flip evalStateT kindEnv $ do
    -- TODO naming of helper functions
    adts' <- inferADTs adts
    traits' <- inferTraits traits
    impls' <- inferImpls impls
    ast' <- inferAST ast
    kindEnv' <- get
    pure (ast', CompilerState adts' traits' impls' kindEnv')

inferADTs :: (MonadError KindError m, MonadState KindEnv m)
          => [ADT 'Parsed]
          -> m [ADT 'KindInferred]
inferADTs adts = concatMapM inferADTGroup groupedADTs where
  adtsGraph = map (\adt -> (adt, adtName adt, adtRefersTo adt)) adts
  groupedADTs = graphToGroupedLists adtsGraph
  updateState (env', adts') = put env' $> adts'
  inferADTGroup adtGroup = do
    env <- get
    let result = runInfer (inferKindForADTs adtGroup) env
    either throwError updateState result

inferTraits :: (MonadError KindError m, MonadState KindEnv m)
            => [Trait 'Parsed]
            -> m [Trait 'KindInferred]
inferTraits = undefined

inferImpls :: (MonadError KindError m, MonadState KindEnv m)
           => [Impl 'Parsed]
           -> m [Impl 'KindInferred]
inferImpls = undefined

inferAST :: (MonadError KindError m, MonadState KindEnv m)
         => Module 'Parsed
         -> m (Module 'KindInferred)
inferAST = undefined

-- | Data type used internally for representing equations of kinds.
--   Compared to the normal kind type, this also contains a variable that
--   is used during the unification algorithm.
data IKind = IStar
           | IKArr IKind IKind
           | IKVar Id
           deriving Eq

-- TODO remove, not needed?
instance Show IKind where
  show IStar         = "*"
  show (IKArr k1 k2) = "(" <> show k1 <> " -> " <> show k2 <> ")"
  show (IKVar var)   = show var

-- | Function for getting the actual kind from an inferred kind.
--   This defaults kinds of remaining phantom type variables to *
normalizeKind :: IKind -> Kind
normalizeKind = \case
  IStar -> Star
  IKArr k1 k2 -> KArr (normalizeKind k1) (normalizeKind k2)
  IKVar _ -> Star

data KConstraint = KConstraint IKind IKind
  deriving Eq

-- TODO remove, not needed?
instance Show KConstraint where
  show (KConstraint k1 k2) = show k1 <> " ~ " <> show k2

type Counter = Int

type KAssump = (Id, IKind)

type KindEnv = Map Id IKind

-- TODO add spans
data KindError = UnificationFail IKind IKind
               | InfiniteKind Id IKind
  deriving (Eq, Show)

type Infer = RWST KindEnv () Counter (Except KindError)

newtype KSubst = KSubst (Map Id IKind)
  deriving (Eq, Show)

instance Semigroup KSubst where
  s1 <> s2 = substitute s1 s2 `union` s1
    where union (KSubst a) (KSubst b) = KSubst (a `Map.union` b)

instance Monoid KSubst where
  mempty = KSubst mempty


class Substitutable a where
  substitute :: KSubst -> a -> a

instance Substitutable a => Substitutable [a] where
  substitute = map . substitute

instance Substitutable KSubst where
  substitute s1 (KSubst m) = KSubst $ map (substitute s1) m

instance Substitutable IKind where
  substitute s@(KSubst subst) = \case
    IStar -> IStar
    IKArr k1 k2 -> IKArr (substitute s k1) (substitute s k2)
    k@(IKVar kv) -> Map.findWithDefault k kv subst

instance Substitutable KConstraint where
  substitute s (KConstraint k1 k2) =
    KConstraint (substitute s k1) (substitute s k2)


class HasType a ph where
  getType :: a -> Type ph

instance HasType (ADTHead ph) ph where
  getType (ADTHead _ ty) = ty

instance HasType (ConDecl ph) ph where
  getType (ConDecl _ _ ty) = ty


runInfer :: Infer a -> KindEnv -> Either KindError a
runInfer m env = fst <$> runExcept (evalRWST m env 0)

fresh :: Infer Id
fresh = do
  ctr <- get
  modify (+ 1)
  pure . Id $ "k" <> T.pack (show ctr)

inferKindForADTs :: [ADT 'Parsed] -> Infer (KindEnv, [ADT 'KindInferred])
inferKindForADTs adts = do
  let types = concatMap getTypeEquations adts
  updatedKindEnv <- solveConstraints types
  let adts' = map (enrichADT updatedKindEnv) adts
  pure (updatedKindEnv, adts')

-- | Helper function for extracting all the types used in the ADT.
getTypeEquations :: ADT 'Parsed -> [Type 'Parsed]
getTypeEquations (ADT _ hd bodies) =
  let hdType = getType hd
      bodyTypes = getType <$> bodies
   in hdType:bodyTypes


class Enrichable a where
  enrich :: a 'Parsed -> Reader KindEnv (a 'KindInferred)

instance Enrichable ADT where
  enrich (ADT sp hd bodies) = do
    enrichedHead <- enrich hd
    enrichedBodies <- mapM enrich bodies
    pure $ ADT sp enrichedHead enrichedBodies

instance Enrichable ADTHead where
  enrich (ADTHead name ty) = ADTHead name <$> enrich ty

instance Enrichable ConDecl where
  enrich (ConDecl sp name ty) = ConDecl sp name <$> enrich ty

instance Enrichable Type where
  enrich = \case
    TVar var -> TVar <$> enrich var
    TCon con -> TCon <$> enrich con
    TApp t1 t2 -> TApp <$> enrich t1 <*> enrich t2

instance Enrichable Tyvar where
  enrich (Tyvar sp var) = do
    k <- asks (getKindForId var)
    pure $ Tyvar (sp, k) var

instance Enrichable Tycon where
  enrich (Tycon sp con) = do
    k <- asks (getKindForId con)
    pure $ Tycon (sp, k) con

getKindForId :: Id -> Map Id IKind -> Kind
getKindForId target = normalizeKind . fromJust . Map.lookup target

enrichADT :: KindEnv -> ADT 'Parsed -> ADT 'KindInferred
enrichADT kindEnv adt = runReader (enrich adt) kindEnv

solveConstraints :: [Type 'Parsed] -> Infer KindEnv
solveConstraints ts = do
  results <- traverse infer ts
  let combine :: Monoid a => (([KAssump], [KConstraint], IKind) -> a) -> a
      combine f = mconcat $ map f results
      as = combine (\(as', _, _) -> as')
      cs = combine (\(_, cs', _) -> cs')
      constraints = cs <> eqKConstraints as
  subst <- solve constraints
  pure $ mkKindEnv as subst

-- | Infers the kind of an expression at the type level
infer :: Type 'Parsed -> Infer ([KAssump], [KConstraint], IKind)
infer = \case
  TVar (Tyvar _ varName) -> do
    kv <- IKVar <$> fresh
    pure ([(varName, kv)], mempty, kv)
  TCon (Tycon _ con) -> do
    kv <- IKVar <$> fresh
    maybeK <- asks (Map.lookup con)
    let cs = maybe mempty (\k -> [KConstraint kv k]) maybeK
    pure ([(con, kv)], cs, kv)
  TApp f arg -> do
    (as1, cs1, k1) <- infer f
    (as2, cs2, k2) <- infer arg
    kv <- IKVar <$> fresh
    let cs = cs1 <> cs2 <> [KConstraint k1 (IKArr k2 kv)]
    pure (as1 <> as2, cs, kv)

-- | Since we have multiple equations all with different variables,
--   generate extra constraints making them equal
eqKConstraints :: [KAssump] -> [KConstraint]
eqKConstraints as =
  let groupById = List.groupBy ((==) `on` fst) . List.sortOn fst
      groupedAs = groupById as
      groupedEquations = map (map snd) groupedAs
      allCombinations xs = [(x, y) | x <- xs, y <- xs, x /= y]
      groupedCs = map allCombinations groupedEquations
   in uncurry KConstraint <$> mconcat groupedCs

solve :: [KConstraint] -> Infer KSubst
solve [] = pure mempty
solve (KConstraint k1 k2 : cs) = do
  su1 <- unify k1 k2
  su2 <- solve $ substitute su1 cs
  pure $ su2 <> su1

unify :: IKind -> IKind -> Infer KSubst
unify k1 k2 | k1 == k2 = pure mempty
unify (IKVar v) k = v `bindTo` k
unify k (IKVar v) = v `bindTo` k
unify (IKArr k1 k2) (IKArr k3 k4) = do
  su1 <- unify k1 k3
  su2 <- unify (substitute su1 k2) (substitute su1 k4)
  pure $ su2 <> su1
unify k1 k2 = throwError $ UnificationFail k1 k2

bindTo :: Id -> IKind -> Infer KSubst
bindTo kv k
  | k == IKVar kv = pure mempty
  | occursCheck kv k = throwError $ InfiniteKind kv k
  | otherwise = pure . KSubst $ Map.fromList [(kv, k)]

occursCheck :: Id -> IKind -> Bool
occursCheck kv k = kv `elem` kindVars where
  kindVars = getVars k
  getVars = \case
    IKArr k1 k2 -> getVars k1 ++ getVars k2
    IKVar v -> [v]
    IStar -> []

-- TODO refactor
mkKindEnv :: [KAssump] -> KSubst -> KindEnv
mkKindEnv as subst =
  let trimmedAs = List.nubBy ((==) `on` fst) as
      filledInAs = [(var, substitute subst k) | (var, k) <- trimmedAs]
  in Map.fromList filledInAs

graphToGroupedLists :: Ord key => [(node, key, [key])] -> [[node]]
graphToGroupedLists = map f . stronglyConnComp
  where
    f = \case
      AcyclicSCC node -> [node]
      CyclicSCC nodes -> nodes

