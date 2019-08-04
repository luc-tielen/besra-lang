
{-# LANGUAGE UndecidableInstances #-}

module Besra.Pass.InferKinds ( pass, CompilerState(..) ) where

import Protolude hiding ( Type, pass, show )
import Data.Maybe ( fromJust )
import Data.Graph
import Besra.TypeSystem.KindSolver
import Besra.Types.IR2
import Besra.Types.Ann
import Besra.Types.Id
import Besra.Types.Kind
import qualified Data.Map as Map
import Data.Map ( Map )


-- The complete documentation for this algorithm can be found here:
-- https://github.com/luc-tielen/besra-lang/blob/master/docs/algorithms/kind_inference.md

-- TODO consistent naming of types?
-- TODO move to other file
data CompilerState (ph :: Phase)
  = CompilerState [ADT ph] [Trait ph] [Impl ph] KindEnv

deriving instance AnnHas Eq ph => Eq (CompilerState ph)
deriving instance AnnHas Show ph => Show (CompilerState ph)


pass :: MonadError KindError m
     => CompilerState 'Parsed
     -> Module 'Parsed
     -> m (Module 'KindInferred, CompilerState 'KindInferred)
pass (CompilerState adts _ _ kindEnv) _ =
  flip evalStateT kindEnv $ do
    -- TODO naming of helper functions
    adts' <- inferADTs adts
    --traits' <- inferTraits traits
    --impls' <- inferImpls impls
    --ast' <- inferAST ast
    kindEnv' <- get
    --pure (ast', CompilerState adts' traits' impls' kindEnv')
    pure (Module [], CompilerState adts' [] [] kindEnv')

-- TODO refactor these 2 blocks of code into 1
inferADTs :: (MonadError KindError m, MonadState KindEnv m)
          => [ADT 'Parsed]
          -> m [ADT 'KindInferred]
inferADTs adts = concatMapM inferADTGroup groupedADTs where
  adtsGraph = map (\adt -> (adt, adtName adt, adtRefersTo adt)) adts
  groupedADTs = graphToGroupedLists adtsGraph
  updateState (solution, adts') = do
    oldEnv <- get
    let newEnv = oldEnv <> (toIKind . normalizeKind <$> solution)
    put newEnv $> adts'
  inferADTGroup adtGroup = do
    env <- get
    let result = runInfer (inferKindForADTs adtGroup) env
    either throwError updateState result


{-
inferTraits :: (MonadError KindError m, MonadState KindEnv m)
            => [Trait 'Parsed]
            -> m [Trait 'KindInferred]
inferTraits traits = concatMapM inferTraitGroup groupedTraits where
  traitsGraph = map (\t -> (t, traitName t, traitRefersTo t)) traits
  groupedTraits = graphToGroupedLists traitsGraph
  updateState (env', traits') = put env' $> traits'
  inferTraitGroup traitGroup = do
    env <- get
    -- TODO runInfer puts ctr at 0 each group
    let result = runInfer (inferKindForTraits traitGroup) env
    either throwError updateState result

inferImpls :: (MonadError KindError m, MonadState KindEnv m)
           => [Impl 'Parsed]
           -> m [Impl 'KindInferred]
inferImpls = undefined

inferAST :: (MonadError KindError m, MonadState KindEnv m)
         => Module 'Parsed
         -> m (Module 'KindInferred)
inferAST = undefined
-}


class HasType a ph where
  getType :: a -> Type ph

instance HasType (ADTHead ph) ph where
  getType (ADTHead _ ty) = ty

instance HasType (ConDecl ph) ph where
  getType (ConDecl _ _ ty) = ty


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

{-
inferKindForTraits :: [Trait 'Parsed] -> Infer (KindEnv, [Trait 'KindInferred])
inferKindForTraits traits = undefined
-}


{- TODO different typeclass? can be used for impls as well
    or only use it after constraints solved
instance Enrichable Module where
  enrich (Module decls) =
    Module <$> traverse enrich decls

instance Enrichable Decl where
    TypeAnnDecl typeAnn -> TypeAnnDecl <$> enrich typeAnn
    BindingDecl binding -> BindingDecl <$> enrich binding

instance Enrichable TypeAnn where
  enrich (TypeAnn sp name sch) =
    TypeAnn sp name <$> enrich sch

instance Enrichable Binding where
  enrich (Binding sp name expr) =
    Binding sp name <$> enrich expr

instance Enrichable Expr where
  enrich = \case
    ELit sp lit -> pure $ ELit sp lit
    EVar sp var -> pure $ EVar sp var
    ECon sp con -> pure $ ECon sp con
    ELam sp args body -> ELam sp args <$> enrich body
    EApp sp f arg ->
      EApp sp <$> enrich f <*> enrich arg
    EIf sp cnd tr fl ->
      EIf sp <$> enrich cnd <*> enrich tr <*> enrich fl
    ECase sp expr clauses ->
      ECase sp <$> enrich expr <*> traverse (traverse enrich) clauses
    ELet sp decls body ->
      ELet sp <$> traverse enrich decls <*> enrich body

instance Enrichable Scheme where
  enrich (Scheme sp ps ty) =
    Scheme sp <$> traverse enrich ps <*> enrich ty

instance Enrichable Pred where
  enrich (IsIn sp name ts) =
    IsIn sp name <$> traverse enrich ts
      -}

enrichADT :: KindEnv -> ADT 'Parsed -> ADT 'KindInferred
enrichADT kindEnv adt = runReader (enrich adt) kindEnv

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

graphToGroupedLists :: Ord key => [(node, key, [key])] -> [[node]]
graphToGroupedLists = map f . stronglyConnComp
  where
    f = \case
      AcyclicSCC node -> [node]
      CyclicSCC nodes -> nodes

