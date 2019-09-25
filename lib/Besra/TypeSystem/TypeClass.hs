
module Besra.TypeSystem.TypeClass
  ( TraitEnv
  , (<:>)
  , addImpl
  , addTrait
  , reduceContext
  , entail
  , defaults
  , initialEnv
  ) where


import Protolude hiding (Type)
import Unsafe ( unsafeFromJust )
import Control.Monad.Loops ( allM )
import qualified Data.Map as Map
import qualified Data.List as List
import Besra.Types.Id
import Besra.Types.Ann
import Besra.Types.Span
import Besra.TypeSystem.Subst
import Besra.TypeSystem.Unify
import Besra.TypeSystem.Error
import Besra.Types.IR3 ( Qual(..), Pred(..), Type(..), Tyvar(..) )


type KI = KindInferred

-- | Type synonym for representing all relevant data of a trait.
--   This includes the set of variables in a trait,
--   the supertraits of the trait and the list of impls that implement
--   the trait.
type Trait = (Span, [Tyvar KI], [Pred KI], [Impl])

-- | Type synonym for an impl declaration of a trait.
type Impl = Qual KI Pred

data TraitEnv = TraitEnv
  { traits  :: Map Id Trait
  , defaults :: [Type KI]
  }

type EnvTransformer = TraitEnv -> Either Error TraitEnv

initialEnv :: TraitEnv
initialEnv =
  TraitEnv
  { traits = Map.empty
  , defaults = []
  }

modifyEnv :: TraitEnv -> Id -> Trait -> TraitEnv
modifyEnv ce i c =
  ce { traits = Map.insert i c $ traits ce }

lookupEnv :: TraitEnv -> Id -> Maybe Trait
lookupEnv ce name =
  Map.lookup name $ traits ce

sig :: MonadError Error m => TraitEnv -> Span -> Id -> m [Tyvar KI]
sig ce sp i =
  case lookupEnv ce i of
    Just (_, vs, _, _) -> pure vs
    Nothing -> throwError $ UnknownTrait sp i

super :: MonadError Error m => TraitEnv -> Span -> Id -> m [Pred KI]
super ce sp i =
  case lookupEnv ce i of
    Just (_, _, is, _) -> pure is
    Nothing -> throwError $ UnknownTrait sp i

impls :: MonadError Error m => TraitEnv -> Span -> Id -> m [Impl]
impls ce sp i =
  case lookupEnv ce i of
    Just (_, _, _, its) -> pure its
    Nothing -> throwError $ UnknownTrait sp i


(<:>) :: EnvTransformer -> EnvTransformer -> EnvTransformer
(<:>) = (>=>)
infixr 5 <:>


-- | Adds a trait impl to the environment.
addImpl :: [Pred KI] -> Pred KI -> EnvTransformer
addImpl ps p@(IsIn ann i _) ce
  | not (isTraitDefined (lookupEnv ce i)) =
    throwError $ NoTraitForImpl ann i
  | otherwise = do
      its <- impls ce ann i
      signature <- sig ce ann i
      superTraits <- super ce ann i
      let qs = [q | (_ :=> q) <- its]
          c = (ann, signature, superTraits, (ps :=> p) : its)
      if | any (overlap p) qs -> throwError $ OverlappingImpls p qs
         | otherwise -> pure (modifyEnv ce i c)

-- | Adds a trait to the environment.
addTrait :: Span -> Id -> [Tyvar KI] -> [Pred KI] -> EnvTransformer
addTrait sp i vs ps ce
  | isTraitDefined traitInfo =
    let (sp', _, _, _) = unsafeFromJust traitInfo
     in throwError $ TraitAlreadyDefined sp' sp i
  | any (not . superTraitDefined) ps =
    let firstNotDefined = unsafeFromJust $ List.find (not . superTraitDefined) ps
     in throwError $ SuperTraitNotDefined firstNotDefined
  | otherwise = pure (modifyEnv ce i (sp, vs, ps, []))
  where traitInfo = lookupEnv ce i
        superTraitDefined = isTraitDefined . lookupEnv ce . predName

predName :: Pred ph -> Id
predName (IsIn _ i _) = i

isTraitDefined :: Maybe a -> Bool
isTraitDefined = isJust

-- | Helper function to check for overlapping impls.
overlap :: Pred KI -> Pred KI -> Bool
overlap p q = isRight (mgu p q)

-- | Get the list of all trait constraints that have to be true
--   when a certain trait constraint is required, based on supertrait information.
bySuper :: MonadError Error m => TraitEnv -> Pred KI -> m [Pred KI]
bySuper ce p@(IsIn ann i ts) = do
  signature <- sig ce ann i
  let s = Subst $ zip signature ts
  supers <- apply s <$> super ce ann i
  (p :) <$> concatMapM (bySuper ce) supers

-- | Get the list of all subgoals for a trait constraint that have to be met,
--   based on impl information.
byImpl :: TraitEnv -> Pred KI -> Either Error [Pred KI]
byImpl ce p@(IsIn ann i _) = findSubGoals . map tryImpl =<< impls ce ann i
  where
    tryImpl (ps :=> h) = do
      u <- match h p
      pure (map (apply u) ps)
    findSubGoals xs = case partitionEithers xs of
      ([], []) -> Left $ NoImplsForTrait p
      (x:_, _) -> Left x
      (_, x:_) -> Right x

-- | Returns True if the predicate will hold when all other predicates also hold.
entail :: MonadError Error m => TraitEnv -> [Pred KI] -> Pred KI -> m Bool
entail ce ps p = do
  entails <- stEntail ce ps p
  if entails
    then pure True
    else byImpl ce p & \case
      Left _ -> pure False
      Right qs -> allM (entail ce ps) qs

-- | Returns True if the predicate will hold when all other predicates also hold (done using only supertrait information).
stEntail :: MonadError Error m => TraitEnv -> [Pred KI] -> Pred KI -> m Bool
stEntail ce ps p =
  any (p `elem`) <$> traverse (bySuper ce) ps

-- | Performs context reduction for a set of predicates.
--   This will effectively simplify the set of trait constraints needed for something.
reduceContext :: MonadError Error m => TraitEnv -> [Pred KI] -> m [Pred KI]
reduceContext ce ps =
  simplify (stEntail ce) =<< elimTauts ce ps

-- | Simplifies a list of predicates by removing all 'redundant' predicates.
simplify :: MonadError Error m
         => ([Pred KI] -> Pred KI -> m Bool)
         -> [Pred KI] -> m [Pred KI]
simplify ent = loop [] where
  loop rs [] = pure rs
  loop rs (p:ps) = ent (rs <> ps) p >>= \case
    True -> loop rs ps
    False -> loop (p : rs) ps

-- | Filters out predicates that require other predicates to hold (no entailment).
elimTauts :: MonadError Error m => TraitEnv -> [Pred KI] -> m [Pred KI]
elimTauts ce = filterM $ map not . entail ce []

