
module Besra.TypeSystem.TypeClass () where

{-
module Besra.TypeSystem.TypeClass
  ( predHead  -- TODO cleanup exports
  , (<:>)
  , addInst
  , addClass
  , entail
  , reduce
  , defaults
  , mkInst
  , instances
  , classes
  , initialEnv
  , sig
  , super
  , insts
  , overlap
  , bySuper
  , byInst
  , scEntail
  , simplify
  ) where

import Protolude hiding (Type, reduce)
import Control.Monad.Fail ( MonadFail(..) )
import qualified Data.Text as T
import Besra.Types.Id
import Besra.Types.Ann
import Besra.Types.Kind
import Besra.TypeSystem.Instantiate
import Besra.TypeSystem.Subst
import Besra.TypeSystem.Unify
import Besra.Types.IR3 ( Qual(..), Pred(..), Type(..), Tyvar(..) )


type KI = KindInferred

-- | Type synonym for representing all relevant data of a type class.
type Class = ([Tyvar KI], [Pred KI], [Inst])

-- | Type synonym for an instance declaration of a typeclass.
type Inst = Qual KI Pred

data ClassEnv = ClassEnv
  { classes  :: Id -> Maybe Class
  , defaults :: [Type KI]
  }

type EnvTransformer = ClassEnv -> Maybe ClassEnv

-- TODO name
predHead :: Pred ph -> Id
predHead (IsIn _ i _) = i

sig :: ClassEnv -> Id -> [Tyvar KI]
sig ce i =
  case classes ce i of
    Just (vs, _, _) -> vs

super :: ClassEnv -> Id -> [Pred KI]
super ce i =
  case classes ce i of
    Just (_, is, _) -> is

insts :: ClassEnv -> Id -> [Inst]
insts ce i =
  case classes ce i of
    Just (_, _, its) -> its

defined :: Maybe a -> Bool
defined = isJust

modifyEnv :: ClassEnv -> Id -> Class -> ClassEnv
modifyEnv ce i c =
  ce
    { classes = \j ->
          if i == j
            then Just c
            else classes ce j
    }

initialEnv :: ClassEnv
initialEnv =
  ClassEnv
    {classes = \_ -> fail "class not defined"}

infixr 5 <:>
(<:>) :: EnvTransformer -> EnvTransformer -> EnvTransformer
(<:>) = (>=>)

-- | Adds a typeclass to the environment.
addClass :: Id -> [Tyvar KI] -> [Pred KI] -> EnvTransformer
addClass i vs ps ce
  | defined (classes ce i) = fail "class already defined"
  | any (not . defined . classes ce . predHead) ps =
    fail "superclass not defined"
  | otherwise = pure (modifyEnv ce i (vs, ps, []))

-- | Adds a type class instance to the environment.
addInst :: [Pred KI] -> Pred KI -> EnvTransformer
addInst ps p@(IsIn name i _) ce
  | not (defined (classes ce i)) = fail "no class for instance"
  | any (overlap p) qs = fail "overlapping instance"
  | otherwise = pure (modifyEnv ce i c)
  where
    its = insts ce i
    qs = [q | (_ :=> q) <- its]
    c = (sig ce i, super ce i, (ps :=> p) : its)

-- | Helper function to check for overlapping instances.
overlap :: Pred KI -> Pred KI -> Bool
overlap p q = defined (mgu p q)

-- | Get the list of all typeclass constraints that have to be true
--   when a certain typeclass constraint is required, based on superclass information.
bySuper :: ClassEnv -> Pred KI -> [Pred KI]
bySuper ce p@(IsIn _ i ts) = p : concatMap (bySuper ce) supers
  where
    supers = apply s (super ce i)
    s = zip (sig ce i) ts

-- | Get the list of all subgoals for a typeclass constraint that have to be met,
--   based on instance information.
byInst :: ClassEnv -> Pred KI -> Maybe [Pred KI]
byInst ce p@(IsIn _ i _) = msum [tryInst it | it <- insts ce i]
  where
    tryInst (ps :=> h) = do
      u <- match h p
      pure (map (apply u) ps)

-- | Returns True if the predicate will hold when all other predicates also hold.
entail :: ClassEnv -> [Pred KI] -> Pred KI -> Bool
entail ce ps p =
  scEntail ce ps p ||
  case byInst ce p of
    Nothing -> False
    Just qs -> all (entail ce ps) qs

-- | Returns True if the predicate will hold when all other predicates also hold (done using only superclass information).
scEntail :: ClassEnv -> [Pred KI] -> Pred KI -> Bool
scEntail ce ps p = any (p `elem`) (map (bySuper ce) ps)

-- | Simplifies a list of predicates by removing all 'redundant' predicates.
simplify :: ([Pred KI] -> Pred KI -> Bool) -> [Pred KI] -> [Pred KI]
simplify ent = loop []
  where
    loop rs [] = rs
    loop rs (p:ps)
      | ent (rs ++ ps) p = loop rs ps
      | otherwise = loop (p : rs) ps

-- | Performs context reduction for a set of predicates.
--   This will effectively simplify the set of typeclass constraints needed for something.
reduce :: ClassEnv -> [Pred KI] -> [Pred KI]
reduce ce = simplify (scEntail ce) . elimTauts ce

-- | Filters out predicates that require other predicates to hold (no entailment).
elimTauts :: ClassEnv -> [Pred KI] -> [Pred KI]
elimTauts ce ps = [p | p <- ps, not (entail ce [] p)]


mkInst :: Instantiate a => [Kind] -> a -> a
mkInst ks = inst ts
  where
    ts = zipWith (\v k -> TVar (Tyvar v k)) vars ks
    vars =
      map (Id . T.pack) $
      [[c] | c <- ['a' .. 'z']] ++
      [c : show n | n <- [0 :: Int ..], c <- ['a' .. 'z']]

-- | Adds a list of instances to the typeclass environment.
instances :: [Inst] -> EnvTransformer
instances = foldr ((<:>) . (\(ps :=> p) -> addInst ps p)) pure

-}
