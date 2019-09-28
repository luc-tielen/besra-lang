
module Besra.TypeSystem.Infer
  ( Infer
  , tiProgram
  ) where

import Protolude hiding ( Type, Alt )
import Unsafe ( unsafeFromJust )
import Control.Monad.Loops ( allM )
import Besra.Types.IR3
import Besra.TypeSystem.FreeTypeVars
import Besra.TypeSystem.TypeClass
import Besra.TypeSystem.Assump
import Besra.TypeSystem.Subst
import Besra.TypeSystem.Error
import Besra.TypeSystem.TI
import Besra.Types.Kind
import Besra.Types.Span
import Besra.Types.Ann
import Besra.Types.Id
import Data.List (intersect, partition, union, (\\))

type KI = KindInferred

type Infer e t = TraitEnv -> [Assump] -> e -> TI ([Pred KI], t)


-- | Performs type inference for literals.
tiLit :: Span -> Lit -> TI ([Pred KI], Type KI)
tiLit sp = \case
  LNumber _ -> pure ([], mkLitType "Int")
  LString _ -> pure ([], mkLitType "String")
  LChar _ -> pure ([], mkLitType "Char")
  where mkLitType typeName = TCon (Tycon (sp, Star) (Id typeName))

-- | Performs type inference for a single pattern.
tiPat :: Pattern KI -> TI ([Pred KI], [Assump], Type KI)
tiPat = \case
  PVar ann i -> do
    v <- newTVar ann Star
    pure ([], [i :>: toScheme ann v], v)
  PWildcard ann -> do
    v <- newTVar ann Star
    pure ([], [], v)
  PLit ann l -> do
    (ps, t) <- tiLit ann l
    pure (ps, [], t)
  PCon ann _ sc pats -> do
    (ps, as, ts) <- tiPats pats
    t' <- newTVar ann Star
    (qs :=> t) <- freshInst sc
    unify t (foldr (fn ann) t' ts)
    pure (ps <> qs, as, t')
  PAs ann i pat -> do
    (ps, as, t) <- tiPat pat
    pure (ps, (i :>: toScheme ann t):as, t)

-- | Performs type inference for multiple patterns.
tiPats :: [Pattern KI] -> TI ([Pred KI], [Assump], [Type KI])
tiPats pats = do
  psasts <- traverse tiPat pats
  let ps = concat [ps' | (ps', _, _) <- psasts]
      as = concat [as' | (_, as', _) <- psasts]
      ts = [t | (_, _, t) <- psasts]
  pure (ps, as, ts)

-- | Performs type inference for a single expression.
tiExpr :: Infer (Expr KI) (Type KI)
tiExpr ce as = \case
  EVar sp i -> do
    sc <- findScheme sp i as
    (ps :=> t) <- freshInst sc
    pure (ps, t)
  ECon _ _ sc -> do
    (ps :=> t) <- freshInst sc
    pure (ps, t)
  ELit ann l -> do
    (ps, t) <- tiLit (span ann) l
    pure (ps, t)
  EApp ann e f -> do
    let sp = span ann
    (ps, te) <- tiExpr ce as e
    (qs, tf) <- tiExpr ce as f
    t <- newTVar sp Star
    unify (fn sp tf t) te
    pure (ps <> qs, t)
  ELet _ bg e -> do
    (ps, as') <- tiBindGroup ce as bg
    (qs, t) <- tiExpr ce (as' <> as) e
    pure (ps <> qs, t)
  ELam _ alt -> tiAlt ce as alt
  ECase ann e branches -> do
    (ps, t) <- tiExpr ce as e
    v <- newTVar (span ann) Star
    let tiBr (pat, f) = do
          (ps', as', t') <- tiPat pat
          unify t t'
          (qs, t'') <- tiExpr ce (as' <> as) f
          unify v t''
          pure (ps' <> qs)
    pss <- traverse tiBr branches
    pure (ps <> concat pss, v)
  EIf ann c t f -> do
    v <- newTVar (span ann) Star
    (ps, c') <- tiExpr ce as c
    (qs, t') <- tiExpr ce as t
    (rs, f') <- tiExpr ce as f
    unify c' (TCon (Tycon (span c, Star) (Id "Bool")))
    unify t' v
    unify f' v
    pure (ps <> qs <> rs, v)

-- Performs type inference for function binding.
tiAlt :: Infer (Alt KI) (Type KI)
tiAlt ce as (pats, e) = do
  (ps, as', ts) <- tiPats pats
  (qs, t) <- tiExpr ce (as' <> as) e
  pure (ps <> qs, foldr (fn (span e)) t ts)

-- Performs type inference for multiple function binding.
tiAlts :: TraitEnv -> [Assump] -> [Alt KI] -> Type KI -> TI [Pred KI]
tiAlts ce as alts t = do
  psts <- traverse (tiAlt ce as) alts
  traverse_ (unify t . snd) psts
  pure (concatMap fst psts)

-- | Performs type inference for a binding group.
tiBindGroup :: Infer (BindGroup KI) [Assump]
tiBindGroup ce as (es, ims) = do
  let as' = [v :>: sc | Explicit v sc _ <- es]
  (ps, as'') <- tiSeq tiImpls ce (as' <> as) ims
  qss <- traverse (tiExpl ce (as'' <> as' <> as)) es
  pure (ps <> concat qss, as'' <> as')

-- | Performs type inference for a list of binding groups,
--   accumulates assumptions along the way
tiSeq :: Infer bg [Assump] -> Infer [bg] [Assump]
tiSeq ti ce as = \case
  [] -> pure ([], [])
  bs:bss -> do
    (ps, as') <- ti ce as bs
    (qs, as'') <- tiSeq ti ce (as' <> as) bss
    pure (ps <> qs, as'' <> as')

restricted :: [Implicit KI] -> Bool
restricted = any simple
  where simple (Implicit _ alts) = any (null . fst) alts

-- | Performs type inference for groups of mutually recursive,
--   implicitly typed bindings
tiImpls :: Infer [Implicit KI] [Assump]
tiImpls ce as bs = do
  ts <- traverse (\_ -> newTVar (Span 0 0) Star) bs  -- TODO where to get span from?
  let (names, altss) = unzip [(name, alts) | Implicit name alts <- bs]
      scs = map (\t -> toScheme (span t) t) ts
      as' = zipWith (:>:) names scs <> as
  pss <- zipWithM (tiAlts ce as') altss ts
  s <- get
  let ps' = apply s (concat pss)
      ts' = apply s ts
      fs = ftv (apply s as)
      vss = map ftv ts'
      gs = foldr union [] vss \\ fs
  (ds, rs) <- split ce fs (foldr intersect [] vss) ps'
  if restricted bs
    then let gs' = gs \\ ftv rs
             scs' = map (quantify gs' . ([] :=>)) ts'
          in pure (ds <> rs, zipWith (:>:) names scs')
    else let scs' = map (quantify gs . (rs :=>)) ts'
          in pure (ds, zipWith (:>:) names scs')

-- | Performs type inference for groups of mutually recursive,
--   explicitly typed bindings
tiExpl :: TraitEnv -> [Assump] -> Explicit KI -> TI [Pred KI]
tiExpl ce as expl@(Explicit _ sc alts) = do
  (qs :=> t) <- freshInst sc
  ps <- tiAlts ce as alts t
  s <- get
  let qs' = apply s qs
      t' = apply s t
      fs = ftv (apply s as)
      gs = ftv t' \\ fs
      sc' = quantify gs (qs' :=> t')
      ps' = apply s ps
  ps'' <- filterM (map not . entail ce qs') ps'
  (ds, rs) <- split ce fs gs ps''
  if | sc /= sc' -> throwError $ TooGeneralSignatureGiven sc' sc
     | not (null rs) -> throwError $ ContextTooWeak expl rs
     | otherwise -> pure ds

quantify :: [Tyvar KI] -> Qual KI Type -> Scheme KI
quantify vs qt = ForAll (span qt) ks (apply s qt)
  where
    vs' = [v | v <- ftv qt, v `elem` vs]
    ks = map kind vs'
    s = Subst $ zip vs' (map TGen [0 ..])

split :: MonadError Error m
      => TraitEnv -> [Tyvar KI] -> [Tyvar KI] -> [Pred KI]
      -> m ([Pred KI], [Pred KI])
split ce fs gs ps = do
  ps' <- reduceContext ce ps
  let (ds, rs) = partition (all (`elem` fs) . ftv) ps'
  rs' <- defaultedPreds ce (fs <> gs) rs
  pure (ds, rs \\ rs')

type Ambiguity = (Tyvar KI, [Pred KI])

ambiguities :: [Tyvar KI] -> [Pred KI] -> [Ambiguity]
ambiguities vs ps = [(v, filter (elem v . ftv) ps) | v <- ftv ps \\ vs]

-- TODO simplify
numClasses :: [Id]
numClasses =
  Id <$> ["Num", "Integral", "Floating", "Fractional", "Real", "RealFloat", "RealFrac"]

-- TODO simplify
stdClasses :: [Id]
stdClasses =
  map Id [ "Eq"
  , "Ord"
  , "Show"
  , "Read"
  , "Bounded"
  , "Enum"
  , "Ix"
  , "Functor"
  , "Monad"
  , "MonadPlus"
  ] <> numClasses

candidates :: MonadError Error m => TraitEnv -> Ambiguity -> m [Type KI]
candidates ce (v, qs) = do
  let is' = concat
        [ is
        | let (is, ts) = unzip [(i, t)| IsIn _ i t <- qs]
        , all ([TVar v] ==) ts
        , any (`elem` numClasses) is
        , all (`elem` stdClasses) is
        ]
      ts' = defaults ce
  flip filterM ts' $ \t -> do
    let ps = [IsIn (span t) i [t] | i <- is']
    allM (entail ce []) ps

withDefaults ::
     MonadError Error m
  => ([Ambiguity] -> [Type KI] -> a)
  -> TraitEnv
  -> [Tyvar KI]
  -> [Pred KI]
  -> m a
withDefaults f ce vs ps = do
  tss <- traverse (candidates ce) vps
  if any null tss
    then throwError $ AmbiguousDefaults vs ps  -- TODO improve error
    else pure (f vps (map (unsafeFromJust . head) tss))
  where
    vps = ambiguities vs ps

defaultedPreds :: MonadError Error m
               => TraitEnv -> [Tyvar KI] -> [Pred KI] -> m [Pred KI]
defaultedPreds = withDefaults (\vps _ -> concatMap snd vps)

defaultSubst :: MonadError Error m
             => TraitEnv -> [Tyvar KI] -> [Pred KI] -> m Subst
defaultSubst = withDefaults (\vps ts -> Subst $ zip (map fst vps) ts)

tiProgram :: TraitEnv -> [Assump] -> Module KI -> Either Error [Assump]
tiProgram ce as (Module es) = runTI $ do
 -- (ps, as') <- tiBindGroup ce as bg
  ps <- traverse (tiExpl ce as) es
  s <- get
  rs <- reduceContext ce (apply s $ concat ps)
  s' <- defaultSubst ce [] rs
  pure (apply (s' <> s) as)  -- TODO is this apply still needed now?

toScheme :: Ann KI -> Type KI -> Scheme KI
toScheme ann ty = ForAll ann [] ([] :=> ty)

tArrow :: Span -> Type KI
tArrow sp = TCon (Tycon (sp, KArr Star (KArr Star Star)) (Id "->"))

fn :: Span -> Type KI -> Type KI -> Type KI
fn sp a = TApp (TApp (tArrow sp) a)

