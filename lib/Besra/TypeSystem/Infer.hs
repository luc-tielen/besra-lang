
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

-- TODO clean up type synonyms
type KI = KindInferred
type Tyvar' = Tyvar KI
type Type' = Type KI
type Pred' = Pred KI
type Scheme' = Scheme KI
type Pattern' = Pattern KI
type Expr' = Expr KI
type Alt' = Alt KI
type Implicit' = Implicit KI
type Explicit' = Explicit KI
type BindGroup' = BindGroup KI
type Module' = Module KI
type Infer e t = TraitEnv -> [Assump] -> e -> TI ([Pred'], t)


tArrow :: Span -> Type'
tArrow sp = TCon (Tycon (sp, KArr Star (KArr Star Star)) (Id "->"))

fn :: Span -> Type' -> Type' -> Type'
fn sp a = TApp (TApp (tArrow sp) a)


-- | Performs type inference for literals.
tiLit :: Span -> Lit -> TI ([Pred'], Type')
tiLit sp = \case
  LNumber _ -> pure ([], mkLitType "Int")
  LString _ -> pure ([], mkLitType "String")
  LChar _ -> pure ([], mkLitType "Char")
  where mkLitType typeName = TCon (Tycon (sp, Star) (Id typeName))

-- TODO lambda case
-- | Performs type inference for a single pattern.
tiPat :: Pattern' -> TI ([Pred'], [Assump], Type')
tiPat (PVar i) = do  -- TODO add span to pvar and use here 2x
  v <- newTVar (Span 0 0) Star
  pure ([], [i :>: toScheme (Span 0 0) v], v)
tiPat PWildcard = do
  v <- newTVar (Span 0 0) Star  -- TODO add span to pwildcard
  pure ([], [], v)
tiPat (PLit l) = do
  (ps, t) <- tiLit (Span 0 0) l  -- TODO add span to PLit
  pure (ps, [], t)
tiPat (PCon _ sc pats) = do
  (ps, as, ts) <- tiPats pats
  t' <- newTVar (Span 0 0) Star  -- TODO add span to pcon
  (qs :=> t) <- freshInst sc
  unify t (foldr (fn (Span 0 0)) t' ts)
  pure (ps <> qs, as, t')
tiPat (PAs i pat) = do
  (ps, as, t) <- tiPat pat
  pure (ps, (i :>: toScheme (span t) t):as, t)

-- | Performs type inference for multiple patterns.
tiPats :: [Pattern'] -> TI ([Pred'], [Assump], [Type'])
tiPats pats = do
  psasts <- traverse tiPat pats
  let ps = concat [ps' | (ps', _, _) <- psasts]
      as = concat [as' | (_, as', _) <- psasts]
      ts = [t | (_, _, t) <- psasts]
  pure (ps, as, ts)

-- TODO lambda case
-- | Performs type inference for a single expression.
tiExpr :: Infer Expr' Type'
tiExpr _ as (EVar sp i) = do
  sc <- findScheme sp i as
  (ps :=> t) <- freshInst sc
  pure (ps, t)
tiExpr _ _ (ECon _ _ sc) = do
  (ps :=> t) <- freshInst sc
  pure (ps, t)
tiExpr _ _ (ELit ann l) = do
  (ps, t) <- tiLit (span ann) l
  pure (ps, t)
tiExpr ce as (EApp ann e f) = do
  let sp = span ann
  (ps, te) <- tiExpr ce as e
  (qs, tf) <- tiExpr ce as f
  t <- newTVar sp Star
  unify (fn sp tf t) te
  pure (ps <> qs, t)
tiExpr ce as (ELet _ bg e) = do
  (ps, as') <- tiBindGroup ce as bg
  (qs, t) <- tiExpr ce (as' <> as) e
  pure (ps <> qs, t)
tiExpr ce as (ELam _ alt) = tiAlt ce as alt
tiExpr ce as (ECase ann e branches) = do
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
tiExpr ce as (EIf ann c t f) = do
  v <- newTVar (span ann) Star
  (ps, c') <- tiExpr ce as c
  (qs, t') <- tiExpr ce as t
  (rs, f') <- tiExpr ce as f
  unify c' (TCon (Tycon (span c, Star) (Id "Bool")))
  unify t' v
  unify f' v
  pure (ps <> qs <> rs, v)


-- Performs type inference for function binding.
tiAlt :: Infer Alt' Type'
tiAlt ce as (pats, e) = do
  (ps, as', ts) <- tiPats pats
  (qs, t) <- tiExpr ce (as' <> as) e
  pure (ps <> qs, foldr (fn (span e)) t ts)

-- Performs type inference for multiple function binding.
tiAlts :: TraitEnv -> [Assump] -> [Alt'] -> Type' -> TI [Pred']
tiAlts ce as alts t = do
  psts <- traverse (tiAlt ce as) alts
  traverse_ (unify t . snd) psts
  pure (concatMap fst psts)

-- | Performs type inference for a binding group.
tiBindGroup :: Infer BindGroup' [Assump]
tiBindGroup ce as (es, ims) = do
  let as' = [v :>: sc | Explicit v sc _ <- es]
  (ps, as'') <- tiSeq tiImpls ce (as' <> as) ims
  qss <- traverse (tiExpl ce (as'' <> as' <> as)) es
  pure (ps <> concat qss, as'' <> as')

-- | Performs type inference for a list of binding groups,
--   accumulates assumptions along the way
tiSeq :: Infer bg [Assump] -> Infer [bg] [Assump]
tiSeq _ _ _ [] = pure ([], [])
tiSeq ti ce as (bs:bss) = do
  (ps, as') <- ti ce as bs
  (qs, as'') <- tiSeq ti ce (as' <> as) bss
  pure (ps <> qs, as'' <> as')

restricted :: [Implicit'] -> Bool
restricted = any simple
  where simple (Implicit _ alts) = any (null . fst) alts

-- TODO refactor
-- | Performs type inference for groups of mutually recursive,
--   implicitly typed bindings
tiImpls :: Infer [Implicit'] [Assump]
tiImpls ce as bs = do
  ts <- traverse (\_ -> newTVar (Span 0 0) Star) bs  -- TODO where to get span from?
  let names = map (\(Implicit name _) -> name) bs
      altss = map (\(Implicit _ alts) -> alts) bs
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
tiExpl :: TraitEnv -> [Assump] -> Explicit' -> TI [Pred']
tiExpl ce as expl@(Explicit _ sc alts) = do
  (qs :=> t) <- freshInst sc
  ps <- tiAlts ce as alts t
  s <- get
  let qs' = apply s qs  -- TODO naming of all these vars
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

quantify :: [Tyvar'] -> Qual KI Type -> Scheme'
quantify vs qt = ForAll (span qt) ks (apply s qt)
  where
    vs' = [v | v <- ftv qt, v `elem` vs]
    ks = map kind vs'
    s = Subst $ zip vs' (map TGen [0 ..])

split :: MonadError Error m
      => TraitEnv -> [Tyvar'] -> [Tyvar'] -> [Pred']
      -> m ([Pred'], [Pred'])
split ce fs gs ps = do
  ps' <- reduceContext ce ps
  let (ds, rs) = partition (all (`elem` fs) . ftv) ps'
  rs' <- defaultedPreds ce (fs <> gs) rs
  pure (ds, rs \\ rs')

type Ambiguity = (Tyvar', [Pred'])

ambiguities :: [Tyvar'] -> [Pred'] -> [Ambiguity]
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
  ] <>
  numClasses

candidates :: MonadError Error m => TraitEnv -> Ambiguity -> m [Type']
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
  => ([Ambiguity] -> [Type'] -> a)
  -> TraitEnv
  -> [Tyvar']
  -> [Pred']
  -> m a
withDefaults f ce vs ps = do
  tss <- traverse (candidates ce) vps
  if any null tss
    then throwError $ AmbiguousDefaults vs ps  -- TODO improve error
    else pure (f vps (map (unsafeFromJust . head) tss))
  where
    vps = ambiguities vs ps

defaultedPreds :: MonadError Error m
               => TraitEnv -> [Tyvar'] -> [Pred'] -> m [Pred']
defaultedPreds = withDefaults (\vps _ -> concatMap snd vps)

defaultSubst :: MonadError Error m
             => TraitEnv -> [Tyvar'] -> [Pred'] -> m Subst
defaultSubst = withDefaults (\vps ts -> Subst $ zip (map fst vps) ts)

tiProgram :: TraitEnv -> [Assump] -> Module' -> Either Error [Assump]
tiProgram ce as (Module bg) = runTI $ do
  (ps, as') <- tiBindGroup ce as bg
  s <- get
  rs <- reduceContext ce (apply s ps)
  s' <- defaultSubst ce [] rs
  pure (apply (s' <> s) as')

toScheme :: Ann KI -> Type' -> Scheme'
toScheme ann ty = ForAll ann [] ([] :=> ty)

-- TODO naming of vars in general
