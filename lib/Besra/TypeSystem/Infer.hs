
module Besra.TypeSystem.Infer
  ( Infer
  , tiProgram
  ) where

import Protolude hiding ( Type, Alt )
import Unsafe ( unsafeFromJust )
import qualified Data.Map as Map
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

type Infer e t = TraitEnv -> [Assump] -> e -> TI ([Pred PreTC], t)


-- | Performs type inference for literals.
tiLit :: Span -> Lit -> TI ([Pred PreTC], Type PreTC)
tiLit sp = \case
  LNumber _ -> pure ([], mkLitType "Int")
  LString _ -> pure ([], mkLitType "String")
  LChar _ -> pure ([], mkLitType "Char")
  where mkLitType typeName = TCon (Tycon (sp, Star) (Id typeName))

-- | Performs type inference for a single pattern.
tiPat :: Pattern PreTC -> TI ([Pred PreTC], [Assump], Type PreTC)
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
tiPats :: [Pattern PreTC] -> TI ([Pred PreTC], [Assump], [Type PreTC])
tiPats pats = do
  psasts <- traverse tiPat pats
  let ps = concat [ps' | (ps', _, _) <- psasts]
      as = concat [as' | (_, as', _) <- psasts]
      ts = [t | (_, _, t) <- psasts]
  pure (ps, as, ts)

-- | Performs type inference for a single expression.
tiExpr :: Infer (Expr PreTC) (Type PreTC)
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
tiAlt :: Infer (Alt PreTC) (Type PreTC)
tiAlt ce as (pats, e) = do
  (ps, as', ts) <- tiPats pats
  (qs, t) <- tiExpr ce (as' <> as) e
  pure (ps <> qs, foldr (fn (span e)) t ts)

-- Performs type inference for multiple function binding.
tiAlts :: TraitEnv -> [Assump] -> [Alt PreTC] -> Type PreTC -> TI [Pred PreTC]
tiAlts ce as alts t = do
  psts <- traverse (tiAlt ce as) alts
  traverse_ (unify t . snd) psts
  pure (concatMap fst psts)

-- | Performs type inference for a binding group.
tiBindGroup :: Infer (BindGroup PreTC) [Assump]
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

restricted :: [Implicit PreTC] -> Bool
restricted = any simple
  where simple (Implicit _ alts) = any (null . fst) alts

-- | Performs type inference for groups of mutually recursive,
--   implicitly typed bindings
tiImpls :: Infer [Implicit PreTC] [Assump]
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
tiExpl :: TraitEnv -> [Assump] -> Explicit PreTC -> TI [Pred PreTC]
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
  if | not (alphaEquivScheme sc sc') -> throwError $ ExplicitTypeMismatch sc' sc
     | not (null rs) -> throwError $ ContextTooWeak expl rs
     | otherwise -> pure ds

-- | Helper function to check if type schemes are alpha equivalent.
alphaEquivScheme :: Scheme PreTC -> Scheme PreTC -> Bool
alphaEquivScheme (ForAll _ ks1 qt1) (ForAll _ ks2 qt2) =
  length ks1 == length ks2 && evalState (go qt1 qt2) (mempty, mempty)
  where
    go (ps1 :=> t1) (ps2 :=> t2) =
      (&&) <$> (and <$> zipWithM goPred ps1 ps2) <*> goType t1 t2
    goPred (IsIn _ name1 ts1) (IsIn _ name2 ts2) =
      (name1 == name2 &&) <$> (and <$> zipWithM goType ts1 ts2)
    goType (TVar (Tyvar _ v1)) (TVar (Tyvar _ v2)) = do
      i <- bind fst first v1
      j <- bind snd second v2
      pure (i == j)
    goType (TCon c1) (TCon c2) = pure $ c1 == c2
    goType (TApp t11 t12) (TApp t21 t22) =
      (&&) <$> goType t11 t21 <*> goType t12 t22
    goType _ _ = pure False
    bind f g v = gets f >>= \ctx ->
      case Map.lookup v ctx of
        Nothing -> do
          let num = Map.size ctx
          modify . g $ Map.insert v num
          pure num
        Just num -> pure num

quantify :: [Tyvar PreTC] -> Qual PreTC Type -> Scheme PreTC
quantify vs qt = ForAll (span qt) ks (apply s qt)
  where
    vs' = [v | v <- ftv qt, v `elem` vs]
    ks = map kind vs'
    s = Subst $ zip vs' (map TGen [0 ..])

split :: MonadError Error m
      => TraitEnv -> [Tyvar PreTC] -> [Tyvar PreTC] -> [Pred PreTC]
      -> m ([Pred PreTC], [Pred PreTC])
split ce fs gs ps = do
  ps' <- reduceContext ce ps
  let (ds, rs) = partition (all (`elem` fs) . ftv) ps'
  rs' <- defaultedPreds ce (fs <> gs) rs
  pure (ds, rs \\ rs')

type Ambiguity = (Tyvar PreTC, [Pred PreTC])

ambiguities :: [Tyvar PreTC] -> [Pred PreTC] -> [Ambiguity]
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

candidates :: MonadError Error m => TraitEnv -> Ambiguity -> m [Type PreTC]
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
  => ([Ambiguity] -> [Type PreTC] -> a)
  -> TraitEnv
  -> [Tyvar PreTC]
  -> [Pred PreTC]
  -> m a
withDefaults f ce vs ps = do
  tss <- traverse (candidates ce) vps
  if any null tss
    then throwError $ AmbiguousDefaults vs ps  -- TODO improve error
    else pure (f vps (map (unsafeFromJust . head) tss))
  where
    vps = ambiguities vs ps

defaultedPreds :: MonadError Error m
               => TraitEnv -> [Tyvar PreTC] -> [Pred PreTC] -> m [Pred PreTC]
defaultedPreds = withDefaults (\vps _ -> concatMap snd vps)

tiProgram :: TraitEnv -> [Assump] -> Module PreTC -> Either Error Subst
tiProgram ce as (Module es) = runTI $ do
  let as' = [v :>: sc | Explicit v sc _ <- es]
  traverse_ (tiExpl ce (as' <> as)) es
  get

toScheme :: Ann PreTC -> Type PreTC -> Scheme PreTC
toScheme ann ty = ForAll ann [] ([] :=> ty)

tArrow :: Span -> Type PreTC
tArrow sp = TCon (Tycon (sp, KArr Star (KArr Star Star)) (Id "->"))

fn :: Span -> Type PreTC -> Type PreTC -> Type PreTC
fn sp a = TApp (TApp (tArrow sp) a)

