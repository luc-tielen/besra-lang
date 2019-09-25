module Besra.TypeSystem.Infer () where

{-
module Besra.TypeSystem.Infer
  ( Infer
  , tiProgram
  , tiProgram'
  ) where


import Protolude hiding ( Type )
import Control.Monad.Fail ( fail )  -- TODO remove
import Besra.Types.IR3 ( BindGroup, Lit, Pattern, Pred, Scheme, Type)
import Besra.TypeSystem.TypeClass
import Besra.TypeSystem.Assump
import Besra.TypeSystem.Subst
import Besra.TypeSystem.TI
import Besra.Types.Id
import Besra.Types.Kind
import Data.List (intersect, partition, union, (\\))


type Infer e t = ClassEnv -> [Assump] -> e -> TI ([Pred], t)
type Alt' = Alt Pattern TExpr
type Impl' = Impl Pattern TExpr
type Expl' = Expl Scheme Pattern TExpr
type BindGroup' = BindGroup Scheme Pattern TExpr


tInt :: Type
tInt = TCon (Tycon (Id "Int") Star)

tChar :: Type
tChar = TCon (Tycon (Id "Char") Star)

tString :: Type
tString = list tChar

list :: Type -> Type
list = TAp tList

tList :: Type
tList = TCon (Tycon (Id "[]") (KArr Star Star))

-- | Performs type inference for literals.
tiLit :: Literal -> TI ([Pred], Type)
tiLit (LitNum _) = pure ([], tInt)
tiLit (LitStr _) = pure ([], tString)

-- | Performs type inference for a single pattern.
tiPat :: Pattern -> TI ([Pred], [Assump], Type)
tiPat (PVar i) = do
  v <- newTVar Star
  pure ([], [i :>: toScheme v], v)
tiPat PWildcard = do
  v <- newTVar Star
  pure ([], [], v)
tiPat (PLit l) = do
  (ps, t) <- tiLit l
  pure (ps, [], t)
tiPat (PCon (_ :>: sc) pats) = do
  (ps, as, ts) <- tiPats pats
  t' <- newTVar Star
  (qs :=> t) <- freshInst sc
  unify t (foldr fn t' ts)
  pure (ps ++ qs, as, t')

-- | Performs type inference for multiple patterns.
tiPats :: [Pattern] -> TI ([Pred], [Assump], [Type])
tiPats pats = do
  psasts <- mapM tiPat pats
  let ps = concat [ps' | (ps', _, _) <- psasts]
      as = concat [as' | (_, as', _) <- psasts]
      ts = [t | (_, _, t) <- psasts]
  pure (ps, as, ts)

-- | Performs type inference for a single expression.
tiExpr :: Infer TExpr Type
tiExpr _ as (Var i) = do
  sc <- find i as
  (ps :=> t) <- freshInst sc
  pure (ps, t)
tiExpr _ _ (Const (_ :>: sc)) = do
  (ps :=> t) <- freshInst sc
  pure (ps, t)
tiExpr _ _ (Lit l) = do
  (ps, t) <- tiLit l
  pure (ps, t)
tiExpr ce as (Ap e f) = do
  (ps, te) <- tiExpr ce as e
  (qs, tf) <- tiExpr ce as f
  t <- newTVar Star
  unify (tf `fn` t) te
  pure (ps ++ qs, t)
tiExpr ce as (Let bg e) = do
  (ps, as') <- tiBindGroup ce as bg
  (qs, t) <- tiExpr ce (as' ++ as) e
  pure (ps ++ qs, t)
tiExpr ce as (Lam alt) = tiAlt ce as alt
tiExpr ce as (Case e branches) = do
  (ps, t) <- tiExpr ce as e
  v <- newTVar Star
  let tiBr (pat, f) = do
        (ps', as', t') <- tiPat pat
        unify t t'
        (qs, t'') <- tiExpr ce (as' ++ as) f
        unify v t''
        pure (ps' ++ qs)
  pss <- mapM tiBr branches
  pure (ps ++ concat pss, v)

-- Performs type inference for function binding.
tiAlt :: Infer Alt' Type
tiAlt ce as (pats, e) = do
  (ps, as', ts) <- tiPats pats
  (qs, t) <- tiExpr ce (as' ++ as) e
  pure (ps ++ qs, foldr fn t ts)

-- Performs type inference for multiple function binding.
tiAlts :: ClassEnv -> [Assump] -> [Alt'] -> Type -> TI [Pred]
tiAlts ce as alts t = do
  psts <- mapM (tiAlt ce as) alts
  mapM_ (unify t . snd) psts
  pure (concatMap fst psts)

-- | Performs type inference for a binding group.
tiBindGroup :: Infer BindGroup' [Assump]
tiBindGroup ce as (es, iss) = do
  let as' = [v :>: sc | (v, sc, _) <- es]
  (ps, as'') <- tiSeq tiImpls ce (as' ++ as) iss
  qss <- mapM (tiExpl ce (as'' ++ as' ++ as)) es
  pure (ps ++ concat qss, as'' ++ as')

-- | Performs type inference for a list of binding groups,
--   accumulates assumptions along the way
tiSeq :: Infer bg [Assump] -> Infer [bg] [Assump]
tiSeq _ _ _ [] = pure ([], [])
tiSeq ti ce as (bs:bss) = do
  (ps, as') <- ti ce as bs
  (qs, as'') <- tiSeq ti ce (as' ++ as) bss
  pure (ps ++ qs, as'' ++ as')

restricted :: [Impl'] -> Bool
restricted = any simple
  where
    simple (_, alts) = any (null . fst) alts

-- | Performs type inference for groups of mutually recursive,
--   implicitly typed bindings
tiImpls :: Infer [Impl'] [Assump]
tiImpls ce as bs = do
  ts <- mapM (\_ -> newTVar Star) bs
  let is = map fst bs
      scs = map toScheme ts
      as' = zipWith (:>:) is scs ++ as
      altss = map snd bs
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
          in pure (ds ++ rs, zipWith (:>:) is scs')
    else let scs' = map (quantify gs . (rs :=>)) ts'
          in pure (ds, zipWith (:>:) is scs')

-- | Performs type inference for groups of mutually recursive,
--   explicitly typed bindings
tiExpl :: ClassEnv -> [Assump] -> Expl' -> TI [Pred]
tiExpl ce as (_, sc, alts) = do
  (qs :=> t) <- freshInst sc
  ps <- tiAlts ce as alts t
  s <- get
  let qs' = apply s qs
      t' = apply s t
      fs = ftv (apply s as)
      gs = ftv t' \\ fs
      sc' = quantify gs (qs' :=> t')
      ps' = filter (not . entail ce qs') (apply s ps)
  (ds, rs) <- split ce fs gs ps'
  if sc /= sc'
    then fail "signature too general"
    else if not (null rs)
           then fail "context too weak"
           else pure ds

split :: Monad m => ClassEnv -> [Tyvar] -> [Tyvar] -> [Pred] -> m ([Pred], [Pred])
split ce fs gs ps = do
  let ps' = reduce ce ps
      (ds, rs) = partition (all (`elem` fs) . ftv) ps'
  rs' <- defaultedPreds ce (fs ++ gs) rs
  pure (ds, rs \\ rs')

type Ambiguity = (Tyvar, [Pred])

ambiguities :: [Tyvar] -> [Pred] -> [Ambiguity]
ambiguities vs ps = [(v, filter (elem v . ftv) ps) | v <- ftv ps \\ vs]

numClasses :: [Id]
numClasses =
  Id <$> ["Num", "Integral", "Floating", "Fractional", "Real", "RealFloat", "RealFrac"]

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
  ] ++
  numClasses

candidates :: ClassEnv -> Ambiguity -> [Type]
candidates ce (v, qs) =
  [ t'
  | let is = [i | IsIn i _ <- qs]
        ts = [t | IsIn _ t <- qs]
  , all ([TVar v] ==) ts
  , any (`elem` numClasses) is
  , all (`elem` stdClasses) is
  , t' <- defaults ce
  , all (entail ce []) [IsIn i [t'] | i <- is]
  ]

withDefaults ::
     Monad m
  => ([Ambiguity] -> [Type] -> a)
  -> ClassEnv
  -> [Tyvar]
  -> [Pred]
  -> m a
withDefaults f ce vs ps
  | any null tss = fail "cannot resolve ambiguity"
  | otherwise = pure (f vps (map (fromJust . head) tss))
  where
    vps = ambiguities vs ps
    tss = map (candidates ce) vps

defaultedPreds :: Monad m => ClassEnv -> [Tyvar] -> [Pred] -> m [Pred]
defaultedPreds = withDefaults (\vps _ -> concatMap snd vps)

defaultSubst :: Monad m => ClassEnv -> [Tyvar] -> [Pred] -> m Subst
defaultSubst = withDefaults (\vps ts -> zip (map fst vps) ts)

tiProgram :: ClassEnv -> [Assump] -> TIModule -> [Assump]
tiProgram ce as mod =
  let bgs = extractBindingGroups mod
   in runTI $ do
        (ps, as') <- tiSeq tiBindGroup ce as bgs
        doTiProgram ce as' ps

tiBindGroup' :: ClassEnv -> [Assump] -> BindGroup' -> TI ([Pred], [Assump])
tiBindGroup' ce as bs = do
  (ps, as') <- tiBindGroup ce as bs
  trim (ftv (as' ++ as))
  pure (ps, as')

extractBindingGroups :: TIModule -> [BindGroup']
extractBindingGroups (Module decls) =
  let unwrapBgDecl (BGDecl bg) = bg
      isBgDecl (BGDecl _) = True
      isBgDecl _          = False
      bgs = unwrapBgDecl <$> filter isBgDecl decls
   in bgs

tiProgram' :: ClassEnv -> [Assump] -> TIModule -> [Assump]
tiProgram' ce as mod =
  let bgs = extractBindingGroups mod
   in runTI $ do
        (ps, as') <- tiSeq tiBindGroup' ce as bgs
        doTiProgram ce as' ps

doTiProgram :: ClassEnv -> [Assump] -> [Pred] -> TI [Assump]
doTiProgram ce as' ps = do
  s <- get
  let rs = reduce ce (apply s ps)
  s' <- defaultSubst ce [] rs
  pure (apply (s' @@ s) as')


quantify :: [Tyvar] -> Qual Type -> Scheme
quantify vs qt = Forall ks (apply s qt)
  where
    vs' = [v | v <- ftv qt, v `elem` vs]
    ks = map kind vs'
    s = zip vs' (map TGen [0 ..])
-}
