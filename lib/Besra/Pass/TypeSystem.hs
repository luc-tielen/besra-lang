
-- TODO remove
{-# OPTIONS_GHC -Wno-deprecations #-}

module Besra.Pass.TypeSystem
  ( pass
  , Error(..)
  ) where

import Protolude hiding ( pass)
import Data.Bitraversable ( bitraverse )
import Besra.Types.Ann
import Besra.TypeSystem.Error
import Besra.TypeSystem.Subst
import Besra.TypeSystem.Infer
import Besra.TypeSystem.TypeClass
import Besra.Types.CompilerState
import qualified Besra.Types.IR3 as IR3

type KI = KindInferred

pass :: Monad m
     => CompilerState KI
     -> IR3.Module KI
     -> ExceptT Error m (IR3.Module PostTC)
pass _ m =
  let m' = prepareAST m  -- TODO take traits/impls into account
      traitEnv = initialEnv
      initialAssumps = []
      result = tiProgram traitEnv initialAssumps m'
   in case result of
     Left err -> throwError err
     Right solution ->
       -- TODO add types to expr nodes in AST
       pure $ runReader (solveTypes m') solution


-- TODO remove this workaround pass after bug is fixed in typechecker.
--
-- This is done separately so the "PreTC" AST stays internally in this pass
-- (has different Eq behavior).
-- This pass performs no real work except for updating all phase tags in the AST.
class PrepareAST a where
  type Res a

  prepareAST :: a -> Res a

instance PrepareAST a => PrepareAST [a] where
  type Res [a] = [Res a]

  prepareAST = map prepareAST

instance PrepareAST (IR3.Module KI) where
  type Res (IR3.Module KI) = IR3.Module PreTC

  prepareAST (IR3.Module decls) =
    IR3.Module (prepareAST decls)

instance PrepareAST (IR3.Explicit KI) where
  type Res (IR3.Explicit KI) = IR3.Explicit PreTC

  prepareAST (IR3.Explicit name sch alts) =
    IR3.Explicit name (prepareAST sch) (prepareAST alts)

instance PrepareAST (IR3.Implicit KI) where
  type Res (IR3.Implicit KI) = IR3.Implicit PreTC

  prepareAST (IR3.Implicit name alts) =
    IR3.Implicit name (prepareAST alts)

instance PrepareAST (IR3.Scheme KI) where
  type Res (IR3.Scheme KI) = IR3.Scheme PreTC

  prepareAST (IR3.ForAll ann ks (ps IR3.:=> a)) =
    IR3.ForAll ann ks (prepareAST ps IR3.:=> prepareAST a)

instance PrepareAST (IR3.Pred KI) where
  type Res (IR3.Pred KI) = IR3.Pred PreTC

  prepareAST (IR3.IsIn ann name ts) =
    IR3.IsIn ann name (prepareAST ts)

instance PrepareAST (IR3.Type KI) where
  type Res (IR3.Type KI) = IR3.Type PreTC

  prepareAST = \case
    IR3.TVar var -> IR3.TVar (prepareAST var)
    IR3.TCon con -> IR3.TCon (prepareAST con)
    IR3.TApp t1 t2 -> IR3.TApp (prepareAST t1) (prepareAST t2)
    IR3.TGen x -> IR3.TGen x

instance PrepareAST (IR3.Tyvar KI) where
  type Res (IR3.Tyvar KI) = IR3.Tyvar PreTC

  prepareAST (IR3.Tyvar ann v) = IR3.Tyvar ann v

instance PrepareAST (IR3.Tycon KI) where
  type Res (IR3.Tycon KI) = IR3.Tycon PreTC

  prepareAST (IR3.Tycon ann v) = IR3.Tycon ann v

instance (PrepareAST a, PrepareAST b) => PrepareAST (a, b) where
  type Res (a, b) = (Res a, Res b)

  prepareAST = bimap prepareAST prepareAST

instance PrepareAST (IR3.Expr KI) where
  type Res (IR3.Expr KI) = IR3.Expr PreTC

  prepareAST = \case
    IR3.ELit ann lit -> IR3.ELit ann lit
    IR3.EVar ann var -> IR3.EVar ann var
    IR3.ECon ann con sch -> IR3.ECon ann con (prepareAST sch)
    IR3.ELam ann alt -> IR3.ELam ann (prepareAST alt)
    IR3.EApp ann e1 e2 -> IR3.EApp ann (prepareAST e1) (prepareAST e2)
    IR3.EIf ann c t f -> IR3.EIf ann (prepareAST c) (prepareAST t) (prepareAST f)
    IR3.ECase ann e clauses -> IR3.ECase ann (prepareAST e) (prepareAST clauses)
    IR3.ELet ann bg e -> IR3.ELet ann (prepareAST bg) (prepareAST e)

instance PrepareAST (IR3.Pattern KI) where
  type Res (IR3.Pattern KI) = IR3.Pattern PreTC

  prepareAST = \case
    IR3.PWildcard ann -> IR3.PWildcard ann
    IR3.PLit ann lit -> IR3.PLit ann lit
    IR3.PVar ann var -> IR3.PVar ann var
    IR3.PCon ann name sch pats -> IR3.PCon ann name (prepareAST sch) (prepareAST pats)
    IR3.PAs ann name pat -> IR3.PAs ann name (prepareAST pat)

-- TODO remove this once bidirectional typechecking is implemented
-- or directly transform to IR4 here
class SolveTypes a where
  type Result a

  solveTypes :: a -> Reader Subst (Result a)

instance SolveTypes a => SolveTypes [a] where
  type Result [a] = [Result a]

  solveTypes = traverse solveTypes

instance SolveTypes (IR3.Module PreTC) where
  type Result (IR3.Module PreTC) = IR3.Module PostTC

  solveTypes (IR3.Module decls) =
    IR3.Module <$> solveTypes decls

instance SolveTypes (IR3.Explicit PreTC) where
  type Result (IR3.Explicit PreTC) = IR3.Explicit PostTC

  solveTypes (IR3.Explicit name sch alts) =
    IR3.Explicit name <$> solveTypes sch <*> solveTypes alts

instance SolveTypes (IR3.Implicit PreTC) where
  type Result (IR3.Implicit PreTC) = IR3.Implicit PostTC

  solveTypes (IR3.Implicit name alts) =
    IR3.Implicit name <$> solveTypes alts

instance SolveTypes (IR3.Scheme PreTC) where
  type Result (IR3.Scheme PreTC) = IR3.Scheme PostTC

  solveTypes (IR3.ForAll ann ks (ps IR3.:=> a)) = do
    ps' <- solveTypes ps
    a' <- solveTypes a
    pure $ IR3.ForAll ann ks (ps' IR3.:=> a')

instance SolveTypes (IR3.Pred PreTC) where
  type Result (IR3.Pred PreTC) = IR3.Pred PostTC

  solveTypes (IR3.IsIn ann name ts) =
    IR3.IsIn ann name <$> solveTypes ts

instance SolveTypes (IR3.Type PreTC) where
  type Result (IR3.Type PreTC) = IR3.Type PostTC

  solveTypes t = do
    subst <- ask
    let t' = apply subst t
    pure $ convert t'
    where
      convert = \case
        IR3.TVar (IR3.Tyvar ann var) -> IR3.TVar (IR3.Tyvar ann var)
        IR3.TCon (IR3.Tycon ann con) -> IR3.TCon (IR3.Tycon ann con)
        IR3.TApp t1 t2 -> IR3.TApp (convert t1) (convert t2)
        IR3.TGen x -> IR3.TGen x

instance (SolveTypes a, SolveTypes b) => SolveTypes (a, b) where
  type Result (a, b) = (Result a, Result b)

  solveTypes = bitraverse solveTypes solveTypes

instance SolveTypes (IR3.Expr PreTC) where
  type Result (IR3.Expr PreTC) = IR3.Expr PostTC

  solveTypes = \case
    IR3.ELit ann lit -> pure $ IR3.ELit ann lit
    IR3.EVar ann var -> pure $ IR3.EVar ann var
    IR3.ECon ann con sch -> IR3.ECon ann con <$> solveTypes sch
    IR3.ELam ann alt -> IR3.ELam ann <$> solveTypes alt
    IR3.EApp ann e1 e2 -> IR3.EApp ann <$> solveTypes e1 <*> solveTypes e2
    IR3.EIf ann c t f -> IR3.EIf ann <$> solveTypes c <*> solveTypes t <*> solveTypes f
    IR3.ECase ann e clauses -> IR3.ECase ann <$> solveTypes e <*> solveTypes clauses
    IR3.ELet ann bg e -> IR3.ELet ann <$> solveTypes bg <*> solveTypes e

instance SolveTypes (IR3.Pattern PreTC) where
  type Result (IR3.Pattern PreTC) = IR3.Pattern PostTC

  solveTypes = \case
    IR3.PWildcard ann -> pure $ IR3.PWildcard ann
    IR3.PLit ann lit -> pure $ IR3.PLit ann lit
    IR3.PVar ann var -> pure $ IR3.PVar ann var
    IR3.PCon ann name sch pats -> IR3.PCon ann name <$> solveTypes sch <*> solveTypes pats
    IR3.PAs ann name pat -> IR3.PAs ann name <$> solveTypes pat
