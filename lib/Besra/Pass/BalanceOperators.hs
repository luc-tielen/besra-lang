
{-# LANGUAGE UndecidableInstances #-}

module Besra.Pass.BalanceOperators ( BalanceError(..), FixitySpec(..), pass ) where

{-
Algorithm based on: https://github.com/haskell/haskell-report/blob/master/report/fixity.verb

Since the parser here emits an AST directly, we convert to tokens,
then fix the precedence of the operators in the AST and finally
convert the tokens back to an AST again.
-}

import Protolude hiding ( Fixity, pass )
import qualified Data.List as List
import Control.Parallel.Strategies
import Besra.Types.IR1.Module
import Besra.Types.IR1.Impl
import Besra.Types.IR1.Expr
import Besra.Types.Fixity
import Besra.Types.Id
import Besra.Types.Ann


type Module' = Module 'Parsed
type Decl' = Decl 'Parsed
type Binding' = Binding 'Parsed
type Expr' = Expr 'Parsed
type ExprDecl' = ExprDecl 'Parsed
type Ann' = Ann 'Parsed
type BalanceError' = BalanceError 'Parsed

data BalanceError ph
  = BadPrecedence FixitySpec FixitySpec (Decl ph)
  | InvalidPrefixPrecedence FixitySpec (Decl ph)

deriving instance Eq (Decl a) => Eq (BalanceError a)
deriving instance Show (Decl a) => Show (BalanceError a)

data FixitySpec = FI Fixity Int Id
  deriving (Eq, Show)

data Token = TExpr Expr'
           | TOp (Id -> Expr' -> Expr' -> Expr') FixitySpec
           | TNeg Ann'

data Env = Env { envFixities :: [FixitySpec]
               , envDecl :: Decl'
               }

type RebalanceM m = ReaderT Env (ExceptT BalanceError' m)


pass :: Monad m => Module' -> ExceptT BalanceError' m Module'
pass (Module decls) =
  let fixities = map toFixitySpec $ filter isFixityDecl decls
      isFixityDecl FixityDecl {} = True
      isFixityDecl _ = False
      toFixitySpec (FixityDecl (FixityInfo _ fixity prec op)) = FI fixity prec op
      toFixitySpec _ = panic "Error while computing operator precedences."
      parMap' = parMap rpar
   in Module <$> sequenceA (parMap' (runRebalance fixities) decls)

runRebalance :: Monad m => [FixitySpec] -> Decl' -> ExceptT BalanceError' m Decl'
runRebalance fsSpecs decl = runReaderT (rebalance decl) env
  where env = Env fsSpecs decl

class Balance a where
  rebalance :: Monad m => a -> RebalanceM m a

instance Balance a => Balance [a] where
  rebalance = traverse rebalance

instance Balance b => Balance (a, b) where
  rebalance = traverse rebalance

instance Balance Decl' where
  rebalance (ImplDecl (Impl ann preds p bindings)) =
    ImplDecl . Impl ann preds p <$> rebalance bindings
  rebalance (BindingDecl (Binding ann id expr)) =
    BindingDecl . Binding ann id <$> rebalance expr
  rebalance d = pure d

instance Balance ExprDecl' where
  rebalance (ExprBindingDecl binding) =
    ExprBindingDecl <$> rebalance binding
  rebalance d = pure d

instance Balance Binding' where
  rebalance (Binding ann id expr) =
    Binding ann id <$> rebalance expr

instance Balance Expr' where
  rebalance expr = do
    tokens <- toTokens expr
    rebalancedExpr <- fst <$> rebalanceTokens startOp tokens
    rebalanceInner rebalancedExpr
    where
      -- Bin op is already rebalanced, only do the rest (inner layers of AST).
      rebalanceInner (EBinOp ann op e1 e2) =
        EBinOp ann op <$> rebalanceInner e1 <*> rebalanceInner e2
      rebalanceInner (EParens ann e) = EParens ann <$> rebalance e
      rebalanceInner (ELam ann vars body) = ELam ann vars <$> rebalance body
      rebalanceInner (EApp ann f args) =
        EApp ann <$> rebalance f <*> rebalance args
      rebalanceInner (ECase ann e clauses) =
        ECase ann <$> rebalance e <*> rebalance clauses
      rebalanceInner (EIf ann cond tClause fClause) =
        EIf ann <$> rebalance cond <*> rebalance tClause <*> rebalance fClause
      rebalanceInner (ENeg ann e) = ENeg ann <$> rebalance e
      rebalanceInner (ELet ann decls body) = do
        let fixities' = map toFixitySpec $ filter isFixityDecl decls
        local (updateFixities fixities') $
          ELet ann <$> rebalance decls <*> rebalance body
      rebalanceInner e = pure e
      isFixityDecl ExprFixityDecl {} = True
      isFixityDecl _ = False
      toFixitySpec (ExprFixityDecl (FixityInfo _ fixity prec op)) = FI fixity prec op
      toFixitySpec _ = panic "Error while computing operator precedences."

startOp :: FixitySpec
startOp = FI M (-1) (Id "startOp")

updateFixities :: [FixitySpec] -> Env -> Env
updateFixities newFixities env =
  env { envFixities = newFixities ++ envFixities env }

lookupFixity :: [FixitySpec] -> Id -> FixitySpec
lookupFixity fsSpecs op =
  let result = List.find (\(FI _ _ op') -> op == op') fsSpecs
      defaultFixity = FI L 9 op
   in maybe defaultFixity identity result

toBinOp :: Ann' -> (Id -> Expr') -> Id -> (Expr' -> Expr' -> Expr')
toBinOp opAnn f opName = EBinOp opAnn (f opName)

toTokens :: Monad m => Expr' -> RebalanceM m [Token]
toTokens (EBinOp opAnn (EVar ann op) e1 e2) =
  opToTokens (toBinOp opAnn (EVar ann)) op e1 e2
toTokens (EBinOp opAnn (ECon ann op) e1 e2) =
  opToTokens (toBinOp opAnn (ECon ann)) op e1 e2
toTokens (ENeg ann e) = pure [TNeg ann, TExpr e]
toTokens e = pure [TExpr e]

opToTokens :: Monad m
           => (Id -> Expr' -> Expr' -> Expr')
           -> Id -> Expr' -> Expr' -> RebalanceM m [Token]
opToTokens f op e1 e2 = do
  fsSpecs <- asks envFixities
  let fs = lookupFixity fsSpecs op
  e1Tokens <- toTokens e1
  e2Tokens <- toTokens e2
  pure $ e1Tokens <> [TOp f fs] <> e2Tokens

rebalanceTokens :: Monad m => FixitySpec -> [Token] -> RebalanceM m (Expr', [Token])
rebalanceTokens op1 (TExpr e1 : rest) = rebalanceTokens' op1 e1 rest
rebalanceTokens op1 (TNeg ann : rest) = do
  when (prec1 >= 6) $ throwError . InvalidPrefixPrecedence op1 =<< asks envDecl
  (r, rest') <- rebalanceTokens negateOp rest
  rebalanceTokens' op1 (ENeg ann r) rest'
  where
    negateOp = FI L 6 (Id "-")
    FI _ prec1 _  = op1
rebalanceTokens _ _ = panic "Error while rebalancing tokens!"

rebalanceTokens' :: Monad m => FixitySpec -> Expr' -> [Token] -> RebalanceM m (Expr', [Token])
rebalanceTokens' _ e1 [] = pure (e1, [])
rebalanceTokens' op1 e1 (TOp f op2 : rest)
  -- case (1): check for illegal expressions
  | prec1 == prec2 && (fix1 /= fix2 || fix1 == M) =
    throwError . BadPrecedence op1 op2 =<< asks envDecl

  -- case (2): op1 and op2 should associate to the left
  | prec1 > prec2 || (prec1 == prec2 && fix1 == L) =
    pure (e1, TOp f op2 : rest)

  -- case (3): op1 and op2 should associate to the right
  | otherwise = do
    (r, rest') <- rebalanceTokens op2 rest
    rebalanceTokens' op1 (f operator e1 r) rest'

  where
    FI fix1 prec1 _ = op1
    FI fix2 prec2 operator = op2
rebalanceTokens' _ _ _ = panic "Error while rebalancing tokens!"

