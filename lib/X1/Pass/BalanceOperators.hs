
{-# LANGUAGE UndecidableInstances #-}

module X1.Pass.BalanceOperators ( BalanceError(..), FixityInfo(..), pass ) where

{-
Algorithm based on: https://github.com/haskell/haskell-report/blob/master/report/fixity.verb

Since the parser here emits an AST directly, we convert to tokens,
then fix the precedence of the operators in the AST and finally
convert the tokens back to an AST again.
-}

import Protolude hiding ( Fixity, pass )
import qualified Data.List as List
import Control.Parallel.Strategies
import X1.Types.Expr1.Module
import X1.Types.Expr1.Impl
import X1.Types.Expr1.Expr
import X1.Types.Fixity
import X1.Types.Id
import X1.Types.Ann


type Module' = Module 'Parsed
type Decl' = Decl 'Parsed
type Binding' = Binding 'Parsed
type Expr1' = Expr1 'Parsed
type ExprDecl' = ExprDecl 'Parsed
type Ann' = Ann 'Parsed
type BalanceError' = BalanceError 'Parsed

data BalanceError ph
  = BadPrecedence FixityInfo FixityInfo (Decl ph)
  | InvalidPrefixPrecedence FixityInfo (Decl ph)

deriving instance Eq (Decl a) => Eq (BalanceError a)
deriving instance Show (Decl a) => Show (BalanceError a)

data FixityInfo = FI Fixity Int Id
  deriving (Eq, Show)

data Token = TExpr Expr1'
           | TOp (Id -> Expr1' -> Expr1' -> Expr1') FixityInfo
           | TNeg Ann'

data Env = Env { envFixities :: [FixityInfo]
               , envDecl :: Decl'
               }

type RebalanceM m = ReaderT Env (ExceptT BalanceError' m)


pass :: Monad m => Module' -> ExceptT BalanceError' m Module'
pass (Module decls) =
  let fixities = map toFixityInfo $ filter isFixityDecl decls
      isFixityDecl FixityDecl {} = True
      isFixityDecl _ = False
      toFixityInfo (FixityDecl _ fixity prec op) = FI fixity prec op
      toFixityInfo _ = panic "Error while computing operator precedences."
      parMap' = parMap rpar
   in Module <$> sequenceA (parMap' (runRebalance fixities) decls)


runRebalance :: Monad m => [FixityInfo] -> Decl' -> ExceptT BalanceError' m Decl'
runRebalance fsSpecs decl = runReaderT (rebalance decl) env
  where env = Env fsSpecs decl

class Balance a where
  rebalance :: Monad m => a -> RebalanceM m a

instance Balance a => Balance [a] where
  rebalance = traverse rebalance

instance Balance b => Balance (a, b) where
  rebalance = traverse rebalance

instance Balance Decl' where
  rebalance (ImplDecl (Impl preds p bindings)) =
    ImplDecl . Impl preds p <$> rebalance bindings
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

instance Balance Expr1' where
  rebalance expr = do
    tokens <- toTokens expr
    rebalancedExpr <- fst <$> rebalanceTokens startOp tokens
    rebalanceInner rebalancedExpr
    where
      startOp = FI M (-1) (Id "startOp")
      -- Bin op is already rebalanced, only do the rest (inner layers of AST).
      rebalanceInner (E1BinOp ann op e1 e2) =
        E1BinOp ann op <$> rebalanceInner e1 <*> rebalanceInner e2
      rebalanceInner (E1Parens ann e) = E1Parens ann <$> rebalance e
      rebalanceInner (E1Lam ann vars body) = E1Lam ann vars <$> rebalance body
      rebalanceInner (E1App ann f args) =
        E1App ann <$> rebalance f <*> rebalance args
      rebalanceInner (E1Case e clauses) =
        E1Case <$> rebalance e <*> rebalance clauses
      rebalanceInner (E1If cond tClause fClause) =
        E1If <$> rebalance cond <*> rebalance tClause <*> rebalance fClause
      rebalanceInner (E1Neg ann e) = E1Neg ann <$> rebalance e
      rebalanceInner (E1Let decls body) =
        E1Let <$> rebalance decls <*> rebalance body
      rebalanceInner e = pure e

lookupFixity :: [FixityInfo] -> Id -> FixityInfo
lookupFixity fsSpecs op =
  let result = List.find (\(FI _ _ op') -> op == op') fsSpecs
      defaultFixity = FI L 9 op
   in maybe defaultFixity identity result

toBinOp :: Ann' -> (Id -> Expr1') -> Id -> (Expr1' -> Expr1' -> Expr1')
toBinOp opAnn f opName = E1BinOp opAnn (f opName)

toTokens :: Monad m => Expr1' -> RebalanceM m [Token]
toTokens (E1BinOp opAnn (E1Var ann op) e1 e2) =
  opToTokens (toBinOp opAnn (E1Var ann)) op e1 e2
toTokens (E1BinOp opAnn (E1Con ann op) e1 e2) =
  opToTokens (toBinOp opAnn (E1Con ann)) op e1 e2
toTokens (E1Neg ann e) = pure [TNeg ann, TExpr e]
toTokens e = pure [TExpr e]

opToTokens :: Monad m
           => (Id -> Expr1' -> Expr1' -> Expr1')
           -> Id -> Expr1' -> Expr1' -> RebalanceM m [Token]
opToTokens f op e1 e2 = do
  fsSpecs <- asks envFixities
  let fs = lookupFixity fsSpecs op
  e1Tokens <- toTokens e1
  e2Tokens <- toTokens e2
  pure $ e1Tokens <> [TOp f fs] <> e2Tokens

rebalanceTokens :: Monad m => FixityInfo -> [Token] -> RebalanceM m (Expr1', [Token])
rebalanceTokens op1 (TExpr e1 : rest) = rebalanceTokens' op1 e1 rest
rebalanceTokens op1 (TNeg ann : rest) = do
  when (prec1 >= 6) $ throwError . InvalidPrefixPrecedence op1 =<< asks envDecl
  (r, rest') <- rebalanceTokens negateOp rest
  rebalanceTokens' op1 (E1Neg ann r) rest'
  where
    negateOp = FI L 6 (Id "-")
    FI _ prec1 _  = op1
rebalanceTokens _ _ = panic "Error while rebalancing tokens!"

rebalanceTokens' :: Monad m => FixityInfo -> Expr1' -> [Token] -> RebalanceM m (Expr1', [Token])
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
    rebalanceTokens' op1 ((f operator) e1 r) rest'

  where
    FI fix1 prec1 _ = op1
    FI fix2 prec2 operator = op2
rebalanceTokens' _ _ _ = panic "Error while rebalancing tokens!"

