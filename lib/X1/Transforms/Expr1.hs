
module X1.Transforms.Expr1 ( Transform(..)
                           , Fold(..)
                           , Tag(..)
                           , transform
                           ) where

import Protolude hiding ( Fixity )
import X1.Types.Expr1.Module
import X1.Types.Expr1.Expr
import X1.Types.Expr1.Impl
import X1.Types.Expr1.TypeAnn
import X1.Types.Expr1.Trait
import X1.Types.Expr1.ADT
import X1.Types.Expr1.Pred
import X1.Types.Expr1.Lit
import X1.Types.Expr1.Pattern
import X1.Types.Fixity
import X1.Types.Id


data Tag a where
  TagD :: Tag Decl
  TagE :: Tag Expr1
  TagED :: Tag ExprDecl
  TagB :: Tag Binding

-- | Typeclass for performing an update in the AST without changing to another IR.
--   Passes a function along with a type level witness in order to update the AST
--   in a typesafe way. The function itself will call transform(M) again for all
--   cases that should stay the same.
--
--   The reasoning behind the algorithm is as follows:
--   1. Tag may contain references further down the tree => recurse deeper into AST
--   2. Tag will not appear further down the tree => stop recursion
--   3. Tag is used at same level update should happen
--      => apply function, but only in places where "recursion" of that type occurs
--   In situation 3, the function will be only applied at 1 level, the function
--   is itself responsible to call the transform(M) function again.
--
--   Algorithm mostly based on/extended from:
--   http://www.cse.chalmers.se/alumni/bringert/publ/composOp-jfp/composOp-jfp.pdf
class Transform a where
  transformM :: Applicative f => Tag b -> (Tag b -> b -> f b) -> a -> f a

-- | Non-monadic variant of the transformM function.
transform :: Transform a => Tag b -> (Tag b -> b -> b) -> a -> a
transform prf f a = runIdentity $ transformM prf ((pure .) . f) a


-- NOTE: use this instance as little as possible, since it skips
-- the behavior of "f" for the top level items in that list.
-- Instead, if something that has a "proof value" appears in the list,
-- calls traverse (f proof) directly instead of transformM.
instance (Traversable f, Transform a) => Transform (f a) where
  transformM prf f = traverse (transformM prf f)

instance Transform Module where
  transformM TagD f (Module decls) =
    Module <$> traverse (f TagD) decls
  transformM prf f (Module decls) =
    Module <$> transformM prf f decls

instance Transform Decl where
  transformM TagD _ d = pure d
  transformM TagB f d =
    case d of
      BindingDecl binding -> BindingDecl <$> f TagB binding
      _ -> pure d
  transformM prf f d =
    case d of
      ImplDecl impl -> ImplDecl <$> transformM prf f impl
      BindingDecl binding -> BindingDecl <$> transformM prf f binding
      -- The following contain nothing to recurse on
      -- (just use TagD to target these.)
      TraitDecl {} -> pure d  -- TODO needs support later (default functions)
      TypeAnnDecl {} -> pure d
      DataDecl {} -> pure d
      FixityDecl {} -> pure d

instance Transform Impl where
  transformM TagD _ impl = pure impl
  transformM TagB f (Impl ps p bindings) =
    Impl ps p <$> traverse (f TagB) bindings
  transformM prf f (Impl ps p bindings) =
    Impl ps p <$> transformM prf f bindings

instance Transform Binding where
  transformM TagD _ b = pure b
  transformM TagE f (Binding name expr) =
    Binding name <$> f TagE expr
  transformM prf f (Binding name expr) =
    Binding name <$> transformM prf f expr

instance Transform ExprDecl where
  transformM TagD _ ed = pure ed
  transformM TagB f ed =
    case ed of
      ExprBindingDecl binding -> ExprBindingDecl <$> f TagB binding
      _ -> pure ed
  transformM prf f ed =
    case ed of
      ExprBindingDecl binding ->
        ExprBindingDecl <$> transformM prf f binding
      _ -> pure ed

instance Transform Expr1 where
  transformM TagD _ e = pure e
  transformM TagE f e =
    case e of
      E1Lit {} -> pure e
      E1Var {} -> pure e
      E1Con {} -> pure e
      E1Lam vars body -> E1Lam vars <$> f TagE body
      E1App func args -> E1App <$> f TagE func <*> traverse (f TagE) args
      E1BinOp op l r -> E1BinOp <$> f TagE op <*> f TagE l <*> f TagE r
      E1Neg expr -> E1Neg <$> f TagE expr
      E1If cond tr fl -> E1If <$> f TagE cond <*> f TagE tr <*> f TagE fl
      E1Case cond clauses ->
        E1Case <$> f TagE cond <*> traverse (traverse (f TagE)) clauses
      E1Let decls expr -> E1Let <$> transformM TagE f decls <*> f TagE expr
      E1Parens expr -> E1Parens <$> f TagE expr
  transformM prf f e =
    case e of
      E1Lit {} -> pure e
      E1Var {} -> pure e
      E1Con {} -> pure e
      E1Lam vars body -> E1Lam vars <$> transformM prf f body
      E1App func args ->
        E1App <$> transformM prf f func <*> transformM prf f args
      E1BinOp op l r ->
        E1BinOp <$> transformM prf f op
                <*> transformM prf f l
                <*> transformM prf f r
      E1Neg expr -> E1Neg <$> transformM prf f expr
      E1If cond tr fl ->
        E1If <$> transformM prf f cond
             <*> transformM prf f tr
             <*> transformM prf f fl
      E1Case cond clauses ->
        E1Case <$> transformM prf f cond
               <*> transformM prf f clauses
      E1Let decls expr ->
        case prf of
          TagED ->
            E1Let <$> traverse (f TagED) decls <*> transformM prf f expr
          _ ->
            E1Let <$> transformM prf f decls <*> transformM prf f expr
      E1Parens expr -> E1Parens <$> transformM prf f expr


type HandlersMM m rModule rDecl =
  [rDecl] -> m rModule  -- Handler for module

type HandlersDM m rDecl rImpl rBinding rExpr =
  ( TypeAnn -> m rDecl              -- Handler for type ann decls
  , ADT -> m rDecl                  -- Handler for data decls
  , Trait -> m rDecl                -- Handler for trait decls
  , rImpl -> m rDecl                -- Handler for impl decls
  , rBinding -> m rDecl             -- Handler for binding decls
  , Fixity -> Int -> Id -> m rDecl  -- Handler for fixity decls
  )

type HandlersIM m rImpl rBinding =
  [Pred] -> Pred -> [rBinding] -> m rImpl  -- Handler for impl decls

type HandlersBM m rBinding rExpr =
  Id -> rExpr -> m rBinding  -- Handler for bindings

type HandlersEDM m rBinding rExprDecl rExpr =
  ( TypeAnn -> m rExprDecl             -- handler for expr type ann decl
  , rBinding -> m rExprDecl            -- Handler for expr binding decl
  , Fixity -> Int -> Id -> m rExprDecl -- Handler for expr fixity decl
  )

type HandlersEM m rBinding rExprDecl rExpr =
  ( Lit -> m rExpr                          -- Handler for literals
  , Id -> m rExpr                           -- Handler for vars
  , Id -> m rExpr                           -- Handler for constructors
  , [Pattern] -> rExpr -> m rExpr           -- Handler for lambda expression
  , rExpr -> [rExpr] -> m rExpr             -- Handler for function application
  , rExpr -> rExpr -> rExpr -> m rExpr      -- Handler for bin op expression
  , rExpr -> m rExpr                        -- Handler for negate expression
  , rExpr -> rExpr -> rExpr -> m rExpr      -- Handler for if expression
  , rExpr -> [(Pattern, rExpr)] -> m rExpr  -- Handler for case expression
  , [rExprDecl] -> rExpr -> m rExpr         -- Handler for let expression
  , rExpr -> m rExpr                        -- Handler for parenthesized expression
  )

type HandlersM m rModule rDecl rImpl rBinding rExprDecl rExpr =
  ( HandlersMM m rModule rDecl               -- Handlers for module
  , HandlersDM m rDecl rImpl rBinding rExpr  -- Handlers for decls
  , HandlersIM m rImpl rBinding              -- Handlers for impls
  , HandlersBM m rBinding rExpr              -- Handlers for bindings
  , HandlersEDM m rBinding rExprDecl rExpr   -- Handlers for decls in exprs
  , HandlersEM m rBinding rExprDecl rExpr    -- Handlers for exprs
  )


-- | Typeclass for performing a fold over the entire AST. This can be used to
--   change from one IR to another, to update annotations, ...
--   Takes a tuple containing functions for each of the possible node types
--   in the AST. The typeclass makes sure that each of the handlers is called
--   on the correct nodes.
--   By making clever use of type variables and type families,
--   no packing/unpacking of types is needed. For each type that is handled
--   specially, add a type variable so that the intermediary results
--   can be forwarded to other handlers.
class Fold a where
  type Result a rM rD rI rB rED rE :: Type

  foldAST :: Monad m
          => HandlersM m rM rD rI rB rED rE
          -> a
          -> m (Result a rM rD rI rB rED rE)

instance Fold a => Fold [a] where
  type Result [a] rM rD rI rB rED rE = [Result a rM rD rI rB rED rE]

  foldAST fs = traverse (foldAST fs)

instance Fold b => Fold (a, b) where
  type Result (a, b) rM rD rI rB rED rE = (a, Result b rM rD rI rB rED rE)

  foldAST fs = traverse (foldAST fs)

instance Fold Module where
  type Result Module rM _ _ _ _ _ = rM

  foldAST fs@(f, _, _, _, _, _) (Module decls) =
    f =<< foldAST fs decls

instance Fold Decl where
  type Result Decl _ rDecl _ _ _ _ = rDecl

  foldAST (_, (f, _, _, _, _, _), _, _, _, _) (TypeAnnDecl typeAnn) =
    f typeAnn
  foldAST (_, (_, f, _, _, _, _), _, _, _, _) (DataDecl adt) =
    f adt
  foldAST (_, (_, _, f, _, _, _), _, _, _, _) (TraitDecl trait) =
    f trait
  foldAST fs@(_, (_, _, _, f, _, _), _, _, _, _) (ImplDecl impl) =
    f =<< foldAST fs impl
  foldAST fs@(_, (_, _, _, _, f, _), _, _, _, _) (BindingDecl binding) =
    f =<< foldAST fs binding
  foldAST (_, (_, _, _, _, _, f), _, _, _, _) (FixityDecl fixity prec var) =
    f fixity prec var

instance Fold Impl where
  type Result Impl _ _ rImpl _ _ _ = rImpl

  foldAST fs@(_, _, f, _, _, _) (Impl ps p bindings) =
    f ps p =<< foldAST fs bindings

instance Fold Binding where
  type Result Binding _ _ _ rBinding _ _ = rBinding

  foldAST fs@(_, _, _, f, _, _) (Binding var expr) =
    f var =<< foldAST fs expr

instance Fold ExprDecl where
  type Result ExprDecl _ _ _ _ rExprDecl _ = rExprDecl

  foldAST (_, _, _, _, (f, _, _), _) (ExprTypeAnnDecl typeAnn) = f typeAnn
  foldAST fs@(_, _, _, _, (_, f, _), _) (ExprBindingDecl binding) =
    f =<< foldAST fs binding
  foldAST (_, _, _, _, (_, _, f), _) (ExprFixityDecl fixity prec var) =
    f fixity prec var

instance Fold Expr1 where
  type Result Expr1 _ _ _ _ _ rExpr = rExpr

  foldAST (_, _, _, _, _, (f, _, _, _, _, _, _, _, _, _, _)) (E1Lit lit) = f lit
  foldAST (_, _, _, _, _, (_, f, _, _, _, _, _, _, _, _, _)) (E1Var var) = f var
  foldAST (_, _, _, _, _, (_, _, f, _, _, _, _, _, _, _, _)) (E1Con con) = f con
  foldAST fs@(_, _, _, _, _, (_, _, _, f, _, _, _, _, _, _, _)) (E1Lam pats body) =
    f pats =<< foldAST fs body
  foldAST fs@(_, _, _, _, _, (_, _, _, _, f, _, _, _, _, _, _)) (E1App func args) =
    join $ f <$> foldAST fs func <*> foldAST fs args
  foldAST fs@(_, _, _, _, _, (_, _, _, _, _, f, _, _, _, _, _)) (E1BinOp op l r) =
    join $ f <$> foldAST fs op <*> foldAST fs l <*> foldAST fs r
  foldAST fs@(_, _, _, _, _, (_, _, _, _, _, _, f, _, _, _, _)) (E1Neg e) =
    f =<< foldAST fs e
  foldAST fs@(_, _, _, _, _, (_, _, _, _, _, _, _, f, _, _, _)) (E1If c tr fl) =
    join $ f <$> foldAST fs c <*> foldAST fs tr <*> foldAST fs fl
  foldAST fs@(_, _, _, _, _, (_, _, _, _, _, _, _, _, f, _, _)) (E1Case e clauses) =
    join $ f <$> foldAST fs e <*> foldAST fs clauses
  foldAST fs@(_, _, _, _, _, (_, _, _, _, _, _, _, _, _, f, _)) (E1Let decls body) =
    join $ f <$> foldAST fs decls <*> foldAST fs body
  foldAST fs@(_, _, _, _, _, (_, _, _, _, _, _, _, _, _, _, f)) (E1Parens e) =
    f =<< foldAST fs e

