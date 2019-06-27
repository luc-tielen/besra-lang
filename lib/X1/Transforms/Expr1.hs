
module X1.Transforms.Expr1 ( Compos(..)
                           , Proof(..)
                           , compos
                           , Fold(..)
                           , FoldResult
                           , HandlersE(..)
                           , HandlersED(..)
                           , HandlersB(..)
                           , HandlersI(..)
                           , HandlersD(..)
                           , HandlersM(..)
                           , Handlers(..)
                           ) where

import Protolude hiding ( Fixity )
import Data.Default
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
import X1.Types.Ann


data Proof a where
  ProofD :: Proof Decl
  ProofE :: Proof Expr1
  ProofED :: Proof ExprDecl
  ProofB :: Proof Binding

-- | Typeclass for performing an update in the AST without changing to another IR.
--   Passes a function along with a type level witness in order to update the AST
--   in a typesafe way. The function itself will call compos(M) again for all
--   cases that should stay the same.
--
--   The reasoning behind the algorithm is as follows:
--   1. Proof may contain references further down the tree => recurse deeper into AST
--   2. Proof will not appear further down the tree => stop recursion
--   3. Proof is used at same level update should happen
--      => apply function, but only in places where "recursion" of that type occurs
--   In situation 3, the function will be only applied at 1 level, the function
--   is itself responsible to call the compos(M) function again.
--
--   Algorithm mostly based on/extended from:
--   http://www.cse.chalmers.se/alumni/bringert/publ/composOp-jfp/composOp-jfp.pdf
class Compos a where
  composM :: Applicative f => Proof b -> (Proof b -> b -> f b) -> a -> f a

-- | Non-monadic variant of the composM function.
compos :: Compos a => Proof b -> (Proof b -> b -> b) -> a -> a
compos prf f a = runIdentity $ composM prf ((pure .) . f) a


-- NOTE: use this instance as little as possible, since it skips
-- the behavior of "f" for the top level items in that list.
-- Instead, if something that has a "proof value" appears in the list,
-- calls traverse (f proof) directly instead of composM.
instance (Traversable f, Compos a) => Compos (f a) where
  composM prf f = traverse (composM prf f)

instance Compos Module where
  composM ProofD f (Module decls) =
    Module <$> traverse (f ProofD) decls
  composM prf f (Module decls) =
    Module <$> composM prf f decls

instance Compos Decl where
  composM ProofD _ d = pure d
  composM ProofB f d =
    case d of
      BindingDecl binding -> BindingDecl <$> f ProofB binding
      _ -> pure d
  composM prf f d =
    case d of
      ImplDecl impl -> ImplDecl <$> composM prf f impl
      BindingDecl binding -> BindingDecl <$> composM prf f binding
      -- The following contain nothing to recurse on
      -- (just use ProofD to target these.)
      TraitDecl {} -> pure d  -- TODO needs support later (default functions)
      TypeAnnDecl {} -> pure d
      DataDecl {} -> pure d
      FixityDecl {} -> pure d

instance Compos Impl where
  composM ProofD _ impl = pure impl
  composM ProofB f (Impl ps p bindings) =
    Impl ps p <$> traverse (f ProofB) bindings
  composM prf f (Impl ps p bindings) =
    Impl ps p <$> composM prf f bindings

instance Compos Binding where
  composM ProofD _ b = pure b
  composM ProofE f (Binding name expr) =
    Binding name <$> f ProofE expr
  composM prf f (Binding name expr) =
    Binding name <$> composM prf f expr

instance Compos ExprDecl where
  composM ProofD _ ed = pure ed
  composM ProofB f ed =
    case ed of
      ExprBindingDecl binding -> ExprBindingDecl <$> f ProofB binding
      _ -> pure ed
  composM prf f ed =
    case ed of
      ExprBindingDecl binding ->
        ExprBindingDecl <$> composM prf f binding
      _ -> pure ed

instance Compos Expr1 where
  composM ProofD _ e = pure e
  composM ProofE f e =
    case e of
      E1Lit {} -> pure e
      E1Var {} -> pure e
      E1Con {} -> pure e
      E1Lam vars body -> E1Lam vars <$> f ProofE body
      E1App func args -> E1App <$> f ProofE func <*> traverse (f ProofE) args
      E1BinOp ann op l r -> E1BinOp ann <$> f ProofE op <*> f ProofE l <*> f ProofE r
      E1Neg expr -> E1Neg <$> f ProofE expr
      E1If cond tr fl -> E1If <$> f ProofE cond <*> f ProofE tr <*> f ProofE fl
      E1Case cond clauses ->
        E1Case <$> f ProofE cond <*> traverse (traverse (f ProofE)) clauses
      E1Let decls expr -> E1Let <$> composM ProofE f decls <*> f ProofE expr
      E1Parens ann expr -> E1Parens ann <$> f ProofE expr
  composM prf f e =
    case e of
      E1Lit {} -> pure e
      E1Var {} -> pure e
      E1Con {} -> pure e
      E1Lam vars body -> E1Lam vars <$> composM prf f body
      E1App func args ->
        E1App <$> composM prf f func <*> composM prf f args
      E1BinOp ann op l r ->
        E1BinOp ann <$> composM prf f op
                    <*> composM prf f l
                    <*> composM prf f r
      E1Neg expr -> E1Neg <$> composM prf f expr
      E1If cond tr fl ->
        E1If <$> composM prf f cond
             <*> composM prf f tr
             <*> composM prf f fl
      E1Case cond clauses ->
        E1Case <$> composM prf f cond
               <*> composM prf f clauses
      E1Let decls expr ->
        case prf of
          ProofED ->
            E1Let <$> traverse (f ProofED) decls <*> composM prf f expr
          _ ->
            E1Let <$> composM prf f decls <*> composM prf f expr
      E1Parens ann expr -> E1Parens ann <$> composM prf f expr


newtype HandlersM m rModule rDecl =
  HandlersM
    { moduleM :: [rDecl] -> m rModule  -- Handler for module
    }

data HandlersD m rDecl rImpl rBinding rExpr =
  HandlersD
    { typeAnnD :: TypeAnn -> m rDecl             -- Handler for type ann decls
    , adtD :: ADT -> m rDecl                     -- Handler for data decls
    , traitD :: Trait -> m rDecl                 -- Handler for trait decls
    , implD :: rImpl -> m rDecl                  -- Handler for impl decls
    , bindingD :: rBinding -> m rDecl            -- Handler for binding decls
    , fixityD :: Fixity -> Int -> Id -> m rDecl  -- Handler for fixity decls
    }

newtype HandlersI m rImpl rBinding =
  HandlersI
    { implI :: [Pred] -> Pred -> [rBinding] -> m rImpl  -- Handler for impl decls
    }

newtype HandlersB m rBinding rExpr =
  HandlersB
    { bindingB :: Id -> rExpr -> m rBinding  -- Handler for bindings
    }

data HandlersED m rBinding rExprDecl rExpr =
  HandlersED
    { typeAnnED :: TypeAnn -> m rExprDecl             -- handler for expr type ann decl
    , bindingED :: rBinding -> m rExprDecl            -- Handler for expr binding decl
    , fixityED :: Fixity -> Int -> Id -> m rExprDecl  -- Handler for expr fixity decl
    }

data HandlersE m rExprDecl rExpr =
  HandlersE
    { litE :: Ann -> Lit -> m rExpr                        -- Handler for literals
    , varE :: Ann -> Id -> m rExpr                         -- Handler for vars
    , conE :: Ann -> Id -> m rExpr                         -- Handler for constructors
    , lamE :: [Pattern] -> rExpr -> m rExpr                -- Handler for lambda expression
    , appE :: rExpr -> [rExpr] -> m rExpr                  -- Handler for function application
    , binOpE :: Ann -> rExpr -> rExpr -> rExpr -> m rExpr  -- Handler for bin op expression
    , negE :: rExpr -> m rExpr                             -- Handler for negate expression
    , ifE :: rExpr -> rExpr -> rExpr -> m rExpr            -- Handler for if expression
    , caseE :: rExpr -> [(Pattern, rExpr)] -> m rExpr      -- Handler for case expression
    , letE :: [rExprDecl] -> rExpr -> m rExpr              -- Handler for let expression
    , parenE :: Ann -> rExpr -> m rExpr                    -- Handler for parenthesized expression
    }

data Handlers m rModule rDecl rImpl rBinding rExprDecl rExpr =
  Handlers
    { handlersM  :: HandlersM m rModule rDecl               -- Handlers for module
    , handlersD  :: HandlersD m rDecl rImpl rBinding rExpr  -- Handlers for decls
    , handlersI  :: HandlersI m rImpl rBinding              -- Handlers for impls
    , handlersB  :: HandlersB m rBinding rExpr              -- Handlers for bindings
    , handlersED :: HandlersED m rBinding rExprDecl rExpr   -- Handlers for decls in exprs
    , handlersE  :: HandlersE m rExprDecl rExpr             -- Handlers for exprs
    }


instance Applicative m => Default (HandlersM m Module Decl) where
  def = HandlersM $ pure . Module

instance Applicative m
  => Default (HandlersD m Decl Impl Binding Expr1) where
  def = HandlersD (pure . TypeAnnDecl)
                  (pure . DataDecl)
                  (pure . TraitDecl)
                  (pure . ImplDecl)
                  (pure . BindingDecl)
                  (\f pr name -> pure $ FixityDecl f pr name)

instance Applicative m => Default (HandlersI m Impl Binding) where
  def = HandlersI (\ps p bindings -> pure $ Impl ps p bindings)

instance Applicative m => Default (HandlersB m Binding Expr1) where
  def = HandlersB (\name expr -> pure $ Binding name expr)

instance Applicative m
  => Default (HandlersE m ExprDecl Expr1) where
  def = HandlersE (\ann lit -> pure $ E1Lit ann lit)
                  (\ann var -> pure $ E1Var ann var)
                  (\ann con -> pure $ E1Con ann con)
                  (\pats body -> pure $ E1Lam pats body)
                  (\func args -> pure $ E1App func args)
                  (\ann op l r -> pure $ E1BinOp ann op l r)
                  (pure . E1Neg)
                  (\c tr fl -> pure $ E1If c tr fl)
                  (\e clauses -> pure $ E1Case e clauses)
                  (\decls body -> pure $ E1Let decls body)
                  (\ann e -> pure $ E1Parens ann e)

instance Applicative m
  => Default (HandlersED m Binding ExprDecl Expr1) where
  def = HandlersED (pure . ExprTypeAnnDecl)
                   (pure . ExprBindingDecl)
                   (\fixity prec var -> pure $ ExprFixityDecl fixity prec var)

instance Applicative m
  => Default (Handlers m Module Decl Impl Binding ExprDecl Expr1) where
  def = Handlers def def def def def def


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
  foldAST :: Monad m
          => Handlers m rM rD rI rB rED rE
          -> a
          -> m (FoldResult a rM rD rI rB rED rE)

type family FoldResult a rM rD rI rB rED rE where
  FoldResult [a] rM rD rI rB rED rE = [FoldResult a rM rD rI rB rED rE]
  FoldResult (a, b) rM rD rI rB rED rE = (a, FoldResult b rM rD rI rB rED rE)
  FoldResult Module   rM _ _ _ _ _ = rM
  FoldResult Decl     _ rD _ _ _ _ = rD
  FoldResult Impl     _ _ rI _ _ _ = rI
  FoldResult Binding  _ _ _ rB _ _ = rB
  FoldResult ExprDecl _ _ _ _ rED _ = rED
  FoldResult Expr1    _ _ _ _ _ rE = rE

instance Fold a => Fold [a] where
  foldAST fs = traverse (foldAST fs)

instance Fold b => Fold (a, b) where
  foldAST fs = traverse (foldAST fs)

instance Fold Module where
  foldAST fs (Module decls) =
    moduleM (handlersM fs) =<< foldAST fs decls

instance Fold Decl where
  foldAST fs d =
    let fs' = handlersD fs
     in case d of
        TypeAnnDecl typeAnn -> typeAnnD fs' typeAnn
        DataDecl adt -> adtD fs' adt
        TraitDecl trait -> traitD fs' trait
        ImplDecl impl -> implD fs' =<< foldAST fs impl
        BindingDecl binding -> bindingD fs' =<< foldAST fs binding
        FixityDecl fixity prec var -> fixityD fs' fixity prec var

instance Fold Impl where
  foldAST fs (Impl ps p bindings) =
    let fs' = handlersI fs
     in implI fs' ps p =<< foldAST fs bindings

instance Fold Binding where
  foldAST fs (Binding var expr) =
    let fs' = handlersB fs
     in bindingB fs' var =<< foldAST fs expr

instance Fold ExprDecl where
  foldAST fs ed =
    let fs' = handlersED fs
     in case ed of
       ExprTypeAnnDecl typeAnn -> typeAnnED fs' typeAnn
       ExprBindingDecl binding -> bindingED fs' =<< foldAST fs binding
       ExprFixityDecl fixity prec var -> fixityED fs' fixity prec var

instance Fold Expr1 where
  foldAST fs expr =
    let fs' = handlersE fs
     in case expr of
       E1Lit ann lit -> litE fs' ann lit
       E1Var ann var -> varE fs' ann var
       E1Con ann con -> conE fs' ann con
       E1Lam pats body -> lamE fs' pats =<< foldAST fs body
       E1App func args ->
         join $ appE fs' <$> foldAST fs func <*> foldAST fs args
       E1BinOp ann op l r ->
         join $ binOpE fs' ann <$> foldAST fs op <*> foldAST fs l <*> foldAST fs r
       E1Neg e -> negE fs' =<< foldAST fs e
       E1If c tr fl ->
         join $ ifE fs' <$> foldAST fs c <*> foldAST fs tr <*> foldAST fs fl
       E1Case e clauses ->
         join $ caseE fs' <$> foldAST fs e <*> foldAST fs clauses
       E1Let decls body ->
         join $ letE fs' <$> foldAST fs decls <*> foldAST fs body
       E1Parens ann e ->
         parenE fs' ann =<< foldAST fs e

