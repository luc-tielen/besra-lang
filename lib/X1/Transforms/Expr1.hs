
module X1.Transforms.Expr1 ( Fold(..)
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


newtype HandlersM m rModule rDecl =
  HandlersM
    { moduleM :: [rDecl] -> m rModule  -- Handler for module
    }

data HandlersD m ph rDecl rImpl rBinding rExpr =
  HandlersD
    { typeAnnD :: TypeAnn -> m rDecl                       -- Handler for type ann decls
    , adtD :: ADT -> m rDecl                               -- Handler for data decls
    , traitD :: Trait -> m rDecl                           -- Handler for trait decls
    , implD :: rImpl -> m rDecl                            -- Handler for impl decls
    , bindingD :: rBinding -> m rDecl                      -- Handler for binding decls
    , fixityD :: Ann ph -> Fixity -> Int -> Id -> m rDecl  -- Handler for fixity decls
    }

newtype HandlersI m rImpl rBinding =
  HandlersI
    { implI :: [Pred] -> Pred -> [rBinding] -> m rImpl  -- Handler for impl decls
    }

newtype HandlersB m ph rBinding rExpr =
  HandlersB
    { bindingB :: Ann ph -> Id -> rExpr -> m rBinding  -- Handler for bindings
    }

data HandlersED m ph rBinding rExprDecl rExpr =
  HandlersED
    { typeAnnED :: TypeAnn -> m rExprDecl                       -- handler for expr type ann decl
    , bindingED :: rBinding -> m rExprDecl                      -- Handler for expr binding decl
    , fixityED :: Ann ph -> Fixity -> Int -> Id -> m rExprDecl  -- Handler for expr fixity decl
    }

data HandlersE m ph rExprDecl rExpr =
  HandlersE
    { litE :: Ann ph -> Lit -> m rExpr                           -- Handler for literals
    , varE :: Ann ph -> Id -> m rExpr                            -- Handler for vars
    , conE :: Ann ph -> Id -> m rExpr                            -- Handler for constructors
    , lamE :: Ann ph -> [Pattern] -> rExpr -> m rExpr            -- Handler for lambda expression
    , appE :: Ann ph -> rExpr -> [rExpr] -> m rExpr              -- Handler for function application
    , binOpE :: Ann ph -> rExpr -> rExpr -> rExpr -> m rExpr     -- Handler for bin op expression
    , negE :: Ann ph -> rExpr -> m rExpr                         -- Handler for negate expression
    , ifE :: Ann ph -> rExpr -> rExpr -> rExpr -> m rExpr        -- Handler for if expression
    , caseE :: Ann ph -> rExpr -> [(Pattern, rExpr)] -> m rExpr  -- Handler for case expression
    , letE :: Ann ph -> [rExprDecl] -> rExpr -> m rExpr          -- Handler for let expression
    , parenE :: Ann ph -> rExpr -> m rExpr                       -- Handler for parenthesized expression
    }

data Handlers m ph rModule rDecl rImpl rBinding rExprDecl rExpr =
  Handlers
    { handlersM  :: HandlersM m rModule rDecl                  -- Handlers for module
    , handlersD  :: HandlersD m ph rDecl rImpl rBinding rExpr  -- Handlers for decls
    , handlersI  :: HandlersI m rImpl rBinding                 -- Handlers for impls
    , handlersB  :: HandlersB m ph rBinding rExpr              -- Handlers for bindings
    , handlersED :: HandlersED m ph rBinding rExprDecl rExpr   -- Handlers for decls in exprs
    , handlersE  :: HandlersE m ph rExprDecl rExpr             -- Handlers for exprs
    }


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
          => Handlers m ph rM rD rI rB rED rE
          -> a ph
          -> m (FoldResult a rM rD rI rB rED rE)

type family FoldResult (a :: Phase -> Type) rM rD rI rB rED rE where
  FoldResult Module   rM _ _ _ _ _ = rM
  FoldResult Decl     _ rD _ _ _ _ = rD
  FoldResult Impl     _ _ rI _ _ _ = rI
  FoldResult Binding  _ _ _ rB _ _ = rB
  FoldResult ExprDecl _ _ _ _ rED _ = rED
  FoldResult Expr1    _ _ _ _ _ rE = rE

type family FoldResult' a rM rD rI rB rED rE where
  FoldResult' (a, b ph) rM rD rI rB rED rE = (a, FoldResult' (b ph) rM rD rI rB rED rE)
  FoldResult' [a ph] rM rD rI rB rED rE = [FoldResult' (a ph) rM rD rI rB rED rE]
  FoldResult' (a _) rM rD rI rB rED rE = FoldResult a rM rD rI rB rED rE


instance Fold Module where
  foldAST fs (Module decls) =
    moduleM (handlersM fs) =<< traverse (foldAST fs) decls

instance Fold Decl where
  foldAST fs d =
    let fs' = handlersD fs
     in case d of
        TypeAnnDecl typeAnn -> typeAnnD fs' typeAnn
        DataDecl adt -> adtD fs' adt
        TraitDecl trait -> traitD fs' trait
        ImplDecl impl -> implD fs' =<< foldAST fs impl
        BindingDecl binding -> bindingD fs' =<< foldAST fs binding
        FixityDecl ann fixity prec var -> fixityD fs' ann fixity prec var

instance Fold Impl where
  foldAST fs (Impl ps p bindings) =
    let fs' = handlersI fs
     in implI fs' ps p =<< traverse (foldAST fs) bindings

instance Fold Binding where
  foldAST fs (Binding ann var expr) =
    let fs' = handlersB fs
     in bindingB fs' ann var =<< foldAST fs expr

instance Fold ExprDecl where
  foldAST fs ed =
    let fs' = handlersED fs
     in case ed of
       ExprTypeAnnDecl typeAnn -> typeAnnED fs' typeAnn
       ExprBindingDecl binding -> bindingED fs' =<< foldAST fs binding
       ExprFixityDecl ann fixity prec var -> fixityED fs' ann fixity prec var

instance Fold Expr1 where
  foldAST fs expr =
    let fs' = handlersE fs
     in case expr of
       E1Lit ann lit -> litE fs' ann lit
       E1Var ann var -> varE fs' ann var
       E1Con ann con -> conE fs' ann con
       E1Lam ann pats body -> lamE fs' ann pats =<< foldAST fs body
       E1App ann func args ->
         join $ appE fs' ann <$> foldAST fs func <*> traverse (foldAST fs) args
       E1BinOp ann op l r ->
         join $ binOpE fs' ann <$> foldAST fs op <*> foldAST fs l <*> foldAST fs r
       E1Neg ann e -> negE fs' ann =<< foldAST fs e
       E1If ann c tr fl ->
         join $ ifE fs' ann <$> foldAST fs c <*> foldAST fs tr <*> foldAST fs fl
       E1Case ann e clauses ->
         join $ caseE fs' ann <$> foldAST fs e <*> traverse (traverse (foldAST fs)) clauses
       E1Let ann decls body ->
         join $ letE fs' ann <$> traverse (foldAST fs) decls <*> foldAST fs body
       E1Parens ann e ->
         parenE fs' ann =<< foldAST fs e

