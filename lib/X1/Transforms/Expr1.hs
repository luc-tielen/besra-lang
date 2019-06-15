
module X1.Transforms.Expr1 ( Transform(..)
                           , Tag(..)
                           , transform
                           ) where

import Protolude
import X1.Types.Expr1.Module
import X1.Types.Expr1.Expr
import X1.Types.Expr1.Impl


data Tag a where
  TagE :: Tag Expr1
  TagB :: Tag Binding
  TagED :: Tag ExprDecl
  TagD :: Tag Decl

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


instance (Traversable f, Transform a) => Transform (f a) where
  transformM prf f = traverse (transformM prf f)

instance Transform Module where
  transformM prf f (Module decls) =
    Module <$> transformM prf f decls

instance Transform Decl where
  transformM TagD _ d = pure d
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
  transformM prf f (Impl ps p bindings) =
    Impl ps p <$> transformM prf f bindings

instance Transform Binding where
  transformM TagD _ b = pure b
  transformM TagB _ b = pure b
  transformM prf f (Binding name expr) =
    Binding name <$> transformM prf f expr

instance Transform ExprDecl where
  transformM TagD _ ed = pure ed
  transformM TagED _ ed = pure ed
  transformM prf f ed =
    case ed of
      ExprBindingDecl binding ->
        ExprBindingDecl <$> transformM prf f binding
      ExprTypeAnnDecl {} -> pure ed
      ExprFixityDecl {} -> pure ed

instance Transform Expr1 where
  transformM TagD _ e = pure e
  transformM TagE f e =
    case e of
      E1Lit {} -> pure e
      E1Var {} -> pure e
      E1Con {} -> pure e
      E1Lam vars body -> E1Lam vars <$> f TagE body
      E1App func args -> E1App <$> f TagE func <*> transformM TagE f args
      E1BinOp op l r -> E1BinOp <$> f TagE op <*> f TagE l <*> f TagE r
      E1Neg expr -> E1Neg <$> f TagE expr
      E1If cond tr fl -> E1If <$> f TagE cond <*> f TagE tr <*> f TagE fl
      E1Case cond clauses ->
        E1Case <$> f TagE cond <*> transformM TagE f clauses
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
        E1Let <$> transformM prf f decls <*> transformM prf f expr
      E1Parens expr -> E1Parens <$> transformM prf f expr

