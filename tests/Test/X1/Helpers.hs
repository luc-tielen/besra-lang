
module Test.X1.Helpers ( module Test.X1.Helpers ) where

import Protolude
import X1.Transforms.Expr1
import Data.Default ( def )
import X1.Types.Expr1.Module
import X1.Types.Expr1.Impl
import X1.Types.Expr1.Expr
import X1.Types.Ann


type Transform =
  Handlers Identity Module Decl Impl Binding ExprDecl Expr1

emptyAnn :: Ann
emptyAnn = Ann TagT ()

stripAnns :: Fold a => a -> Result a Module Decl Impl Binding ExprDecl Expr1
stripAnns ast =
  let fs :: Transform
      fs = def { handlersE = def { litE = stripLitAnn
                                 , varE = stripVarAnn
                                 , parenE = stripParenAnn
                                 }
               }
      stripParenAnn _ e = pure $ E1Parens emptyAnn e
      stripLitAnn _ lit = pure $ E1Lit emptyAnn lit
      stripVarAnn _ var = pure $ E1Var emptyAnn var
   in runIdentity $ foldAST fs ast

