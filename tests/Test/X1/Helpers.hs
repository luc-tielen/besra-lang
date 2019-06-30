
module Test.X1.Helpers ( module Test.X1.Helpers ) where

import Protolude
import X1.Transforms.Expr1
import X1.Types.Expr1.Module
import X1.Types.Expr1.Impl
import X1.Types.Expr1.Expr
import X1.Types.Ann


type Transform =
  Handlers Identity 'Parsed
    (Module 'Testing) (Decl 'Testing) (Impl 'Testing)
    (Binding 'Testing) (ExprDecl 'Testing) (Expr1 'Testing)

type Result a ph =
  FoldResult a (Module ph) (Decl ph) (Impl ph)
               (Binding ph) (ExprDecl ph) (Expr1 ph)

stripAnns :: Fold a => a 'Parsed -> Result a 'Testing
stripAnns ast =
  let fs :: Transform
      fs = Handlers { handlersM = fsM
                    , handlersD = fsD
                    , handlersI = fsI
                    , handlersB = fsB
                    , handlersED = fsED
                    , handlersE = fsE
                    }
      fsM = HandlersM { moduleM = pure . Module }
      fsD = HandlersD { typeAnnD = pure . TypeAnnDecl
                      , adtD = pure . DataDecl
                      , traitD = pure . TraitDecl
                      , implD = pure . ImplDecl
                      , bindingD = pure . BindingDecl
                      , fixityD = stripFixityDeclAnn
                      }
      fsI = HandlersI { implI = \ps p bs -> pure $ Impl ps p bs }
      fsB = HandlersB { bindingB = stripBindingAnn }
      fsED = HandlersED { typeAnnED = pure . ExprTypeAnnDecl
                        , bindingED = pure . ExprBindingDecl
                        , fixityED = stripExprFixityDeclAnn
                        }
      fsE = HandlersE { litE = stripLitAnn
                      , varE = stripVarAnn
                      , conE = stripConAnn
                      , lamE = stripLamAnn
                      , appE = stripAppAnn
                      , binOpE = stripBinOpAnn
                      , negE = stripNegAnn
                      , ifE = stripIfAnn
                      , caseE = \e cs -> pure $ E1Case e cs
                      , letE = \decls body -> pure $ E1Let decls body
                      , parenE = stripParenAnn
                      }
      stripExprFixityDeclAnn _ fx prec name = pure $ ExprFixityDecl emptyAnn fx prec name
      stripFixityDeclAnn _ fx prec name = pure $ FixityDecl emptyAnn fx prec name
      stripBindingAnn _ name expr = pure $ Binding emptyAnn name expr
      stripLitAnn _ lit = pure $ E1Lit emptyAnn lit
      stripVarAnn _ var = pure $ E1Var emptyAnn var
      stripConAnn _ con = pure $ E1Con emptyAnn con
      stripNegAnn _ e = pure $ E1Neg emptyAnn e
      stripBinOpAnn _ op l r = pure $ E1BinOp emptyAnn op l r
      stripParenAnn _ e = pure $ E1Parens emptyAnn e
      stripLamAnn _ pats body = pure $ E1Lam emptyAnn pats body
      stripAppAnn _ func args = pure $ E1App emptyAnn func args
      stripIfAnn _ c tr fl = pure $ E1If emptyAnn c tr fl
   in runIdentity $ foldAST fs ast


emptyAnn :: Ann 'Testing
emptyAnn = ()

