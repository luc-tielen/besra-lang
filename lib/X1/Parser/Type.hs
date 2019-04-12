
module X1.Parser.Type ( parser ) where

import Protolude hiding ( Type, try )
import Control.Monad ( fail )
import qualified X1.Parser.Tycon as Tycon
import qualified X1.Parser.Tyvar as Tyvar
import X1.Parser.Types.Type
import X1.Parser.Helpers
import X1.Types
import Control.Monad.Combinators.Expr


-- TODO: upgrade the parsing of types for better precedence handling

parser :: Parser Type
parser = typeExpr

typeExpr :: Parser Type
typeExpr = typeExpr' <?> "type"
  where
    typeExpr' = makeExprParser typeTerm typeOperators

typeTerm :: Parser Type
typeTerm = computeType =<< typeParser <?> "type"
  where
    typeParser = some $ lexeme typeParser'
    typeParser' = betweenParens typeExpr <|> concreteType <|> typeVar
    concreteType = TCon <$> Tycon.parser
    typeVar = TVar <$> Tyvar.parser

typeOperators :: [[Operator Parser Type]]
typeOperators =
  [ [ InfixR (arrow <$ keyword "->") ] ]
  where
    arrow :: Type -> Type -> Type
    arrow t1 t2 = TApp (TCon (Tycon (Id "->"))) [t1, t2]

computeType :: [Type] -> Parser Type
computeType [] = fail "Parse error when parsing type signature."
computeType [t] = pure t
computeType (t:ts) = pure $ TApp t ts

