
module X1.Parser.Type ( parser ) where

import Protolude hiding ( Type, try )
import qualified X1.Parser.Tycon as Tycon
import qualified X1.Parser.Tyvar as Tyvar
import X1.Types.Expr1.Type
import X1.Parser.Helpers
import X1.Types.Id
import Control.Monad.Combinators.Expr


-- TODO: upgrade the parsing of types for better precedence handling

parser :: Parser Type
parser = typeExpr

typeExpr :: Parser Type
typeExpr = makeExprParser typeTerm typeOperators <?> "type"

typeTerm :: Parser Type
typeTerm = computeType <$> typeParser' <?> "type"
  where
    typeParser' = typeParser `sepBy1` whitespace'

typeParser :: Parser Type
typeParser =
  lexeme $  betweenParens typeExpr
        <|> concreteType
        <|> typeVar
  where
    concreteType = TCon <$> Tycon.parser
    typeVar = TVar <$> Tyvar.parser

typeOperators :: [[Operator Parser Type]]
typeOperators =
  [ [ InfixR (arrow <$ keyword "->") ] ]
  where
    arrow :: Type -> Type -> Type
    arrow t1 t2 = TApp (TCon (Tycon (Id "->"))) [t1, t2]

computeType :: [Type] -> Type
computeType [] = panic "Parse error when parsing type signature."
computeType [t] = t
computeType (t:ts) = TApp t ts

