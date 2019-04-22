
module X1.Parser.Expr1 ( parser ) where

import Protolude
import X1.Types.Expr1
import X1.Parser.Helpers
import qualified X1.Parser.Lit as Lit


parser :: Parser Expr1
parser =  E1Lit <$> Lit.parser
      <|> ifParser


ifParser :: Parser Expr1
ifParser = ifParser' <?> "if expression" where
  ifParser' = withLineFold $ do
    keyword "if"
    cond <- lexeme' parser
    keyword "then"
    trueClause <- lexeme' parser
    keyword "else"
    E1If cond trueClause <$> parser

