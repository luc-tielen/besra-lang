
module X1.Parser.Scheme ( parser, predicatesPrefix ) where

import Protolude hiding ( try, pred )
import X1.Parser.Helpers
import qualified X1.Parser.Pred as Pred
import qualified X1.Parser.Type as Type
import X1.Types.Expr1.Scheme
import X1.Types.Expr1.Pred
import X1.Types.Ann


type Scheme' = Scheme 'Parsed
type Pred' = Pred 'Parsed

parser :: Parser Scheme'
parser = parser' <?> "typescheme" where
  parser' = Scheme <$> predicatesPrefix <*> Type.parser

predicatesPrefix :: Parser [Pred']
predicatesPrefix = withDefault [] $ try prefix
  where prefix = predicates <* lexeme' (chunk "=>")

predicates :: Parser [Pred']
predicates =
  lexeme' $ nPredicates <|> singlePredicate
    where
      nPredicates = betweenParens $ predParser `sepBy1` comma
      singlePredicate = pure <$> betweenOptionalParens predParser
      predParser = lexeme' $ betweenParens predParser <|> Pred.parser
      comma = lexeme' $ char ','

