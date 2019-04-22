
module X1.Parser.Scheme ( parser ) where

import Protolude hiding ( try, pred )
import X1.Parser.Helpers
import qualified X1.Parser.Pred as Pred
import qualified X1.Parser.Type as Type
import X1.Parser.Types.Scheme
import X1.Parser.Types.Pred


parser :: Parser Scheme
parser = parser' <?> "typescheme" where
  parser' = try schemeWithPredicates <|> schemeWithoutPredicates
  schemeWithoutPredicates = Scheme [] <$> Type.parser
  schemeWithPredicates = do
    preds <- predicates
    lexeme' . void $ chunk "=>"
    Scheme preds <$> Type.parser

predicates :: Parser [Pred]
predicates =
  lexeme' $ nPredicates <|> singlePredicate
    where
      nPredicates = betweenParens $ predParser `sepBy1` comma
      singlePredicate = pure <$> betweenOptionalParens predParser
      predParser = lexeme' $ betweenParens predParser <|> Pred.parser
      comma = lexeme' $ char ','

