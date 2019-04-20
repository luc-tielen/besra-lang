
module X1.Parser.Scheme ( parser ) where

import Protolude hiding ( try, pred )
import X1.Parser.Helpers
import qualified X1.Parser.Pred as Pred
import qualified X1.Parser.Type as Type
import X1.Parser.Types.Scheme
import X1.Parser.Types.Pred
import qualified Text.Megaparsec.Char.Lexer as L


parser :: Parser () -> Parser Scheme
parser ws' = parser' <?> "typescheme" where
  lexeme' = L.lexeme ws'
  parser' = try schemeWithPredicates <|> schemeWithoutPredicates
  schemeWithoutPredicates = Scheme [] <$> Type.parser ws'
  schemeWithPredicates = do
    preds <- predicates ws'
    lexeme' . void $ chunk "=>"
    Scheme preds <$> Type.parser ws'

predicates :: Parser () -> Parser [Pred]
predicates ws' =
  lexeme' $ nPredicates <|> singlePredicate
    where
      lexeme' = L.lexeme ws'
      nPredicates = betweenParens $ predParser `sepBy1` comma
      singlePredicate = pure <$> betweenOptionalParens predParser
      predParser = lexeme' $ betweenParens predParser <|> Pred.parser ws'
      comma = lexeme' $ char ','

