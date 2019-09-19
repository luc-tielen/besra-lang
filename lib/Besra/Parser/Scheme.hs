
module Besra.Parser.Scheme ( parser, predicatesPrefix ) where

import Protolude hiding ( try, pred )
import Besra.Parser.Helpers
import qualified Besra.Parser.Pred as Pred
import qualified Besra.Parser.Type as Type
import Besra.Types.IR1 ( Scheme(..), Pred(..) )
import Besra.Types.Ann
import Besra.Types.Span


type Scheme' = Scheme Parsed
type Pred' = Pred Parsed

parser :: Parser Scheme'
parser = parser' <?> "typescheme" where
  parser' = do
    startPos <- getOffset
    ps <- predicatesPrefix
    ty <- Type.parser
    pure $ Scheme (startPos .> span ty) ps ty

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

