
module Besra.Parser.Pattern ( parser ) where

import Protolude
import Besra.Parser.Helpers
import Besra.Types.IR1.Pattern
import Besra.Types.Id
import qualified Besra.Parser.Lit as Lit


parser :: Parser Pattern
parser = parser' <?> "pattern" where
  parser' =  wildcardPattern
         <|> literalPattern
         <|> varOrAsPattern
         <|> conPattern

wildcardPattern :: Parser Pattern
wildcardPattern = PWildcard <$ char '_' <* notFollowedBy (letterChar <|> digitChar)

literalPattern :: Parser Pattern
literalPattern = PLit <$> Lit.parser

conPattern :: Parser Pattern
conPattern = singleCon <|> multiCon where
  conParser = Id <$> lexeme capitalIdentifier <?> "constructor"
  multiCon = sameLine $ betweenParens $ do
    con <- conParser
    PCon con <$> many (lexeme parser)
  singleCon = flip PCon [] <$> conParser

varOrAsPattern :: Parser Pattern
varOrAsPattern = do
  var <- Id <$> identifier
  asBinding <- optional (char '@' <?> "pattern")
  case asBinding of
    Nothing -> pure $ PVar var
    Just _ -> PAs var <$> parser

