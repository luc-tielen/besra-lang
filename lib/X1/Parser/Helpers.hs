
module X1.Parser.Helpers ( Parser
                         , ParseErr
                         , ParseError
                         , ParseResult
                         , lexeme
                         , between
                         , betweenParens
                         , singleQuote
                         , digitChar
                         , hexDigitChar
                         , binDigitChar
                         , keyword
                         , identifier
                         , capitalIdentifier
                         , char
                         , oneOf
                         , notFollowedBy
                         , satisfy
                         , try
                         , (<?>)
                         ) where

import Protolude hiding (many, first, try)
import qualified Data.Text as T
import qualified Text.Megaparsec.Char.Lexer as L (lexeme, skipBlockComment,
                                                  skipLineComment, space)
import Text.Megaparsec hiding (ParseError)
import qualified Text.Megaparsec as P (ParseErrorBundle)
import Text.Megaparsec.Char (digitChar, lowerChar, upperChar)


type ParseErr = Void
type Parser = Parsec ParseErr Text
type ParseError = P.ParseErrorBundle Text ParseErr
type ParseResult = Either ParseError


lexeme :: Parser a -> Parser a
lexeme = L.lexeme whitespace

whitespace :: Parser ()
whitespace = L.space spaceParser commentParser blockCommentParser where
  spaceParser = skipSome wsChar

wsChar :: Parser ()
wsChar = void (oneOf [' ', '\n'] <?> "whitespace")

commentParser :: Parser ()
commentParser = L.skipLineComment "--"

blockCommentParser :: Parser ()
blockCommentParser = L.skipBlockComment "{-" "-}"

betweenParens :: Parser a -> Parser a
betweenParens = between (lexeme $ char '(') (char ')') . lexeme

char :: Char -> Parser Char
char = single

hexDigitChar :: Parser Char
hexDigitChar = oneOf hexChars <?> "hex digit" where
  hexChars = ['0'..'9'] ++ ['a'..'f'] ++ ['A'..'F']

binDigitChar :: Parser Char
binDigitChar = oneOf ['0', '1'] <?> "binary digit"

keyword :: Text -> Parser ()
keyword s = lexeme (chunk s <* lookAhead wsChar) $> ()

identifier :: Parser Text
identifier = do
  -- TODO forbid keywords
  firstChar <- lowerChar
  rest <- many (identifierChar <?> "rest of identifier" )
  pure $ T.pack $ [firstChar] <> rest

capitalIdentifier :: Parser Text
capitalIdentifier = do
  firstChar <- upperChar
  rest <- many (identifierChar <?> "rest of identifier" )
  pure $ T.pack $ [firstChar] <> rest

identifierChar :: Parser Char
identifierChar =  lowerChar
              <|> upperChar
              <|> digitChar
              <|> singleQuote

singleQuote :: Parser Char
singleQuote = char '\''

