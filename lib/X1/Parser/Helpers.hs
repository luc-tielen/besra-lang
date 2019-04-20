
module X1.Parser.Helpers ( Parser
                         , ParseErr
                         , ParseError
                         , ParseResult
                         , Lexeme
                         , lexeme
                         , whitespace
                         , withLineFold
                         , eof
                         , between
                         , betweenParens
                         , betweenOptionalParens
                         , singleQuote
                         , digitChar
                         , hexDigitChar
                         , binDigitChar
                         , keyword
                         , chunk
                         , identifier
                         , capitalIdentifier
                         , char
                         , oneOf
                         , notFollowedBy
                         , sepBy
                         , sepBy1
                         , endBy
                         , endBy1
                         , satisfy
                         , try
                         , (<?>)
                         ) where

import Protolude hiding (many, first, try)
import qualified Data.Text as T
import qualified Text.Megaparsec.Char.Lexer as L ( lexeme, skipBlockComment
                                                 , skipLineComment, space, lineFold )
import Text.Megaparsec hiding (ParseError)
import qualified Text.Megaparsec as P (ParseErrorBundle)
import Text.Megaparsec.Char (digitChar, lowerChar, upperChar)


type ParseErr = Void
type Parser = Parsec ParseErr Text
type ParseError = P.ParseErrorBundle Text ParseErr
type ParseResult = Either ParseError
type Lexeme = forall a. Show a => Parser a -> Parser a


lexeme :: Parser a -> Parser a  -- TODO remove entirely, needs to be provided by monad
lexeme = L.lexeme whitespace

whitespace :: Parser ()
whitespace = L.space spaceParser commentParser blockCommentParser where
  spaceParser = skipSome wsChar

-- | Helper for parsing a line fold (parser spanning multiple lines, with lines after
--   beginning line requiring greater indentation). Tries to parse whitespace after the linefold.
withLineFold :: (Parser () -> Parser a) -> Parser a
withLineFold f = lexeme $ L.lineFold whitespace $ \whitespace' ->
  f (try whitespace')
  -- TODO monad transformer to clean up awkward signature (extra reader?)
  -- TODO clean up this entire file

wsChar :: Parser ()
wsChar = void (oneOf [' ', '\n'] <?> "whitespace")

commentParser :: Parser ()
commentParser = L.skipLineComment "--"

blockCommentParser :: Parser ()
blockCommentParser = L.skipBlockComment "{-" "-}"

betweenParens :: Parser a -> Parser a
betweenParens = between (lexeme $ char '(') (char ')') . lexeme

betweenOptionalParens :: Parser a -> Parser a
betweenOptionalParens p = betweenParens p <|> p

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

