
module X1.Parser.Helpers ( Parser
                         , ParseErr
                         , ParseError
                         , ParseResult
                         , lexeme
                         , lexeme'
                         , whitespace
                         , whitespace'
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
                                                 , skipLineComment, space, indentLevel, indentGuard )
import Text.Megaparsec hiding (ParseError)
import qualified Text.Megaparsec as P (ParseErrorBundle)
import Text.Megaparsec.Char (digitChar, lowerChar, upperChar)


type ParseErr = Void
type IndentLevel = Pos
type Parser = ParsecT ParseErr Text (Reader IndentLevel)
type ParseError = P.ParseErrorBundle Text ParseErr
type ParseResult = Either ParseError


-- Higher order parser that parses all trailing whitespace after the given parser.
lexeme :: Parser a -> Parser a
lexeme = L.lexeme whitespace

-- | Same as lexeme, but takes last indent level into account (for example in a linefold)
lexeme' :: Parser a -> Parser a
lexeme' = L.lexeme whitespace'

-- | Parser that consumes whitespace in general.
whitespace :: Parser ()
whitespace = L.space spaceParser commentParser blockCommentParser where
  spaceParser = skipSome wsChar

-- | Same as whitespace, but takes last indent level into account (e.g. in a line fold)
whitespace' :: Parser ()
whitespace' = try $ do
  lastIndentLvl <- ask
  void $ L.indentGuard whitespace GT lastIndentLvl

-- | Helper for parsing a line fold (parser spanning multiple lines, with lines after
--   beginning line requiring greater indentation). Tries to parse whitespace after the linefold.
withLineFold :: Parser a -> Parser a
withLineFold p = lexeme $ do
  whitespace
  currentIndent <- L.indentLevel
  local (const currentIndent) p

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

