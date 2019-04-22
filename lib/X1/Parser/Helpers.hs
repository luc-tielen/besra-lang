
module X1.Parser.Helpers ( Parser, ParseError, ParseErr, ParseResult
                         , lexeme, lexeme', whitespace, whitespace', withLineFold
                         , eof, between, betweenParens, betweenOptionalParens
                         , singleQuote, digitChar, hexDigitChars, binDigitChars
                         , keyword, chunk, char
                         , identifier, capitalIdentifier
                         , notFollowedBy
                         , sepBy, sepBy1, endBy, endBy1
                         , satisfy, takeWhileP
                         , try
                         , (<?>)
                         ) where

import Protolude hiding (try)
import qualified Data.Vector.Unboxed as V
import qualified Data.Text as T
import qualified Text.Megaparsec.Char.Lexer as L ( lexeme, skipBlockComment
                                                 , skipLineComment, space
                                                 , indentLevel, indentGuard )
import Text.Megaparsec hiding (ParseError)
import qualified Text.Megaparsec as P (ParseErrorBundle)
import Text.Megaparsec.Char (digitChar, lowerChar, upperChar)
import GHC.Unicode (isLower, isUpper, isDigit)


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
wsChar = void (char ' ' <|> char '\n') <?> "whitespace"

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

hexDigitChars :: Parser Text
hexDigitChars = takeWhile1P (Just "hex digit") (`V.elem` hexChars) where
  hexChars = ['0'..'9'] V.++ ['a'..'f'] V.++ ['A'..'F']

binDigitChars :: Parser Text
binDigitChars = takeWhile1P (Just "binary digit") (\c -> c == '0' || c == '1')

keyword :: Text -> Parser ()
keyword s = lexeme' (chunk s <* lookAhead wsChar) $> ()

identifier :: Parser Text
identifier = do
  -- TODO forbid keywords
  firstChar <- lowerChar
  rest <- takeWhileP (Just "rest of identifier") isIdentifierChar
  pure $ T.cons firstChar rest

capitalIdentifier :: Parser Text
capitalIdentifier = do
  firstChar <- upperChar
  rest <- takeWhileP (Just "rest of identifier") isIdentifierChar
  pure $ T.cons firstChar rest

isIdentifierChar :: Char -> Bool
isIdentifierChar c = isLower c || isUpper c || isDigit c || c == '\''

singleQuote :: Parser Char
singleQuote = char '\''

