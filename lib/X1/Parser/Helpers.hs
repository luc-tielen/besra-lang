
module X1.Parser.Helpers ( Parser, ParseError, ParseErr, ParseResult
                         , ParseState(..), ParseMode(..), KeywordResult(..)
                         , lexeme, lexeme', whitespace, whitespace', withLineFold
                         , eof, between, betweenParens, betweenOptionalParens
                         , singleQuote, digitChar, hexDigitChars, binDigitChars
                         , letterChar, opIdentifier
                         , keyword, keyword', chunk, char
                         , identifier, capitalIdentifier
                         , notFollowedBy, lookAhead, hidden
                         , sepBy, sepBy1, endBy, endBy1
                         , L.indentLevel, withIndent, indented, sameLine
                         , withDefault, withSpan
                         , satisfy, takeWhileP
                         , try
                         , (<?>)
                         ) where

import Protolude hiding (try, first)
import Control.Monad ( fail )
import X1.Types.Ann
import X1.Types.Span
import qualified Data.Vector.Unboxed as VU
import qualified Data.Vector as V
import qualified Data.Text as T
import qualified Text.Megaparsec.Char.Lexer as L ( lexeme, skipBlockComment
                                                 , skipLineComment, space
                                                 , indentLevel, indentGuard )
import Text.Megaparsec hiding (ParseError)
import qualified Text.Megaparsec as P (ParseErrorBundle)
import Text.Megaparsec.Char (digitChar, letterChar, lowerChar, upperChar)
import GHC.Unicode (isLower, isUpper, isDigit)


data KeywordResult = TrailingWS | NoTrailingWS
  deriving (Eq, Show)

type IndentLevel = Pos

data ParseMode = Normal | SameLine
  deriving (Eq, Show)

data ParseState = ParseState { psIndentLevel :: IndentLevel, psParseMode :: ParseMode }
  deriving (Eq, Show)

type ParseErr = Void
type Parser = ParsecT ParseErr Text (Reader ParseState)
type ParseError = P.ParseErrorBundle Text ParseErr
type ParseResult = Either ParseError


-- Higher order parser that parses all trailing whitespace after the given parser.
lexeme :: Parser a -> Parser a
lexeme p = asks psParseMode >>= \case
  Normal -> L.lexeme whitespace p
  SameLine -> L.lexeme whitespaceSameLine p

-- | Same as lexeme, but takes last indent level into account (for example in a linefold)
lexeme' :: Parser a -> Parser a
lexeme' = L.lexeme whitespace'

-- | Parser that consumes whitespace in general.
whitespace :: Parser ()
whitespace = L.space spaceParser commentParser blockCommentParser where
  spaceParser = skipSome wsChar
  commentParser = L.skipLineComment "--"
  blockCommentParser = L.skipBlockComment "{-" "-}"

-- | Same as whitespace, but takes last indent level into account (e.g. in a line fold)
whitespace' :: Parser ()
whitespace' = try $ do
  lastIndentLvl <- asks psIndentLevel
  void $ L.indentGuard whitespace GT lastIndentLvl

whitespaceSameLine :: Parser ()
whitespaceSameLine = L.space (skipSome ws) empty empty where
  ws = char ' ' <?> "whitespace"

-- | Helper for parsing a chunk of text, with everything on same line.
sameLine :: Parser a -> Parser a
sameLine p = do
  parseState <- ask
  local (const $ parseState { psParseMode = SameLine }) p

-- | Helper for parsing a line fold (parser spanning multiple lines, with lines after
--   beginning line requiring greater indentation). Tries to parse whitespace after the linefold.
withLineFold :: Parser a -> Parser a
withLineFold p = lexeme $ do
  whitespace
  currentIndent <- L.indentLevel
  parseState <- ask
  local (const $ parseState { psIndentLevel = currentIndent }) p

withIndent :: Pos -> Parser a -> Parser a
withIndent indent p = L.indentGuard whitespace EQ indent *> p

-- | Helper for only parsing if a word occurs with indentation > 1.
--   Assumes space in front of the token to be parsed has already been parsed.
--   This works best for declarations that can only appear at top level in
--   combination with lexeme (instead of lexeme' in a linefold).
indented :: Parser a -> Parser a
indented p = L.indentGuard (pure ()) GT pos1 *> p

wsChar :: Parser ()
wsChar = void (char ' ' <|> char '\n') <?> "whitespace"

betweenParens :: Parser a -> Parser a
betweenParens = between (lexeme' $ char '(') (char ')') . lexeme'

betweenOptionalParens :: Parser a -> Parser a
betweenOptionalParens p = betweenParens p <|> p

-- | Helper for optionally parsing a value. If the parsing fails,
--   it will use the provided default value instead.
withDefault :: a -> Parser a -> Parser a
withDefault def p = p <|> pure def

char :: Char -> Parser Char
char = single

hexDigitChars :: Parser Text
hexDigitChars = takeWhile1P (Just "hex digit") (`VU.elem` hexChars) where
  hexChars = ['0'..'9'] VU.++ ['a'..'f'] VU.++ ['A'..'F']

binDigitChars :: Parser Text
binDigitChars = takeWhile1P (Just "binary digit") (\c -> c == '0' || c == '1')

-- | Helper function for creating a parser that consumes a keyword.
--   Expects trailing whitespace after the actual keyword.
keyword :: Text -> Parser ()
keyword s = lexeme' (chunk s <* lookAhead wsChar) $> ()


-- | Helper function for creating a parser that consumes a keyword
--   with optional trailing whitespace. The return value indicates
--   if it consumed whitespace or not.
keyword' :: Text -> Parser KeywordResult
keyword' s = try (keyword s $> TrailingWS)
          <|> chunk s $> NoTrailingWS

identifier :: Parser Text
identifier = do
  firstChar <- lowerChar
  rest <- takeWhileP (Just "rest of identifier") isIdentifierChar
  let parsed = T.cons firstChar rest
  when (parsed `V.elem` reserved) (fail . T.unpack $ "Reserved keyword: " <> parsed)
  pure parsed
  where reserved = [ "module", "type", "data", "trait", "impl"
                   , "do", "let", "in", "where", "if", "else", "case", "of"
                   , "infix", "infixl", "infixr"
                   ]

capitalIdentifier :: Parser Text
capitalIdentifier = do
  firstChar <- upperChar
  rest <- takeWhileP (Just "rest of identifier") isIdentifierChar
  pure $ T.cons firstChar rest

isIdentifierChar :: Char -> Bool
isIdentifierChar c = isLower c || isUpper c || isDigit c || c == '\''

opIdentifier :: Parser Text
opIdentifier = do
  first <- satisfy isOperatorChar <?> "operator"
  rest <- takeWhileP (Just "rest of operator") isOperatorChar
  let parsed = T.cons first rest
  when (parsed `V.elem` reserved) (fail . T.unpack $ "Reserved operator: '" <> parsed <> "'")
  notFollowedBy $ satisfy isDigit
  pure parsed
  where
    isOperatorChar c = c `VU.elem` opChars
    opChars = ['!', '#', '$', '%', '&', '.', '+', '*', '/', '<', '>'
              , '=', '?', '@', '\\', '^', '|', '-', '~', ':']
    reserved = [ "..", ":", "=", "\\", "|", "<-", "->", "=>", "@", "~" ]

singleQuote :: Parser Char
singleQuote = char '\''

-- | Helper function for getting the begin and end offset when parsing something.
--   Important: use this function before lexeme/lexeme' or the trailing
--   whitespace will also be counted!
withSpan :: Parser a -> Parser (Ann 'Parsed, a)
withSpan p = do
  begin <- getOffset
  result <- p
  end <- getOffset
  pure (Span begin end, result)

