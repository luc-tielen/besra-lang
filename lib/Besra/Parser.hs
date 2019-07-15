module Besra.Parser ( Parser, ParseError, parse, parseFile, printError ) where

-- Mostly based on https://www.haskell.org/onlinereport/lexemes.html,
-- with some minor variations.

import Protolude
import Besra.Parser.Helpers ( Parser, ParseErr, ParseError
                         , ParseResult, ParseState(..), ParseMode(..) )
import qualified Besra.Parser.Module as Module
import Besra.Types.IR1.Module
import Besra.Types.Ann
import qualified Text.Megaparsec as P
import qualified Data.Text as T


type Module' = Module 'Parsed

parse :: Parser a -> FilePath -> Text -> ParseResult a
parse p path txt =
  let beginState = ParseState P.pos1 Normal
   in flip runReader beginState $ P.runParserT p path txt

parseFile :: FilePath -> Text -> ParseResult Module'
parseFile = parse Module.parser

-- | Pretty prints a parser error
printError :: P.ParseErrorBundle Text ParseErr -> Text
printError e = T.pack $ P.errorBundlePretty e
