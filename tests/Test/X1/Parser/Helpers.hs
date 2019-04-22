
module Test.X1.Parser.Helpers ( module Test.X1.Parser.Helpers
                              , Parser
                              , ParseErr
                              , ParseResult
                              ) where

import Protolude
import X1.Parser
import X1.Parser.Helpers
import qualified Test.Hspec.Megaparsec as TP ( initialState, shouldFailWith
                                             , succeedsLeaving)
import qualified Text.Megaparsec as P ( ParseError, mkPos, runParserT')
import Text.Megaparsec.Error ( ErrorFancy(..) )
import Text.Megaparsec.Pos ( mkPos )
import Text.Megaparsec.Error.Builder ( EF, fancy )


mkParser :: Parser a -> (Text -> ParseResult a)
mkParser = flip parse ""

badIndent :: Int -> Int -> EF ParseErr
badIndent refLvl actualLvl = fancy $ ErrorIndentation GT (mkPos refLvl) (mkPos actualLvl)

succeedsLeaving :: Show a
                => (Parser a, Text) -> Text
                -> IO ()
succeedsLeaving (parser, text) restOfText =
  let parser' = P.runParserT' parser (TP.initialState text)
   in runReader parser' (P.mkPos 1) `TP.succeedsLeaving` restOfText

shouldFailWith :: Show a
               => (Text -> ParseResult a, Text) -> P.ParseError Text ParseErr
               -> IO ()
shouldFailWith (parser, text) err =
  parser text `TP.shouldFailWith` err
