
module Test.X1.Parser.Char ( module Test.X1.Parser.Char ) where

import Protolude
import X1.Parser.Char (parser)
import Test.Hspec.Megaparsec hiding (shouldFailWith)
import Test.X1.Parser.Helpers
import Test.Tasty.Hspec


(==>) :: Text -> Char -> IO ()
a ==> b = parse a `shouldParse` b

parse :: Text -> ParseResult Char
parse = mkParser parser


spec_charParseTest :: Spec
spec_charParseTest = describe "char parser" $ parallel $ do
  it "can parse chars without escaping" $ do
    "'a'" ==> 'a'
    "'b'" ==> 'b'
    "'1'" ==> '1'
    "' '" ==> ' '

  --it "can parse chars with escaping" $ do
    --[text|"\""|] ==> "\""
    --[text|"a\"b"|] ==> "a\"b"
    --[text|"\n"|] ==> "\n"
    --[text|"a\nb"|] ==> "a\nb"

  it "fails with readable error message" $ do
    (parse, "'ab'") `shouldFailWith` err 2 (utok 'b' <> elabel "single quote (')")
    (parse, "'a '") `shouldFailWith` err 2 (utok ' ' <> elabel "single quote (')")
    (parse, "'a") `shouldFailWith` err 2 (ueof <> elabel "single quote (')")
    (parse, "a'") `shouldFailWith` err 0 (utok 'a' <> elabel "single quote (')")

