
{-# LANGUAGE QuasiQuotes #-}

module Test.Besra.Parser.String ( module Test.Besra.Parser.String ) where

import Protolude
import Test.Tasty.Hspec
import Test.Hspec.Megaparsec hiding (shouldFailWith)
import Besra.Parser.String (parser)
import Besra.Types.IR1.String
import NeatInterpolation
import Test.Besra.Parser.Helpers


(==>) :: Text -> Text -> IO ()
a ==> b = parse a `shouldParse` String b

parse :: Text -> ParseResult String
parse = mkParser parser


spec_stringParseTest :: Spec
spec_stringParseTest = describe "string parser" $ parallel $ do
  it "can parse strings without escaping" $ do
    [text|""|] ==> ""
    [text|"a"|] ==> "a"
    [text|"ab"|] ==> "ab"
    [text|"ab123"|] ==> "ab123"
    [text|" "|] ==> " "
    [text|"  "|] ==> "  "
    [text|"a b"|] ==> "a b"

  it "fails with readable error message" $ do
    (parse, "abc") `shouldFailWith` err 0 (utok 'a' <> elabel "string")
    (parse, "\"abc") `shouldFailWith` err 4 (ueof <> etok '"')
    (parse, [text|"abc
      |]) `shouldFailWith` err 5 (ueof <> etok '"')

  --it "can parse strings with escaping" $ do
    --[text|"\""|] ==> "\""
    --[text|"a\"b"|] ==> "a\"b"
    --[text|"\n"|] ==> "\n"
    --[text|"a\nb"|] ==> "a\nb"

  --it "can't parse multiline strings" $ do
  --  [text|"
  --    "|] `shouldFail`
