
module Test.X1.Parser.Expr1 ( module Test.X1.Parser.Expr1 ) where

import Protolude
import Test.Tasty.Hspec
import Test.X1.Parser.Helpers
import X1.Types.Expr1
import X1.Types.Lit
import X1.Parser.Expr1 (parser)
import X1.Parser.Types.String
import X1.Parser.Types.Number
import Test.Hspec.Megaparsec hiding (shouldFailWith)


parse :: Text -> ParseResult Expr1
parse = mkParser parser

num :: Number -> Expr1
num = E1Lit . LNumber

str :: Text -> Expr1
str = E1Lit . LString . String

char :: Char -> Expr1
char = E1Lit . LChar


spec_exprParseTest :: Spec
spec_exprParseTest = describe "expression parser" $ parallel $ do
  describe "happy path" $ parallel $ do
    it "can parse literals" $ do
      let a ==> b = parse a `shouldParse` str b
      "\"\"" ==> ""
      "\"0\"" ==> "0"
      "\"ab123\"" ==> "ab123"
      "\"a b\"" ==> "a b"

    it "can parse number literals" $ do
      let a ==> b = parse a `shouldParse` num b
      "123" ==> SInt 123
      "0b1011" ==> SBin "0b1011"
      "0xC0FF33" ==> SHex "0xC0FF33"

    it "can parse char literals" $ do
      let a ==> b = parse a `shouldParse` char b
      "'0'" ==> '0'
      "'a'" ==> 'a'

  describe "non-happy path" $ parallel $ do
    let labels = elabel "number" <> elabel "character literal" <> elabel "string"

    it "fails with readable error message for strings" $ do
      (parse, "abc") `shouldFailWith` err 0 (utok 'a' <> labels)
      (parse, "\"abc") `shouldFailWith` err 4 (ueof <> etok '"')
      (parse, "abc") `shouldFailWith` err 0 (utok 'a' <> labels)

    it "fails with readable error message for characters" $ do
      (parse, "''") `shouldFailWith` err 1 (utok '\'' <> elabel "character literal")
      (parse, "'ab'") `shouldFailWith` err 2 (utok 'b' <> elabel "closing single quote (')")
      (parse, "'a '") `shouldFailWith` err 2 (utok ' ' <> elabel "closing single quote (')")
      (parse, "'a") `shouldFailWith` err 2 (ueof <> elabel "closing single quote (')")
      (parse, "a'") `shouldFailWith` err 0 (utok 'a' <> labels)

    it "fails with readable error message for numbers" $ do
      (parse, "-0b0") `shouldFailWith` err 0 (utok '-' <> labels)
      (parse, "0b2") `shouldFailWith` err 2 (utok '2' <> elabel "binary digit")
      (parse, "0bb1") `shouldFailWith` err 2 (utok 'b' <> elabel "binary digit")
      (parse, "0b") `shouldFailWith` err 2 (ueof <> elabel "binary digit")
