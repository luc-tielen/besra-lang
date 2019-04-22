
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


spec_exprParseTest :: Spec
spec_exprParseTest = describe "expression parser" $ parallel $ do
  describe "literals" $ parallel $ do
    let labels = mconcat $ elabel <$> ["number", "character literal", "string", "if expression"]
        num = E1Lit . LNumber
        str = E1Lit . LString . String
        char = E1Lit . LChar

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

    it "fails with readable error message for strings" $ do
      (parse, "abc") `shouldFailWith` err 0 (utoks "ab" <> labels)
      (parse, "\"abc") `shouldFailWith` err 4 (ueof <> etok '"')
      (parse, "abc") `shouldFailWith` err 0 (utoks "ab" <> labels)

    it "fails with readable error message for characters" $ do
      (parse, "''") `shouldFailWith` err 1 (utok '\'' <> elabel "character literal")
      (parse, "'ab'") `shouldFailWith` err 2 (utok 'b' <> elabel "closing single quote (')")
      (parse, "'a '") `shouldFailWith` err 2 (utok ' ' <> elabel "closing single quote (')")
      (parse, "'a") `shouldFailWith` err 2 (ueof <> elabel "closing single quote (')")
      (parse, "a'") `shouldFailWith` err 0 (utoks "a'" <> labels)

    it "fails with readable error message for numbers" $ do
      (parse, "-0b0") `shouldFailWith` err 0 (utoks "-0" <> labels)
      (parse, "0b2") `shouldFailWith` err 2 (utok '2' <> elabel "binary digit")
      (parse, "0bb1") `shouldFailWith` err 2 (utok 'b' <> elabel "binary digit")
      (parse, "0b") `shouldFailWith` err 2 (ueof <> elabel "binary digit")

  describe "if expressions" $ parallel $ do
    -- NOTE: does not take typesystem into account, only parsing.
    let if' = E1If
        num = E1Lit . LNumber . SInt
        str = E1Lit . LString . String
        char = E1Lit . LChar
        a ==> b = parse a `shouldParse` b

    it "can parse single-line if expressions" $ do
      "if 123 then 456 else 789" ==> if' (num 123) (num 456) (num 789)
      "if 'a' then \"abc\" else 1" ==> if' (char 'a') (str "abc") (num 1)

    it "can parse multi-line if expressions" $ do
      "if\n 123\n then\n 456\n else\n 789" ==> if' (num 123) (num 456) (num 789)
      "if 'a'\n then\n \"abc\"\n else\n 1" ==> if' (char 'a') (str "abc") (num 1)

    it "can parse nested if expressions" $ do
      "if if 1 then 2 else 3 then 4 else 5" ==> if' (if' (num 1) (num 2) (num 3)) (num 4) (num 5)
      "if 1 then if 2 then 3 else 4 else 5" ==> if' (num 1) (if' (num 2) (num 3) (num 4)) (num 5)
      "if 1 then 2 else if 3 then 4 else 5" ==> if' (num 1) (num 2) (if' (num 3) (num 4) (num 5))

    it "fails with readable error message for ifs" $ do
      (parse, "if") `shouldFailWith` err 2 (ueof <> elabel "whitespace")
      (parse, "if 1") `shouldFailWith` err 4 (ueof <> etoks "then")
      (parse, "if 1 then") `shouldFailWith` err 9 (ueof <> elabel "whitespace")
      (parse, "if 1 then 2") `shouldFailWith` err 11 (ueof <> etoks "else")
      (parse, "if 1 then 2 else") `shouldFailWith` err 16 (ueof <> elabel "whitespace")
      (parse, "iff 1") `shouldFailWith` err 2 (utok 'f' <> elabel "whitespace")
      (parse, "if 1 thn 2") `shouldFailWith` err 5 (utoks "thn " <> etoks "then")
      (parse, "if 1 then 2 ele 3") `shouldFailWith` err 12 (utoks "ele " <> etoks "else")

    it "fails with readable error message for incorrect line folds" $ do
      (parse, "if \n1 then 2 else 3") `shouldFailWith` errFancy 4 (badIndent 1 1)
      (parse, " if \n1 then 2 else 3") `shouldFailWith` errFancy 5 (badIndent 2 1)
      (parse, "if \n 1\nthen 2 else 3") `shouldFailWith` errFancy 7 (badIndent 1 1)
      (parse, "if \n 1\n then\n2 else 3") `shouldFailWith` errFancy 13 (badIndent 1 1)
      (parse, "if \n 1\n then\n 2\nelse 3") `shouldFailWith` errFancy 16 (badIndent 1 1)
      (parse, "if \n 1\n then\n 2\n else\n3") `shouldFailWith` errFancy 22 (badIndent 1 1)
      (parse, "if 1 then if 2\nthen 3 else 4 else 5") `shouldFailWith` errFancy 15 (badIndent 11 1)

      -- error with line fold (indents after each part)
      -- nested example
