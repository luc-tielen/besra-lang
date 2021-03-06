
module Test.Besra.Parser.LitSpec ( module Test.Besra.Parser.LitSpec ) where

import Protolude
import Test.Hspec
import Test.Hspec.Megaparsec hiding (shouldFailWith)
import Besra.Parser.Lit (parser)
import Test.Besra.Parser.Helpers
import Besra.Types.IR1 ( Lit(..), Number(..), String(..) )


parse :: Text -> ParseResult Lit
parse = mkParser parser


spec :: Spec
spec = describe "literals parser" $ parallel $ do
  describe "happy path" $ parallel $ do
    it "can parse string literals" $ do
      let a ==> b = parse a `shouldParse` LString (String b)
      "\"\"" ==> ""
      "\"0\"" ==> "0"
      "\"ab123\"" ==> "ab123"
      "\"a b\"" ==> "a b"

    it "can parse number literals" $ do
      let a ==> b = parse a `shouldParse` LNumber b
      "123" ==> SInt 123
      "0b1011" ==> SBin "0b1011"
      "0xC0FF33" ==> SHex "0xC0FF33"

    it "can parse char literals" $ do
      let a ==> b = parse a `shouldParse` LChar b
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
