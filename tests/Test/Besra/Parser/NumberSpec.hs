
module Test.Besra.Parser.NumberSpec ( module Test.Besra.Parser.NumberSpec ) where

import Protolude
import Test.Hspec
import Test.Hspec.Megaparsec hiding (shouldFailWith, succeedsLeaving)
import Besra.Parser.Number (parser)
import Besra.Types.IR1.Number
import Test.Besra.Parser.Helpers


parse :: Text -> ParseResult Number
parse = mkParser parser

spec :: Spec
spec = describe "number parser" $ parallel $ do
  describe "decimal number parser" $ parallel $ do
    let (==>) :: Text -> Int -> IO ()
        a ==> b = parse a `shouldParse` SInt b

    it "can parse decimal numbers" $ do
      "0" ==> 0
      "1234567890" ==> 1234567890

    it "gives readable error message when it fails" $ do
      (parse, "not a number") `shouldFailWith` err 0 (utok 'n' <> elabel "number")
      (parse, "0123") `shouldFailWith` err 1
        (utok '1' <> elabel "constant number prefix: hex (0x) or binary (0b)")
      (parse, "-0") `shouldFailWith` err 0 (utok '-' <> elabel "number")

  describe "hexadecimal number parser" $ parallel $ do
    let (==>) :: Text -> Text -> IO ()
        a ==> b = parse a `shouldParse` SHex b

    it "can parse hexadecimal numbers" $ do
      "0x0" ==> "0x0"
      "0x1" ==> "0x1"
      "0xa" ==> "0xa"
      "0xA" ==> "0xA"
      "0xf" ==> "0xf"
      "0xF" ==> "0xF"
      "0xF3" ==> "0xF3"

    it "gives readable error message when it fails" $ do
      (parse, "-0x5") `shouldFailWith` err 0 (utok '-' <> elabel "number")
      (parse, "0xx3") `shouldFailWith` err 2 (utok 'x' <> elabel "hex digit")
      (parse, "0xG") `shouldFailWith` err 2 (utok 'G' <> elabel "hex digit")
      (parse, "0x") `shouldFailWith` err 2 (ueof <> elabel "hex digit")

  describe "binary number parser" $ parallel $ do
    let (==>) :: Text -> Text -> IO ()
        a ==> b = parse a `shouldParse` SBin b

    it "can parse binary numbers" $ do
      "0b0" ==> "0b0"
      "0b1" ==> "0b1"
      "0b111" ==> "0b111"
      "0b101" ==> "0b101"

    it "gives readable error message when it fails" $ do
      (parse, "-0b0") `shouldFailWith` err 0 (utok '-' <> elabel "number")
      (parse, "0b2") `shouldFailWith` err 2 (utok '2' <> elabel "binary digit")
      (parse, "0bb1") `shouldFailWith` err 2 (utok 'b' <> elabel "binary digit")
      (parse, "0b") `shouldFailWith` err 2 (ueof <> elabel "binary digit")
      (parser, "0b12") `succeedsLeaving` "2"

