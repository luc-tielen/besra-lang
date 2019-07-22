
{-# LANGUAGE QuasiQuotes #-}

module Test.Besra.PrettyPrinterSpec ( module Test.Besra.PrettyPrinterSpec ) where

import Protolude
import Test.Hspec
import Test.Besra.Helpers
import Besra.Parser
import NeatInterpolation
import Besra.PrettyPrinter
import qualified Data.Text as T


(==>) :: Text -> Text -> IO ()
input ==> expected =
  let
    parsed = parseFile "prettyprinter.test" input
   in
    case parsed of
      Left err -> panic $ formatError err
      Right ast -> do
        let reprinted = prettyPrint ast
            reparsed = parseFile "reprinted.test" reprinted
        reprinted `shouldBe` T.strip expected
        (stripAnns <$> reparsed) `shouldBe` (stripAnns <$> Right ast)


spec :: Spec
spec = describe "prettyprinter" $ parallel $ do
  -- TODO figure out way to cleverly layout newlines between certain decls
  -- TODO make use of "grouping" to nicely layout heavily nested layouts
  describe "binding declarations" $ parallel $ do
    it "can print binding declarations with no arguments" $ do
      "x    =   1234" ==> "x = 1234"
      "abc    =  \n 1234" ==> "abc = 1234"

    it "can print binding declarations with 1 argument" $
      "abc   def     =  1234" ==> "abc def = 1234"

    it "can print binding declarations with multiple arguments" $
      "abc   def  ghi   =  1234" ==> "abc def ghi = 1234"

    it "can print binding for operator" $
      "(+)      abc  def  =    1234" ==> "(+) abc def = 1234"

  it "can print type annotations" $ do
    "x  :  Eq   a   =>   a   ->   a  ->  Bool"
      ==> "x : Eq a => a -> a -> Bool"
    "x  :  ( Eq   a  )  =>   a   ->   a  ->  Bool"
      ==> "x : Eq a => a -> a -> Bool"
    "x  :  ( Eq   a ,   Ord a )  =>   a   ->   Int  ->  Bool"
      ==> "x : (Eq a, Ord a) => a -> Int -> Bool"
    "x  :  Eq   a   =>   (  (  a ) )   ->   a  ->  Bool"
      ==> "x : Eq a => ((a)) -> a -> Bool"
    "x  :  Eq   a   =>   Maybe  (  Either  String   Int)   -> a -> Bool"
      ==> "x : Eq a => Maybe (Either String Int) -> a -> Bool"

  describe "data declarations" $ parallel $ do
    it "can print data declarations with no body" $ do
      "data   A" ==> "data A"
      "data   A   a  b   c" ==> "data A a b c"

    it "can print data declarations with single body variant" $ do
      "data   A  =  A" ==> "data A = A"
      "data   A   a  b   c  =  A  a  b  c " ==> "data A a b c = A a b c"

    it "can print data declarations with multiple body variants" $ do
      "data   A  =  A   |   B   |   C" ==> "data A = A | B | C"
      "data   A  =  A |  B  a  (b -> c)" ==> "data A = A | B a (b -> c)"

  describe "trait declarations" $ parallel $ do
    it "can print trait blocks with no nested declarations" $ do
      "trait  Eq   a   where" ==> "trait Eq a where"
      "trait  Convert  a  b   where" ==> "trait Convert a b where"

    it "can print trait blocks with single nested declaration" $ do
      let input = [text|
            trait  Eq  a  where
              equals   :   a   ->   a ->  Bool
            |]
          output = [text|
            trait Eq a where
              equals : a -> a -> Bool
            |]
      input ==> output

    it "can print trait block with multiple nested declarations" $ do
      let input = [text|
            trait  Eq  a  where
              equals   :   a   ->   a ->  Bool
              notEquals   :   a   ->   a ->  Bool
            |]
          output = [text|
            trait Eq a where
              equals : a -> a -> Bool
              notEquals : a -> a -> Bool
            |]
      input ==> output

    it "can print trait block containing constraints" $ do
      let input1 = "trait  Eq  a   =>   Ord  a  where"
          output1 = "trait Eq a => Ord a where"
          input2 = [text|
            trait  Eq  a   => Ord a where
              equals   :   a   ->   a ->  Bool
            |]
          output2 = [text|
            trait Eq a => Ord a where
              equals : a -> a -> Bool
            |]
          input3 = [text|
            trait  (A  a ,   B  b )   =>  Convert  a  b where
              convert   : a   ->  b
            |]
          output3 = [text|
            trait (A a, B b) => Convert a b where
              convert : a -> b
            |]
      input1 ==> output1
      input2 ==> output2
      input3 ==> output3


  describe "impl declarations" $ parallel $ do
    it "can print impl blocks with no nested declarations" $ do
      "impl  Eq   Int   where" ==> "impl Eq Int where"
      "impl  Convert  Int String   where" ==> "impl Convert Int String where"

    it "can print impl blocks with single nested declaration" $ do
      let input = [text|
            impl  Show  String  where
              show  =  identity
            |]
          output = [text|
            impl Show String where
              show = identity
            |]
      input ==> output

    it "can print impl blocks with multiple nested declarations" $ do
      let input = [text|
            impl  MyClass  String  where
              func1  a =  1 + 1   + a

              func2  =  identity
            |]
          output = [text|
            impl MyClass String where
              func1 a = 1 + 1 + a
              func2 = identity
            |]
      input ==> output

    it "can print impl blocks with constraints" $ do
      let input1 = "impl  (  Show  a  ,  Show  b )   =>   Show  (  Tuple   a  b  )  where"
          output1 = "impl (Show a, Show b) => Show (Tuple a b) where"
          input2 = [text|
            impl  Show   a   =>   Show   (  Maybe  a  )  where
              func1   =   identity
            |]
          output2 = [text|
            impl Show a => Show (Maybe a) where
              func1 = identity
            |]
          input3 = [text|
            impl  (A  a ,   B  b )   =>  MyClass  (Tuple a  b) where
              convert   = bodyGoesHere
            |]
          output3 = [text|
            impl (A a, B b) => MyClass (Tuple a b) where
              convert = bodyGoesHere
            |]
          scenarios :: [(Text, Text)]
          scenarios = [(input1, output1), (input2, output2), (input3, output3)]
      forM_ scenarios $ uncurry (==>)

  it "can print fixity declarations" $ do
    "infix   5   +++" ==> "infix 5 +++"
    "infixl   4  *" ==> "infixl 4 *"
    "infixr   6    **" ==> "infixr 6 **"

  describe "expressions" $ parallel $ do
    describe "let expressions" $ parallel $ do
      it "can print single line let" $ do
        let input = "z =  let   x   =   1   +   2   in   x"
            expected = "z = \n  let\n    x = 1 + 2\n  in\n    x"
        input ==> expected

      it "can print let with single declaration" $ do
        let input1 =
              [text|
                z =
                  let   x   =   1   +   2
                  in   x
                |]
            input2 =
              [text|
                z =
                  let   x   =   1   +   2
                  in
                    x
                |]
            input3 =
              [text|
                z =
                  let
                    x   =   1   +   2
                  in
                    x
                |]
            expected = "z = \n  let\n    x = 1 + 2\n  in\n    x"
        forM_ ([input1, input2, input3] :: [Text]) $ \input -> input ==> expected

      it "can print let with multiple declarations" $ do
        let input1 =
              [text|
                z =
                  let   x   =   1   +   2
                        y   = 3   + 4
                  in   x   +  y
                |]
            input2 =
              [text|
                z =
                  let   x   =   1   +   2
                        y =   3   + 4
                  in
                    x   +  y
                |]
            expected = "z = \n  let\n    x = 1 + 2\n    y = 3 + 4\n  in\n    x + y"
        forM_ ([input1, input2] :: [Text]) $ \input -> input ==> expected

      it "leaves no newlines between declarations in let" $ do
        let input =
              [text|
                z =
                  let   (+.)   :   Int -> Int   -> Int
                        (+.) a b = a   +   b
                        infixl   5   +.
                  in   10   +.  32
                |]
            expected = "z = \n  let\n    (+.) : Int -> Int -> Int\n"
              <> "    (+.) a b = a + b\n    infixl 5 +.\n  in\n    10 +. 32"
        input ==> expected

    describe "literals" $ parallel $ do
      it "can print decimal numbers" $ do
        "x    =   0" ==> "x = 0"
        "x    =   1234" ==> "x = 1234"

      it "can print hexadecimal numbers" $ do
        "x    =   0x0" ==> "x = 0x0"
        "x    =   0x00" ==> "x = 0x00"
        "x    =   0x123456789" ==> "x = 0x123456789"
        "x    =   0xDEADBEEF" ==> "x = 0xDEADBEEF"
        "x    =   0xC0ff33" ==> "x = 0xC0ff33"

      it "can print binary numbers" $ do
        "x    =   0b0" ==> "x = 0b0"
        "x    =   0b1" ==> "x = 0b1"
        "x    =   0b1101" ==> "x = 0b1101"

      it "can print strings" $ do
        "x   =  \"\"" ==> "x = \"\""
        "x   =   \" \"" ==> "x = \" \""
        "x   =    \"abc123\"" ==> "x = \"abc123\""

      it "can print char literals" $ do
        "x   =   'a'" ==> "x = 'a'"
        "x   =   '9'" ==> "x = '9'"

    it "can print variables" $ do
      "x   =   a" ==> "x = a"
      "x   =   abc123" ==> "x = abc123"

    it "can print constructors/constants" $ do
      "x   =   A" ==> "x = A"
      "x   =   Abc123" ==> "x = Abc123"

    describe "lambdas" $ parallel $ do
      it "can print lambdas with 1 argument" $ do
        "x   =   (  \\  abc123   ->   abc123  )" ==> "x = (\\abc123 -> abc123)"
        "x = (\\  abc123   ->\n abc123)" ==> "x = (\\abc123 -> abc123)"

      it "can print lambdas with multiple arguments" $
        "x   =   (  \\  a   b  ->   c)" ==> "x = (\\a b -> c)"

      describe "patterns" $ parallel $ do
        it "can print lambda containing wildcard pattern" $
          "x   =   (  \\  _ _  ->   123)" ==> "x = (\\_ _ -> 123)"

        it "can print lambda containing literal pattern" $ do
          "x   =   (  \\  1234  567  ->   123)" ==> "x = (\\1234 567 -> 123)"
          "x   =   (  \\  'a'  ->   123)" ==> "x = (\\'a' -> 123)"
          "x   =   (  \\  \"abc\"  ->   123)" ==> "x = (\\\"abc\" -> 123)"

        it "can print lambda containing var pattern" $
          "x   =   (  \\  abc   def  ->   ghi)" ==> "x = (\\abc def -> ghi)"

        it "can print lambda containing constructor pattern" $ do
          "x   =   (  \\  Abc  ->   ghi)" ==> "x = (\\Abc -> ghi)"
          "x   =   (  \\  Abc   True  ->   ghi)" ==> "x = (\\Abc True -> ghi)"

        it "can print lambda containing nested constructor pattern" $ do
          "x   =   (  \\  (A   b   c  ) -> 1)" ==> "x = (\\(A b c) -> 1)"
          "x   =   (  \\  (  Abc   True  ( Abc   def )  ) -> 1)"
            ==> "x = (\\(Abc True (Abc def)) -> 1)"

        it "can print lambda containing as-pattern" $ do
          "x   =   (  \\  abc@(A   b   c  ) -> 1)"
            ==> "x = (\\abc@(A b c) -> 1)"
          "x   =   (  \\  a@1 -> 1)" ==> "x = (\\a@1 -> 1)"

    describe "application" $ parallel $ do
      it "can print function application with 1 argument" $ do
        "x   =  f    123" ==> "x = f 123"
        "x   =  func    123" ==> "x = func 123"

      it "can print function application with multiple arguments" $ do
        "x   =  f    1   2   3" ==> "x = f 1 2 3"
        "x   =  f    123   abc   'x'" ==> "x = f 123 abc 'x'"

    describe "operators" $ parallel $ do
      it "can print binary operators" $ do
        "x   =  1   +   2   *   3" ==> "x = 1 + 2 * 3"
        "x   =  1   +   2   *   3 -  4" ==> "x = 1 + 2 * 3 - 4"

      it "can print prefix unary negation operator" $ do
        "x   =  - 1   +   2" ==> "x = -1 + 2"
        "x   =  1   +   -2" ==> "x = 1 + -2"

      it "can print prefix operators between parentheses" $
        "x =   (+)   1   2" ==> "x = (+) 1 2"

    describe "conditionals" $ parallel $ do
      it "can print if expressions" $ do
        "x = if   True then     1    else    0"
          ==> "x = \n  if True\n    then 1\n    else 0"
        "x = if   True\n     then     1\n      else    0"
          ==> "x = \n  if True\n    then 1\n    else 0"

      it "can print case expressions with 1 clause" $ do
        let expected =
              [text|
                x = case True of
                  y ->
                    123
                |]
        "x   =  case   True   of   y   ->   123" ==> expected

      it "can print case expressions with multiple clauses" $ do
        let input =
              [text|
                x =   case   True   of
                       False ->    123
                       True ->   456
                |]
            expected =
              [text|
                x = case True of
                  False ->
                    123
                  True ->
                    456
                |]
        input ==> expected

    it "can print parenthesized expressions" $ do
      "x   =   (   1  )" ==> "x = (1)"
      "x   =   (   \\a   ->  a  )" ==> "x = (\\a -> a)"

    it "can print nested expression" $ do
      let input =
            [text|
              x =
                let y =
                      let z = 1 + 2
                      in z
                in \a b -> y
              |]
          innerLet = "let\n        z = 1 + 2\n      in\n        z"
          output = "x = \n  let\n    y = \n      " <> innerLet <> "\n  in\n    \\a b -> y"
      input ==> output
