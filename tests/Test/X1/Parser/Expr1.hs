
module Test.X1.Parser.Expr1 ( module Test.X1.Parser.Expr1 ) where

import Protolude hiding ( Type )
import Test.Tasty.Hspec
import Test.X1.Parser.Helpers
import X1.Types.Expr1
import X1.Types.Lit
import X1.Types.Id
import X1.Parser.Expr1 (parser)
import X1.Parser.Types.String
import X1.Parser.Types.Type
import X1.Parser.Types.Scheme
import X1.Parser.Types.Number
import Test.Hspec.Megaparsec hiding (shouldFailWith)


c :: Text -> Type
c = TCon . Tycon . Id

parse :: Text -> ParseResult Expr1
parse = mkParser parser


spec_exprParseTest :: Spec
spec_exprParseTest = describe "expression parser" $ parallel $ do
  describe "literals" $ parallel $ do
    let labels = mconcat $ elabel <$> [ "number", "character literal"
                                      , "string", "if expression"
                                      , "let expression", "variable"
                                      , "lambda expression" ]
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

    it "fails with readable error message for strings" $
      (parse, "\"abc") `shouldFailWith` err 4 (ueof <> etok '"')

    it "fails with readable error message for characters" $ do
      (parse, "''") `shouldFailWith` err 1 (utok '\'' <> elabel "character literal")
      (parse, "'ab'") `shouldFailWith` err 2 (utok 'b' <> elabel "closing single quote (')")
      (parse, "'a '") `shouldFailWith` err 2 (utok ' ' <> elabel "closing single quote (')")
      (parse, "'a") `shouldFailWith` err 2 (ueof <> elabel "closing single quote (')")

    it "fails with readable error message for numbers" $ do
      (parse, "-0b0") `shouldFailWith` err 0 (utoks "-0b" <> labels)
      (parse, "0b2") `shouldFailWith` err 2 (utok '2' <> elabel "binary digit")
      (parse, "0bb1") `shouldFailWith` err 2 (utok 'b' <> elabel "binary digit")
      (parse, "0b") `shouldFailWith` err 2 (ueof <> elabel "binary digit")

  it "can parse variables" $ do
    let a ==> b = parse a `shouldParse` E1Var (Id b)
    "abc123" ==> "abc123"
    "a'" ==> "a'"

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

  describe "let expressions" $ parallel $ do
    let let' = E1Let
        num = E1Lit . LNumber . SInt
        binding x = ExprBindingDecl (Id x)
        sig x ty = ExprTypeDecl (Id x) (Scheme [] ty)
        var = E1Var . Id
        a ==> b = parse a `shouldParse` b

    it "can parse multi-line let expressions" $ do
      "let x = 1 in x" ==> let' [binding "x" (num 1)] (var "x")  -- special case, for now
      "let x = 1\nin x" ==> let' [binding "x" (num 1)] (var "x")
      "let x = 1\nin\n x" ==> let' [binding "x" (num 1)] (var "x")
      "let x = 1\n    y = 2\nin x" ==> let' [binding "x" (num 1), binding "y" (num 2)] (var "x")
      "let x = 1\n    y = 2\nin\n x" ==> let' [binding "x" (num 1), binding "y" (num 2)] (var "x")
      "let\n  x = 1\n  y = 2\nin\n x" ==> let' [binding "x" (num 1), binding "y" (num 2)] (var "x")

    it "can parse nested let expressions" $ do
      "let x = 1 in let y = 2 in y"
        ==> let' [binding "x" (num 1)] (let' [binding "y" (num 2)] (var "y"))
      "let x = let y = 2 in y in x"
        ==> let' [binding "x" (let' [binding "y" (num 2)] (var "y"))] (var "x")
      "let x = let y = 2\n         in y in x"  -- TODO force same indentation in second in
        ==> let' [binding "x" (let' [binding "y" (num 2)] (var "y"))] (var "x")

    it "can parse type signature in let expressions" $
      "let x : Int\n    x = 1 in x" ==> let' [sig "x" (c "Int"), binding "x" (num 1)] (var "x")

    it "fails with readable error message for let expressions" $ do
      (parse, "let") `shouldFailWith` err 3 (ueof <> elabel "whitespace")
      (parse, "let ") `shouldFailWith` err 4 (ueof <> elabel "declaration")
      (parse, "let x") `shouldFailWith` err 5
        (ueof <> elabel "rest of assignment" <> elabel "rest of identifier"
        <> elabel "rest of type declaration")
      (parse, "let x = 1") `shouldFailWith` err 9
        (ueof <> elabel "properly indented declaration or 'in' keyword")
      (parse, "let x = 1 in") `shouldFailWith` err 12 (ueof <> elabel "whitespace")
      (parse, "let x = 1 y = 2 in x") `shouldFailWith` err 10
        (utoks "y " <> elabel "properly indented declaration or 'in' keyword")
      (parse, "let x = 1 inx") `shouldFailWith` err 12 (utok 'x' <> elabel "whitespace")
      (parse, "let x = 1\n    y = 2 in ") `shouldFailWith` err 23
        (ueof <> elabel "if expression" <> elabel "let expression"
        <> elabel "character literal" <> elabel "string"
        <> elabel "number" <> elabel "variable" <> elabel "lambda expression")
      (parse, "let x = 1in") `shouldFailWith` err 9 (utok 'i')

    it "fails with readable error for mismatching indent in bindings" $ do
      (parse, "let x = 1\n y = 2 in x") `shouldFailWith` err 11
        (utoks "y " <> elabel "properly indented declaration or 'in' keyword")
      (parse, "let x = 1\n       y = 2 in x") `shouldFailWith` err 17
        (utoks "y " <> elabel "properly indented declaration or 'in' keyword")
      (parse, "let   x = 1\n    y = 2\nin x") `shouldFailWith` err 16
        (utoks "y " <> elabel "properly indented declaration or 'in' keyword")

  describe "lambdas" $ parallel $ do
    let a ==> b = parse a `shouldParse` b
        lam vars = E1Lam (Id <$> vars)
        var = E1Var . Id
        num = E1Lit . LNumber . SInt

    it "can parse lambda with 1 argument" $ do
      "\\x -> 1" ==> lam ["x"] (num 1)
      "\\x -> x" ==> lam ["x"] (var "x")
      "\\ x -> x" ==> lam ["x"] (var "x")
      "\\x->x" ==> lam ["x"] (var "x")

    it "can parse lambda with more than 1 argument" $ do
      "\\a b -> a" ==> lam ["a", "b"] (var "a")
      "\\ a b c -> 1" ==> lam ["a", "b", "c"] (num 1)

    it "can parse lambda over multiple lines" $ do
      "\\a b -> \n a" ==> lam ["a", "b"] (var "a")
      -- TODO thinks in is a variable: upgrade keyword parser
      "let x = \\a b ->\n     a\nin x" ==> lam ["a", "b"] (var "a")

    it "fails with readable error message" $ do
      (parse, "\\ -> 1") `shouldFailWith` err 2 (utok '-' <> elabel "variable")
      (parse, "\\a \nb -> a") `shouldFailWith` err 4 (utoks "b " <> elabel "lambda arrow")
      (parse, "\\a \n b -> a") `shouldFailWith` err 6
        (utoks "b " <> elabel "lambda arrow")
      (parse, "\\a \n -> a") `shouldFailWith` err 2 (utok '-' <> elabel "variable")

  it "can parse variables" $ do
    let a ==> b = parse a `shouldParse` E1Var (Id b)
    "a" ==> "a"
    "abc123" ==> "abc123"
    "a'" ==> "a'"
    "a'b" ==> "a'b"

