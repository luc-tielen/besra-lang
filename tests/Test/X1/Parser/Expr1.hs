
module Test.X1.Parser.Expr1 ( module Test.X1.Parser.Expr1 ) where

import Protolude hiding ( Type )
import Test.Tasty.Hspec
import Test.X1.Parser.Helpers
import X1.Parser.Expr1 (parser)
import X1.Types.Id
import X1.Types.Fixity
import X1.Types.Expr1
import X1.Types.Expr1.Lit
import X1.Types.Expr1.Type
import X1.Types.Expr1.String
import X1.Types.Expr1.Scheme
import X1.Types.Expr1.Number
import X1.Types.Expr1.Pattern
import X1.Types.Expr1.TypeAnn
import Test.Hspec.Megaparsec hiding (shouldFailWith, succeedsLeaving)


c :: Text -> Type
c = TCon . Tycon . Id

let' :: [ExprDecl] -> Expr1 -> Expr1
let' = E1Let

binding :: Text -> Expr1 -> ExprDecl
binding x = ExprBindingDecl . Binding (Id x)

sig :: Text -> Type -> ExprDecl
sig x ty = ExprTypeAnnDecl $ TypeAnn (Id x) (Scheme [] ty)

parse :: Text -> ParseResult Expr1
parse = mkParser parser


spec_exprParseTest :: Spec
spec_exprParseTest = describe "expression parser" $ parallel $ do
  describe "literals" $ parallel $ do
    let num = E1Lit . LNumber
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
      (parse, "-0b0") `shouldFailWith` err 0 (utoks "-0b0" <> elabel "expression")
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
      (parse, "if 1") `shouldFailWith` err 4 (ueof <> etoks "then" <> elabel "operator")
      (parse, "if 1 then") `shouldFailWith` err 9 (ueof <> elabel "whitespace")
      (parse, "if 1 then 2") `shouldFailWith` err 11 (ueof <> etoks "else" <> elabel "operator")
      (parse, "if 1 then 2 else") `shouldFailWith` err 16 (ueof <> elabel "whitespace")
      (parse, "iff 1") `shouldFailWith` err 2 (utok 'f' <> elabel "whitespace")
      (parse, "if 1 thn 2") `shouldFailWith` err 5 (utoks "thn " <> etoks "then" <> elabel "operator")
      (parse, "if 1 then 2 ele 3") `shouldFailWith` err 12
        (utoks "ele " <> etoks "else" <> elabel "operator")

    it "fails with readable error message for incorrect line folds" $ do
      (parse, "if \n1 then 2 else 3") `shouldFailWith` errFancy 4 (badIndent 1 1)
      (parse, " if \n1 then 2 else 3") `shouldFailWith` errFancy 5 (badIndent 2 1)
      (parse, "if \n 1\nthen 2 else 3") `shouldFailWith` errFancy 7 (badIndent 1 1)
      (parse, "if \n 1\n then\n2 else 3") `shouldFailWith` errFancy 13 (badIndent 1 1)
      (parse, "if \n 1\n then\n 2\nelse 3") `shouldFailWith` errFancy 16 (badIndent 1 1)
      (parse, "if \n 1\n then\n 2\n else\n3") `shouldFailWith` errFancy 22 (badIndent 1 1)
      (parse, "if 1 then if 2\nthen 3 else 4 else 5") `shouldFailWith` errFancy 15 (badIndent 11 1)

  describe "let expressions" $ parallel $ do
    let var = E1Var . Id
        num = E1Lit . LNumber . SInt
        lam vars = E1Lam (PVar . Id <$> vars)
        a ==> b = parse a `shouldParse` b

    it "can parse multi-line let expressions" $ do
      "let x = 1 in x" ==> let' [binding "x" (num 1)] (var "x")  -- special case, for now
      "let x = 1\nin x" ==> let' [binding "x" (num 1)] (var "x")
      "let x = 1\nin\n x" ==> let' [binding "x" (num 1)] (var "x")
      "let x = 1\n    y = 2\nin x" ==> let' [binding "x" (num 1), binding "y" (num 2)] (var "x")
      "let x = 1\n    y = 2\nin\n x" ==> let' [binding "x" (num 1), binding "y" (num 2)] (var "x")
      "let\n  x = 1\n  y = 2\nin\n x" ==> let' [binding "x" (num 1), binding "y" (num 2)] (var "x")

    it "can parse named functions with 1 arg in let block" $ do
      let expected = let' [binding "f" (lam ["x"] (num 1))] (var "f")
      "let f x = 1 in f" ==> expected  -- special case, for now
      "let f x = 1\nin f" ==> expected
      "let f x =\n      1\nin f" ==> expected

    it "can parse named functions with multiple args in let block" $ do
      let expected = let' [binding "f" (lam ["x", "y"] (num 1))] (var "f")
      "let f x y = 1 in f" ==> expected  -- special case, for now
      "let f x y =\n      1\nin f" ==> expected

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
        <> elabel "rest of type declaration" <> elabel "pattern")
      (parse, "let x = 1") `shouldFailWith` err 9
        (ueof <> elabel "properly indented declaration or 'in' keyword")
      (parse, "let x = 1 in") `shouldFailWith` err 12 (ueof <> elabel "whitespace")
      (parse, "let x = 1 y = 2 in x") `shouldFailWith` err 10
        (utoks "y " <> elabel "properly indented declaration or 'in' keyword")
      (parse, "let x = 1 inx") `shouldFailWith` err 12 (utok 'x' <> elabel "whitespace")
      (parse, "let x = 1\n    y = 2 in ") `shouldFailWith` err 23 (ueof <> elabel "expression")
      (parse, "let x = 1in") `shouldFailWith` err 9 (utok 'i')
      (parse, "let x\n      a = 1\nin x") `shouldFailWith` err 12
        (utok 'a' <> elabel "rest of assignment" <> elabel "rest of type declaration")

    it "fails with readable error for mismatching indent in bindings" $ do
      (parse, "let x = 1\n y = 2 in x") `shouldFailWith` err 11
        (utoks "y " <> elabel "properly indented declaration or 'in' keyword")
      (parse, "let x = 1\n       y = 2 in x") `shouldFailWith` err 17
        (utoks "y " <> elabel "properly indented declaration or 'in' keyword")
      (parse, "let   x = 1\n    y = 2\nin x") `shouldFailWith` err 16
        (utoks "y " <> elabel "properly indented declaration or 'in' keyword")

  describe "lambdas" $ parallel $ do
    let a ==> b = parse a `shouldParse` b
        lam vars = E1Lam (PVar . Id <$> vars)
        num' = LNumber . SInt
        num = E1Lit . num'
        str' =  LString . String
        var = E1Var . Id

    it "can parse lambda with 1 argument" $ do
      "\\x -> 1" ==> lam ["x"] (num 1)
      "\\x -> x" ==> lam ["x"] (var "x")
      "\\ x -> x" ==> lam ["x"] (var "x")
      "\\x->x" ==> lam ["x"] (var "x")

    it "can parse lambda with more than 1 argument" $ do
      "\\a b -> a" ==> lam ["a", "b"] (var "a")
      "\\ a b c -> 1" ==> lam ["a", "b", "c"] (num 1)

    it "can parse lambdas containing patterns" $ do
      "\\1 \"abc\" -> 123" ==> E1Lam [PLit (num' 1), PLit (str' "abc")] (num 123)
      "\\a@(X y) -> 123" ==> E1Lam [PAs (Id "a") $ PCon (Id "X") [PVar (Id "y")]] (num 123)

    it "can parse lambda over multiple lines" $ do
      "\\a b -> \n a" ==> lam ["a", "b"] (var "a")
      "let x = \\a b ->\n         a\nin x"
        ==> let' [binding "x" (lam ["a", "b"] (var "a"))] (var "x")

    it "can parse nested lambdas" $ do
      "\\a -> \\b -> a" ==> lam ["a"] (lam ["b"] (var "a"))
      "\\a -> \n \\b -> a" ==> lam ["a"] (lam ["b"] (var "a"))
      "\\a -> \n \\b ->\n  a" ==> lam ["a"] (lam ["b"] (var "a"))

    it "fails with readable error message" $ do
      let expected = elabel "lambda arrow" <> elabel "pattern"
      (parse, "\\ -> 1") `shouldFailWith` err 2 (utok '-' <> elabel "pattern")
      (parse, "\\\n -> 1") `shouldFailWith` err 1 (utok '\n' <> elabel "pattern")
      (parse, "\\a \nb -> a") `shouldFailWith` err 3 (utoks "\nb" <> expected)
      (parse, "\\a \n b -> a") `shouldFailWith` err 3 (utoks "\n " <> expected)
      (parse, "\\a \n -> a") `shouldFailWith` err 3 (utoks "\n " <> expected)
      (parse, "\\a ->\na") `shouldFailWith` errFancy 6 (badIndent 1 1)

  describe "function application" $ parallel $ do
    let a ==> b = parse a `shouldParse` b
        app = E1App
        num = E1Lit . LNumber . SInt
        var = E1Var . Id
        con = E1Con . Id

    it "can parse application with 1 argument" $ do
      "f 1" ==> app (var "f") [num 1]
      "f abc" ==> app (var "f") [var "abc"]
      "DataCon abc" ==> app (con "DataCon") [var "abc"]

    it "can parse application with more than 1 argument" $ do
      "f 1 2" ==> app (var "f") [num 1, num 2]
      "f abc def" ==> app (var "f") [var "abc", var "def"]
      "DataCon abc def" ==> app (con "DataCon") [var "abc", var "def"]

    it "fails with readable error message" $ do
      (parser, "f\n a") `succeedsLeaving` "a"  -- parses variable only
      (parse, "in a") `shouldFailWith` errFancy 2 (failMsg "Reserved keyword: in")
      (parser, "a in") `succeedsLeaving` "in"

    it "can parse expressions inside parentheses" $ do
      "f (1)" ==> app (var "f") [num 1]
      "f (a 1)" ==> app (var "f") [app (var "a") [num 1]]
      "f a (b 1)" ==> app (var "f") [var "a", app (var "b") [num 1]]
      "(((1)))" ==> num 1

  describe "case expressions" $ parallel $ do
    let a ==> b = parse a `shouldParse` b
        case' = E1Case
        pvar = PVar . Id
        pcon x = PCon (Id x)
        var = E1Var . Id
        num' = LNumber . SInt
        num = E1Lit . num'
        str = E1Lit . LString . String

    it "can parse a case expression with 1 branch" $ do
      "case 1 of\n x -> x" ==> case' (num 1) [(pvar "x", var "x")]
      "case 1 of\n x ->\n   x" ==> case' (num 1) [(pvar "x", var "x")]
      "case \"abc\" of\n x -> x" ==> case' (str "abc") [(pvar "x", var "x")]
      "case 1 of\n (X y) -> y" ==> case' (num 1) [(pcon "X" [pvar "y"], var "y")]

    it "can parse a case expression with multiple branches" $ do
      "case bool of\n True -> 1\n False -> 0"
        ==> case' (var "bool") [(pcon "True" [], num 1), (pcon "False" [], num 0)]
      "case bool of\n True ->\n  1\n False ->\n  0"
        ==> case' (var "bool") [(pcon "True" [], num 1), (pcon "False" [], num 0)]
      "case 1 of\n 0 -> 0\n 1 -> 1\n _ -> 0"
        ==> case' (num 1) [(PLit (num' 0), num 0), (PLit (num' 1), num 1), (PWildcard, num 0)]

    it "fails with readable error message" $ do
      (parse, "case") `shouldFailWith` err 4 (ueof <> elabel "whitespace")
      (parse, "case ") `shouldFailWith` err 5 (ueof <> elabel "expression")
      (parse, "case 1 ") `shouldFailWith` err 7 (ueof <> etoks "of" <> elabel "operator")
      (parse, "case 1 of") `shouldFailWith` err 9 (ueof <> elabel "whitespace")
      (parse, "case 1 of ") `shouldFailWith` err 10 (ueof <> elabel "case clause")
      (parse, "case 1 of\n1 -> 1") `shouldFailWith` errFancy 10 (badIndent 1 1)
      (parse, "case 1 of\n 1") `shouldFailWith` err 12 (ueof <> etoks "->")
      (parse, "case 1 of\n 1 ->") `shouldFailWith` err 15 (ueof <> elabel "expression")
      (parse, "case 1 of\n  2 -> 2\n 1 -> 1") `shouldFailWith` err 20
        (utok '1' <> elabel "properly indented case clause")
      (parse, "case 1 of\n 1 -> 1\n  2 -> 2") `shouldFailWith` err 20
        (utok '2' <> elabel "properly indented case clause")

  describe "operators" $ parallel $ do
    let num = E1Lit . LNumber . SInt
        fixity ty prio op = ExprFixityDecl ty prio (Id op)
        app = E1App
        var = E1Var . Id
        con = E1Con . Id
        a ==> b = parse a `shouldParse` b

    it "can parse a type declaration for an operator in a let" $ do
      "let (+) : Int\nin\n 1" ==> let' [sig "+" (c "Int")] (num 1)
      "let (+) : Int\n    (*) : Int\nin\n 1"
        ==> let' [sig "+" (c "Int"), sig "*" (c "Int")] (num 1)

    it "can parse a prefix binding declaration for an operator in a let" $ do
      "let (+) = 1\nin\n 1" ==> let' [binding "+" (num 1)] (num 1)
      "let (+) = 1\n    (*) = 2\nin\n 1"
        ==> let' [binding "+" (num 1), binding "*" (num 2)] (num 1)

    it "can parse fixity decl in let " $ do
      "let infixl 4 +\nin\n 1" ==> let' [fixity L 4 "+"] (num 1)
      "let infixl 4 +\n    infixl 5 *\nin\n 1"
        ==> let' [fixity L 4 "+", fixity L 5 "*"] (num 1)
      "let infixl 4 `plus`\n    infixl 5 `mul`\nin\n 1"
        ==> let' [fixity L 4 "plus", fixity L 5 "mul"] (num 1)

    it "can parse fixity decl with default fixity in let " $ do
      "let infixl +\nin\n 1" ==> let' [fixity L 9 "+"] (num 1)
      "let infixr +\n    infix *\nin\n 1"
        ==> let' [fixity R 9 "+", fixity M 9 "*"] (num 1)

    it "can parse valid operators" $ do
      "1 + 2" ==> app (var "+") [num 1, num 2]
      "1 + 2 + 3" ==> app (var "+") [app (var "+") [num 1, num 2], num 3]
      "1 + 2 * 3" ==> app (var "*") [app (var "+") [num 1, num 2], num 3]
      "True || False && True"
        ==> app (var "&&") [app (var "||") [con "True", con "False"], con "True"]

    it "can parse expressions with mix of parentheses and operators" $
      "1 + (2 + 3)" ==> app (var "+") [num 1, app (var "+") [num 2, num 3]]

    it "can parse expressions with function application and operators" $ do
      "f 1 + a f 2" ==> app (var "+") [ app (var "f") [num 1]
                                      , app (var "a") [var "f", num 2]]
      "f 1 + g (2 + 3)" ==> app (var "+") [ app (var "f") [num 1]
                                          , app (var "g") [app (var "+") [num 2, num 3]]
                                          ]
      "f 1 + g 2 * h 3"
        ==> app (var "*") [ app (var "+") [app (var "f") [num 1], app (var "g") [num 2]]
                          , app (var "h") [num 3]]

    it "can parse operators as return values" $
      "(+)" ==> var "+"

    it "can parse infix functions" $ do
      "1 `plus` 2" ==> app (var "plus") [num 1, num 2]
      "f 1 `Plus` a f 2" ==> app (con "Plus") [ app (var "f") [num 1]
                                              , app (var "a") [var "f", num 2]]
      "f 1 `plus` g 2 `Mul` h 3"
        ==> app (con "Mul") [ app (var "plus") [ app (var "f") [num 1]
                                               , app (var "g") [num 2]]
                            , app (var "h") [num 3]]

    it "fails with readable error message" $ do
      (parse, "(+") `shouldFailWith` err 2 (ueof <> etok ')' <> elabel "rest of operator")
      (parse, "1 `") `shouldFailWith` err 3
        (ueof <> elabel "infix constructor" <> elabel "infix function")
      (parse, "1 `plu") `shouldFailWith` err 6 (ueof <> etok '`' <> elabel "rest of identifier")
      (parse, "1 +") `shouldFailWith` err 3
        (ueof <> elabel "expression" <> elabel "rest of operator")
      (parser, "1 f") `succeedsLeaving` "f"
      (parse, "1 `if` 2") `shouldFailWith` errFancy 5 (failMsg "Reserved keyword: if")
      (parse, "1 + + 2") `shouldFailWith` err 4 (utoks "+ 2" <> elabel "expression")

  it "can parse variables" $ do
    let a ==> b = parse a `shouldParse` E1Var (Id b)
    "a" ==> "a"
    "abc123" ==> "abc123"
    "a'" ==> "a'"
    "a'b" ==> "a'b"

  it "can parse constructors" $ do
    let a ==> b = parse a `shouldParse` b
        con = E1Con . Id
        var = E1Var . Id
        app = E1App
    "True" ==> con "True"
    "X y" ==> app (con "X") [var "y"]
    "Abc123 y" ==> app (con "Abc123") [var "y"]
    "Abc123 y z" ==> app (con "Abc123") [var "y", var "z"]

