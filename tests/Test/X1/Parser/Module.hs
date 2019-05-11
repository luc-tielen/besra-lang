
module Test.X1.Parser.Module ( module Test.X1.Parser.Module ) where

import Protolude hiding ( Type, Fixity )
import Test.Tasty.Hspec
import Test.X1.Parser.Helpers
import X1.Types.Id
import X1.Types.Fixity
import X1.Types.Module
import X1.Types.Expr1
import X1.Types.Expr1.Lit
import X1.Types.Expr1.Pred
import X1.Types.Expr1.Type
import X1.Types.Expr1.Number
import X1.Types.Expr1.String
import X1.Types.Expr1.Scheme
import X1.Types.Expr1.Pattern
import X1.Parser.Module (parser)
import Test.Hspec.Megaparsec hiding (shouldFailWith)


parse :: Text -> ParseResult (Module Decl)
parse = mkParser parser

con :: Text -> Type
con = TCon . Tycon . Id

var :: Text -> Type
var = TVar . Tyvar . Id

app :: Type -> [Type] -> Type
app = TApp

(==>) :: Text -> Module Decl -> IO ()
a ==> b = parse a `shouldParse` b

(-->) :: Type -> Type -> Type
t1 --> t2 = app (con "->") [t1, t2]

num :: Int -> Expr1
num = E1Lit . LNumber . SInt

str :: Text -> Expr1
str = E1Lit . LString . String

char :: Char -> Expr1
char = E1Lit . LChar

lam :: [Text] -> Expr1 -> Expr1
lam vars = E1Lam (PVar . Id <$> vars)


infixr 2 -->
infixr 1 ==>


spec_moduleParseTest :: Spec
spec_moduleParseTest = describe "module parser" $ parallel $ do
  it "can parse empty module" $
    "" ==> Module []

  it "ignores whitespace at the beginning of a file" $
    "    \n\n   \n  \nx : Int"
      ==> Module [TypeDecl (Id "x") (Scheme [] $ con "Int")]

  it "ignores whitespace at the end of a file" $
    "x : Int    \n\n   \n  \n"
      ==> Module [TypeDecl (Id "x") (Scheme [] $ con "Int")]

  it "can parse multiple type level declarations" $ do
    "x : Int\nx = 5" ==> Module [ TypeDecl (Id "x") (Scheme [] $ con "Int")
                                , BindingDecl (Id "x") (num 5) ]
    "x = 5\nx : Int" ==> Module [ BindingDecl (Id "x") (num 5)
                                , TypeDecl (Id "x") (Scheme [] $ con "Int") ]

  describe "type declarations" $ parallel $ do
    it "can parse top level type declaration" $ do
      "x : Int" ==> Module [TypeDecl (Id "x") (Scheme [] $ con "Int")]
      "x : a" ==> Module [TypeDecl (Id "x") (Scheme [] $ var "a")]
      "x : Int -> Int" ==> Module [TypeDecl (Id "x") (Scheme [] $ con "Int" --> con "Int")]

    it "can parse multiple type level declarations" $ do
      "x : Int \ny : String" ==> Module [ TypeDecl (Id "x") (Scheme [] $ con "Int")
                                      , TypeDecl (Id "y") (Scheme [] $ con "String")]
      "x : Int\ny : String" ==> Module [ TypeDecl (Id "x") (Scheme [] $ con "Int")
                                      , TypeDecl (Id "y") (Scheme [] $ con "String")]

    it "can parse multi-line declarations" $ do
      "x\n :\n Int" ==> Module [TypeDecl (Id "x") (Scheme [] $ con "Int")]
      "x\n :\n Int -> Int" ==> Module [TypeDecl (Id "x") (Scheme [] $ con "Int" --> con "Int")]
      "x\n :\n Eq a => a -> a" ==> Module [TypeDecl (Id "x")
                                    (Scheme [IsIn (Id "Eq") [var "a"]] $ var "a" --> var "a")]

    it "can handle linefolds in type signatures correctly" $ do
      "x : Int -> Int\ny : String" ==> Module [ TypeDecl (Id "x") (Scheme [] $ con "Int" --> con "Int")
                                      , TypeDecl (Id "y") (Scheme [] $ con "String")]
      "x : Int \n  -> Int\ny : String" ==> Module [ TypeDecl (Id "x") (Scheme [] $ con "Int" --> con "Int")
                                      , TypeDecl (Id "y") (Scheme [] $ con "String")]
      "x : Int ->\n Int\ny : String" ==> Module [ TypeDecl (Id "x") (Scheme [] $ con "Int" --> con "Int")
                                      , TypeDecl (Id "y") (Scheme [] $ con "String")]
      "x\n : Int \n ->\n Int\ny : String" ==> Module [ TypeDecl (Id "x") (Scheme [] $ con "Int" --> con "Int")
                                      , TypeDecl (Id "y") (Scheme [] $ con "String")]
      "x :\n Int \n ->\n Int\ny : String" ==> Module [ TypeDecl (Id "x") (Scheme [] $ con "Int" --> con "Int")
                                      , TypeDecl (Id "y") (Scheme [] $ con "String")]

  describe "binding declarations" $ parallel $ do
    it "can parse top level constants" $ do
      "x = 5" ==> Module [BindingDecl (Id "x") $ num 5]
      "x = \"abc123\"" ==> Module [BindingDecl (Id "x") $ str "abc123"]
      "x = 'a'" ==> Module [BindingDecl (Id "x") $ char 'a']

    it "can parse an assignment spanning multiple lines" $ do
      "x =\n 5" ==> Module [BindingDecl (Id "x") $ num 5]
      "x\n =\n 5" ==> Module [BindingDecl (Id "x") $ num 5]

    it "can parse multiple assignments" $
      "x = 5\ny = \"abc123\"\nz = 'a'" ==> Module [ BindingDecl (Id "x") $ num 5
                                                  , BindingDecl (Id "y") $ str "abc123"
                                                  , BindingDecl (Id "z") $ char 'a']

    it "can parse top level named functions" $ do
      "f x = 5" ==> Module [BindingDecl (Id "f") $ lam ["x"] (num 5)]
      "f x y = \"abc123\"" ==> Module [BindingDecl (Id "f") $ lam ["x", "y"] (str "abc123")]

    it "can parse a named function spanning multiple lines" $ do
      "f x =\n 5" ==> Module [BindingDecl (Id "f") $ lam ["x"] (num 5)]
      "f x y =\n \"abc123\"" ==> Module [BindingDecl (Id "f") $ lam ["x", "y"] (str "abc123")]

    it "can parse a named function containing patterns" $ do
      "f 1 \"abc\" = 123" ==> Module [BindingDecl (Id "f") (E1Lam [ PLit (LNumber (SInt 1))
                                                                  , PLit (LString (String "abc"))]
                                                                  (num 123))]
      "f a@(X y) = 123" ==> Module [BindingDecl (Id "f") (E1Lam [ PAs (Id "a")
                                                                  (PCon (Id "X") [PVar (Id "y")])]
                                                                  (num 123))]

    it "can parse multiple named functions" $
      "f x = 5\ng x y = \"abc123\""
        ==> Module [ BindingDecl (Id "f") $ lam ["x"] (num 5)
                   , BindingDecl (Id "g") $ lam ["x", "y"] (str "abc123")
                   ]

  it "fails with readable error message" $ do
    let labels = elabel <$> [ "rest of assignment", "rest of type declaration", "pattern" ]
    (parse, "x -") `shouldFailWith` err 2 (utok '-' <> mconcat labels)
    (parse, "1") `shouldFailWith` err 0 (utok '1' <> elabel "type or binding declaration" <> eeof)

  describe "operators" $ parallel $ do
    describe "fixity declarations" $ parallel $ do
      let expected op fixity prio = Module [FixityDecl fixity prio (Id op)]

      it "can parse top level fixity declarations" $ do
        let expected' = expected "+"
        "infixl 0 +" ==> expected' L 0
        "infixl 5 +" ==> expected' L 5
        "infixl 9 +" ==> expected' L 9
        "infixr 0 +" ==> expected' R 0
        "infixr 5 +" ==> expected' R 5
        "infixr 9 +" ==> expected' R 9
        "infix 0 +" ==> expected' M 0
        "infix 5 +" ==> expected' M 5
        "infix 9 +" ==> expected' M 9

      it "can parse top level fixity decl with default fixity" $ do
        "infixl +" ==> expected "+" L 9
        "infixr +" ==> expected "+" R 9
        "infix +" ==> expected "+" M 9

      it "can parse multiple fixity declarations" $
        "infixl 5 +\ninfixl 6 *"
          ==> Module [FixityDecl L 5 (Id "+"), FixityDecl L 6 (Id "*")]

      it "can parse multiline fixity declaration" $ do
        "infixl\n 7\n  ***" ==> expected "***" L 7
        "infixr\n 7\n ***" ==> expected "***" R 7
        "infix\n 7\n ***" ==> expected "***" M 7
        "infix 7\n ***" ==> expected "***" M 7

      it "fails with readable error message (basic errors)" $ do
        (parse, "infi") `shouldFailWith` err 4
          (ueof <> elabel "pattern" <> elabel "rest of assignment"
          <> elabel "rest of identifier" <> elabel "rest of type declaration" )
        (parse, "infixr 0") `shouldFailWith` err 8 (ueof <> elabel "operator")
        (parse, "infixr -1 +") `shouldFailWith` err 8 (utok '1' <> elabel "rest of operator")
        (parse, "infixr a") `shouldFailWith` err 7 (utok 'a' <> elabel "operator" <> elabel "precedence between 0..9")
        (parse, "infixr 6 a") `shouldFailWith` err 9 (utok 'a' <> elabel "operator")

      it "fails with readable error (bad linefolds)" $ do
        (parse, "infixr\n0") `shouldFailWith` errFancy 7 (badIndent 1 1)
        (parse, "infixr 0\n+") `shouldFailWith` errFancy 9 (badIndent 1 1)

      it "fails with readable error message (out of range precedence)" $ do
        (parse, "infixr 10 +") `shouldFailWith` err 8
          (utok '0' <> elabel "precedence between 0..9")
        (parse, "infixr 11 +") `shouldFailWith` err 8
          (utok '1' <> elabel "precedence between 0..9")

      it "fails with readable error message (reserved keywords)" $ do
        (parse, "infix") `shouldFailWith` errFancy 5 (failMsg "Reserved keyword: infix")
        (parse, "infixl") `shouldFailWith` errFancy 6 (failMsg "Reserved keyword: infixl")
        (parse, "infixr") `shouldFailWith` errFancy 6 (failMsg "Reserved keyword: infixr")

      it "fails with readable error message (reserved operators)" $ do
        (parse, "infixr 0 :") `shouldFailWith` errFancy 10 (failMsg "Reserved operator: ':'")
        (parse, "infixr :") `shouldFailWith` errFancy 8 (failMsg "Reserved operator: ':'")
        (parse, "infixr ..") `shouldFailWith` errFancy 9 (failMsg "Reserved operator: '..'")
        (parse, "infixr =") `shouldFailWith` errFancy 8 (failMsg "Reserved operator: '='")
        (parse, "infixr \\") `shouldFailWith` errFancy 8 (failMsg "Reserved operator: '\\'")
        (parse, "infixr |") `shouldFailWith` errFancy 8 (failMsg "Reserved operator: '|'")
        (parse, "infixr <-") `shouldFailWith` errFancy 9 (failMsg "Reserved operator: '<-'")
        (parse, "infixr ->") `shouldFailWith` errFancy 9 (failMsg "Reserved operator: '->'")
        (parse, "infixr =>") `shouldFailWith` errFancy 9 (failMsg "Reserved operator: '=>'")
        (parse, "infixr @") `shouldFailWith` errFancy 8 (failMsg "Reserved operator: '@'")
        (parse, "infixr ~") `shouldFailWith` errFancy 8 (failMsg "Reserved operator: '~'")

