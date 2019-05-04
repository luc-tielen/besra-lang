
module Test.X1.Parser.Module ( module Test.X1.Parser.Module ) where

import Protolude hiding ( Type )
import Test.Tasty.Hspec
import Test.X1.Parser.Helpers
import X1.Types.Module
import X1.Types.Id
import X1.Types.Expr1
import X1.Types.Lit
import X1.Parser.Types.Number
import X1.Parser.Types.String
import X1.Parser.Types.Scheme
import X1.Parser.Types.Pred
import X1.Parser.Types.Type
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
lam vars = E1Lam (Id <$> vars)


infixr 2 -->
infixr 1 ==>


spec_exprParseTest :: Spec
spec_exprParseTest = describe "module parser" $ parallel $ do
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

    it "can parse multiple named functions" $
      "f x = 5\ng x y = \"abc123\""
        ==> Module [ BindingDecl (Id "f") $ lam ["x"] (num 5)
                   , BindingDecl (Id "g") $ lam ["x", "y"] (str "abc123")
                   ]

  it "fails with readable error message" $ do
    let labels = elabel <$> [ "rest of assignment", "rest of type declaration", "variable" ]
    (parse, "x -") `shouldFailWith` err 2 (utok '-' <> mconcat labels)
    (parse, "1") `shouldFailWith` err 0 (utok '1' <> elabel "type or binding declaration" <> eeof)

