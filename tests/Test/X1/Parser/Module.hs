
module Test.X1.Parser.Module ( module Test.X1.Parser.Module ) where

import Protolude hiding ( Type )
import Test.Tasty.Hspec
import Test.X1.Parser.Helpers
import X1.Types.Module
import X1.Types.Id
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

infixr 2 -->
infixr 1 ==>


spec_exprParseTest :: Spec
spec_exprParseTest = describe "module parser" $ parallel $ do
  it "can parse empty module" $
    "" ==> Module []

  it "ignores whitespace at the beginning of a file" $
    "    \n\n   \n  \nx : Int"
      ==> Module [TypeDecl (Id "x") (Scheme [] $ con "Int")]

  it "ignores whitespace at the end of a file" $ do
    "x : Int    \n\n   \n  \n"
      ==> Module [TypeDecl (Id "x") (Scheme [] $ con "Int")]

  it "can parse top level type declaration" $ do
    "x : Int" ==> Module [TypeDecl (Id "x") (Scheme [] $ con "Int")]
    "x : a" ==> Module [TypeDecl (Id "x") (Scheme [] $ var "a")]
    "x : Int -> Int" ==> Module [TypeDecl (Id "x") (Scheme [] $ con "Int" --> con "Int")]

  it "can parse multiple top level declarations" $ do
    "x : Int \ny : String" ==> Module [ TypeDecl (Id "x") (Scheme [] $ con "Int")
                                     , TypeDecl (Id "y") (Scheme [] $ con "String")]
    "x : Int\ny : String" ==> Module [ TypeDecl (Id "x") (Scheme [] $ con "Int")
                                     , TypeDecl (Id "y") (Scheme [] $ con "String")]
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

  it "can parse multi-line declarations" $ do
    "x\n :\n Int" ==> Module [TypeDecl (Id "x") (Scheme [] $ con "Int")]
    "x\n :\n Int -> Int" ==> Module [TypeDecl (Id "x") (Scheme [] $ con "Int" --> con "Int")]
    "x\n :\n Eq a => a -> a" ==> Module [TypeDecl (Id "x")
                                  (Scheme [IsIn (Id "Eq") [var "a"]] $ var "a" --> var "a")]

  -- TODO
  --it "fails with readable error message" $ do
  --  pending
