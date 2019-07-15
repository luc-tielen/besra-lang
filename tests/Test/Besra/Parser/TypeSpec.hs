
module Test.Besra.Parser.TypeSpec ( module Test.Besra.Parser.TypeSpec ) where

import Protolude hiding ( Type )
import Test.Hspec.Megaparsec hiding (shouldFailWith)
import Test.Besra.Parser.Helpers
import Test.Besra.Helpers
import Test.Hspec
import Besra.Parser.Type (parser)
import Besra.Types.IR1.Type
import Besra.Types.Id
import Besra.Types.Ann


type Type' = Type 'Testing

parser' :: Text -> ParseResult (Type 'Parsed)
parser' = mkParser parser

(==>) :: Text -> Type' -> IO ()
a ==> b = (stripAnns <$> parser' a) `shouldParse` b

con :: Text -> Type'
con = TCon . Tycon emptyAnn . Id

var :: Text -> Type'
var = TVar . Tyvar emptyAnn . Id

app :: Type' -> [Type'] -> Type'
app = TApp

parens :: Type' -> Type'
parens = TParen emptyAnn

(-->) :: Type' -> Type' -> Type'
t1 --> t2 = app (con "->") [t1, t2]

tMaybe :: Type'
tMaybe = con "Maybe"

tEither :: Type'
tEither = con "Either"

infixr 2 -->
infixr 1 ==>


spec :: Spec
spec = describe "parsing types" $ parallel $ do
  it "can parse concrete types" $ do
    "X" ==> con "X"
    "String" ==> con "String"
    "Int" ==> con "Int"
    "(X)" ==> parens (con "X")

  it "can parse type variables" $ do
    "a" ==> var "a"
    "abc" ==> var "abc"
    "(abc)" ==> parens (var "abc")

  it "can parse types of functions" $ do
    "Int -> Int" ==> (con "Int" --> con "Int")
    "Int -> String" ==> (con "Int" --> con "String")
    "a -> b" ==> (var "a" --> var "b")
    "Int -> Int -> String" ==> (con "Int" --> (con "Int" --> con "String"))
    "(Int -> Int) -> String" ==> (parens (con "Int" --> con "Int") --> con "String")
    "(Int -> Int)" ==> parens (con "Int" --> con "Int")

  it "can parse types containing higher kinded types" $ do
    "Maybe Int" ==> app tMaybe [con "Int"]
    "(Maybe Int)" ==> parens (app tMaybe [con "Int"])
    "Either String Int" ==> app tEither [con "String", con "Int"]
    "f a" ==> app (var "f") [var "a"]
    "Maybe (Maybe Int)" ==> app tMaybe [parens $ app tMaybe [con "Int"]]

  it "can parse a type signature spanning across multiple lines" $ do
    "Maybe a ->\n Maybe String" ==> app tMaybe [var "a"] --> app tMaybe [con "String"]
    "Maybe a ->\n  Maybe String" ==> app tMaybe [var "a"] --> app tMaybe [con "String"]

  it "can parse a mix of everything" $ do
    "Maybe a -> Maybe String" ==> app tMaybe [var "a"] --> app tMaybe [con "String"]
    "Maybe a -> String" ==> app tMaybe [var "a"] --> con "String"
    "Either String a -> Maybe a"
      ==> app tEither [con "String", var "a"] --> app tMaybe [var "a"]

  it "ignores space after open parentheses in type signature" $
    "(   a -> a)" ==> parens (var "a" --> var "a")

  it "ignores space before close parentheses in type signature" $
    "(a -> a  )" ==> parens (var "a" --> var "a")

  it "fails with readable error message" $ do
    (parser', "") `shouldFailWith` err 0 (ueof <> elabel "type")
    (parser', "(") `shouldFailWith` err 1 (ueof <> elabel "type")
    (parser', "()") `shouldFailWith` err 1 (utok ')' <> elabel "type")
    (parser', ")") `shouldFailWith` err 0 (utok ')' <> elabel "type")
    (parser', "(a -> 2)") `shouldFailWith` err 6 (utok '2' <> elabel "type")
    (parser', "(Maybe 2)") `shouldFailWith` err 7
      (utok '2' <> etoks "->" <> etok '(' <> etok ')'
      <> elabel "concrete type" <> elabel "type variable")
    (parser', "Int -> -> String") `shouldFailWith` err 7
      (utok '-' <> elabel "type")

