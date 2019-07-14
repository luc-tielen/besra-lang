
module Test.X1.Parser.Scheme ( module Test.X1.Parser.Scheme ) where

import Protolude hiding (Type)
import Test.Hspec.Megaparsec hiding (shouldFailWith)
import Test.Tasty.Hspec
import Test.X1.Parser.Helpers
import Test.X1.Helpers
import X1.Parser.Scheme (parser)
import X1.Types.Id
import X1.Types.Ann
import X1.Types.Expr1.Pred
import X1.Types.Expr1.Scheme
import X1.Types.Expr1.Type


type Scheme' = Scheme 'Testing
type Type' = Type 'Testing
type Pred' = Pred 'Testing

parser' :: Text -> ParseResult (Scheme 'Parsed)
parser' = mkParser parser

(==>) :: Text -> Scheme' -> IO ()
a ==> b = (stripAnns <$> parser' a) `shouldParse` b

con :: Text -> Type'
con = TCon . Tycon emptyAnn . Id

var :: Text -> Type'
var = TVar . Tyvar emptyAnn . Id

app :: Type' -> [Type'] -> Type'
app = TApp

parens :: Type' -> Type'
parens = TParen emptyAnn

isIn :: Text -> [Type'] -> Pred'
isIn ty = IsIn emptyAnn (Id ty)

scheme :: [Pred'] -> Type' -> Scheme'
scheme = Scheme emptyAnn


(-->) :: Type' -> Type' -> Type'
t1 --> t2 = app (con "->") [t1, t2]

infixr 2 -->
infixr 1 ==>


-- NOTE: currently no forall supported in typescheme syntax
spec_typeschemeParseTest :: Spec
spec_typeschemeParseTest = describe "parsing typeschemes" $ parallel $ do
  describe "without typeclasses" $ parallel $ do
    it "can parse typeschemes of concrete types" $ do
      "X" ==> scheme [] (con "X")
      "String" ==> scheme [] (con "String")
      "Int" ==> scheme [] (con "Int")
      "(Int)" ==> scheme [] (parens $ con "Int")
      "((Int))" ==> scheme [] (parens $ parens $ con "Int")

    it "can parse typeschemes of type variables" $ do
      "a" ==> scheme [] (var "a")
      "abc" ==> scheme [] (var "abc")
      "(abc)" ==> scheme [] (parens $ var "abc")
      "((abc))" ==> scheme [] (parens . parens $ var "abc")

    it "can parse typeschemes of functions" $ do
      "Int -> Int" ==> scheme [] (con "Int" --> con "Int")
      "Int -> String" ==> scheme [] (con "Int" --> con "String")
      "a -> b" ==> scheme [] (var "a" --> var "b")
      "Int -> Int -> String" ==> scheme [] (con "Int" --> (con "Int" --> con "String"))

    it "can parse typeschemes containing higher kinded types" $ do
      "Maybe a" ==> scheme [] (app (con "Maybe") [var "a"])
      "f a -> a" ==> scheme [] (app (var "f") [var "a"] --> var "a")

    it "can deal with whitespace in typescheme" $
      "(     a -> b   )" ==> scheme [] (parens $ var "a" --> var "b")

  describe "type classes" $ parallel $ do
    it "can parse a single typeclass constraint in a typescheme" $ do
      "Eq a => a" ==> scheme [isIn "Eq" [var "a"]] (var "a")
      "Eq a =>a" ==> scheme [isIn "Eq" [var "a"]] (var "a")
      "Eq a=> a" ==> scheme [isIn "Eq" [var "a"]] (var "a")
      "Ord a => a" ==> scheme [isIn "Ord" [var "a"]] (var "a")
      "Convert a b => a" ==> scheme [isIn "Convert" [var "a", var "b"]] (var "a")
      "Eq a => a -> a" ==> scheme [isIn "Eq" [var "a"]] (var "a" --> var "a")
      "(Eq a) => a" ==> scheme [isIn "Eq" [var "a"]] (var "a")
      "((Eq a)) => a" ==> scheme [isIn "Eq" [var "a"]] (var "a")

    it "can parse multiple typeclass constraint in a typescheme" $ do
      "(Eq a, Ord a) => a" ==> scheme [isIn "Eq" [var "a"], isIn "Ord" [var "a"]] (var "a")
      "(Eq a,Ord a) => a" ==> scheme [isIn "Eq" [var "a"], isIn "Ord" [var "a"]] (var "a")
      "(Eq a ,Ord a) => a" ==> scheme [isIn "Eq" [var "a"], isIn "Ord" [var "a"]] (var "a")
      "(Eq a, Convert a b) => a"
        ==> scheme [isIn "Eq" [var "a"], isIn "Convert" [var "a", var "b"]] (var "a")
      "(Eq a, Ord a) => a -> a"
        ==> scheme [isIn "Eq" [var "a"], isIn "Ord" [var "a"]] (var "a" --> var "a")
      "(((Eq a)) ,Ord a) => a" ==> scheme [isIn "Eq" [var "a"], isIn "Ord" [var "a"]] (var "a")

    it "can deal with whitespace before constraints" $ do
      "(    Eq a) =>   a" ==> scheme [isIn "Eq" [var "a"]] (var "a")
      "(  ( Eq a) ,  ( Ord a)) => a" ==> scheme [isIn "Eq" [var "a"], isIn "Ord" [var "a"]] (var "a")

    it "can deal with whitespace after constraints" $ do
      "(Eq a  ) =>  a" ==> scheme [isIn "Eq" [var "a"]] (var "a")
      "((Eq a  ),  (Ord a  )  )  => a" ==> scheme [isIn "Eq" [var "a"], isIn "Ord" [var "a"]] (var "a")

  it "can parse a mix of everything" $ do
    "Maybe (a -> b)" ==> scheme [] (app (con "Maybe") [parens $ var "a" --> var "b"])
    "Maybe (Maybe a)" ==> scheme [] (app (con "Maybe") [parens $ app (con "Maybe") [var "a"]])
    "Maybe (Either a String)"
      ==> scheme [] (app (con "Maybe") [parens $ app (con "Either") [var "a", con "String"]])
    "(Eq a) => (Maybe (a -> b))"
      ==> scheme [isIn "Eq" [var "a"]] (parens $ app (con "Maybe") [parens $ var "a" --> var "b"])

  it "fails with readable error message" $ do
    (parser', "") `shouldFailWith` err 0 (ueof <> elabel "typescheme")
    (parser', "(") `shouldFailWith` err 1
      (ueof <> elabel "type")
    (parser', ")") `shouldFailWith` err 0 (utok ')' <> elabel "typescheme")
    (parser', "()") `shouldFailWith` err 1 (utok ')' <> elabel "type")
    (parser', "(a => a)") `shouldFailWith` err 3
      (utok '=' <> etoks "->" <> etok '(' <> etok ')' <>
       elabel "concrete type" <> elabel "type variable")
    (parser', "=> a") `shouldFailWith` err 0 (utok '=' <> elabel "typescheme")
    {- TODO fix it so these fail with clearer error message in the future
    (parser', "MonadReader Env m => a") `shouldFailWith` err 17
      (utok 'E' <> elabel "type variable")
    (parser', "Constraint (Maybe a) => a") `shouldFailWith` err 16
      (utok '(' <> elabel "type variable")
    -}
