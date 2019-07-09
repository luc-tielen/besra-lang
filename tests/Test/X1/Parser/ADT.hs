{-# LANGUAGE GADTs #-}

module Test.X1.Parser.ADT ( module Test.X1.Parser.ADT ) where

import Protolude hiding ( Type )
import Test.Hspec.Megaparsec hiding ( shouldFailWith, succeedsLeaving )
import Test.X1.Parser.Helpers
import Test.X1.Helpers
import Test.Tasty.Hspec
import X1.Parser.ADT (parser)
import X1.Types.Id
import X1.Types.Ann
import X1.Types.Expr1.ADT
import X1.Types.Expr1.Type


type ADT' = ADT 'Testing
type ADTHead' = ADTHead 'Testing
type ADTBody' = ADTBody 'Testing
type ConDecl' = ConDecl 'Testing
type Tycon' = Tycon 'Testing
type Tyvar' = Tyvar 'Testing
type Type' = Type 'Testing

con :: Text -> Tycon'
con = Tycon emptyAnn . Id

var :: Text -> Tyvar'
var = Tyvar emptyAnn . Id

c :: Text -> Type'
c = TCon . con

v :: Text -> Type'
v = TVar . var

(-->) :: Type' -> Type' -> Type'
t1 --> t2 = app (c "->") [t1, t2]

app :: Type' -> [Type'] -> Type'
app = TApp

hd :: Text -> [Text] -> ADTHead'
hd constr vars = ADTHead (con constr) (var <$> vars)

body :: Text -> [Type'] -> ConDecl'
body constr = ConDecl (Id constr)

adt :: ADTHead' -> ADTBody' -> ADT'
adt = ADT emptyAnn


parse :: Text -> ParseResult (ADT 'Parsed)
parse = mkParser parser

(==>) :: Text -> ADT' -> IO ()
a ==> b = (stripAnns <$> parse a) `shouldParse` b


spec_adtParseTest :: Spec
spec_adtParseTest = describe "ADT parser" $ parallel $ do
  it "can parse an ADT with empty body" $ do
    "data X " ==> adt (hd "X" []) []
    "data Abc123" ==> adt (hd "Abc123" []) []
    "data X y z " ==> adt (hd "X" ["y", "z"]) []

  it "can parse an ADT with 1 body" $ do
    let maybeInt = app (c "Maybe") [c "Int"]
        complexType = app (v "a") [c "Int", v "b" --> v "c"]
    "data Abc = Def " ==> adt (hd "Abc" []) [body "Def" []]
    "data Abc = Def X Y Z "
      ==> adt (hd "Abc" []) [body "Def" $ c <$> ["X", "Y", "Z"]]
    "data X = X (Maybe Int -> a Int (b -> c)) "
      ==> adt (hd "X" []) [body "X" [maybeInt --> complexType]]

  it "can parse an ADT with multiple bodies" $ do
    "data A = A B C | D E F | G H I "
      ==> adt (hd "A" []) [ body "A" $ c <$> ["B", "C"]
                                      , body "D" $ c <$> ["E", "F"]
                                      , body "G" $ c <$> ["H", "I"]
                                      ]
    "data A = A B C | D (e -> f) | G H I"
      ==> adt (hd "A" []) [ body "A" $ c <$> ["B", "C"]
                                      , body "D" [v "e" --> v "f"]
                                      , body "G" $ c <$> ["H", "I"]
                                      ]

  it "can parse ADT containing type variables" $ do
    "data Maybe a = Nothing | Just a "
      ==> adt (hd "Maybe" ["a"]) [body "Nothing" [], body "Just" [v "a"]]
    "data Either a b = Left a | Right b"
      ==> adt (hd "Either" ["a", "b"]) [body "Left" [v "a"], body "Right" [v "b"]]

  it "can parse multiline ADT" $ do
    "data\n Maybe a = Nothing | Just a"
      ==> adt (hd "Maybe" ["a"]) [body "Nothing" [], body "Just" [v "a"]]
    "data Maybe a = Nothing\n | Just a"
      ==> adt (hd "Maybe" ["a"]) [body "Nothing" [], body "Just" [v "a"]]
    "data Maybe a = Nothing |\n Just a"
      ==> adt (hd "Maybe" ["a"]) [body "Nothing" [], body "Just" [v "a"]]

  it "fails with readable error message" $ do
    (parse, "") `shouldFailWith` err 0 (ueof <> etoks "data")
    (parse, "dat") `shouldFailWith` err 0 (utoks "dat" <> etoks "data")
    (parse, "data") `shouldFailWith` err 4 (ueof <> elabel "whitespace")
    (parse, "data ") `shouldFailWith` err 5 (ueof <> elabel "name of datatype")
    (parser, "data X Y") `succeedsLeaving` "Y"
    (parse, "data X = 1 a b") `shouldFailWith` err 9 (utok '1' <> elabel "constructor")
    (parser, "data X = A 1") `succeedsLeaving` "1"
    (parser, "data X = A & 1") `succeedsLeaving` "& 1"
    (parser, "data X = A | B 1") `succeedsLeaving` "1"

