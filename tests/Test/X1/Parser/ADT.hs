{-# LANGUAGE GADTs #-}

module Test.X1.Parser.ADT ( module Test.X1.Parser.ADT ) where

import Protolude hiding ( Type )
import Test.Hspec.Megaparsec hiding ( shouldFailWith, succeedsLeaving )
import Test.X1.Parser.Helpers
import Test.Tasty.Hspec
import X1.Parser.ADT (parser)
import X1.Types.Id
import X1.Types.Ann
import X1.Types.Span
import X1.Types.Expr1.ADT
import X1.Types.Expr1.Type


type ADT' = ADT 'Parsed
type ADTHead' = ADTHead 'Parsed
type ADTBody' = ADTBody 'Parsed
type ConDecl' = ConDecl 'Parsed
type Tycon' = Tycon 'Parsed
type Tyvar' = Tyvar 'Parsed
type Type' = Type 'Parsed

con :: Span -> Text -> Tycon'
con sp = Tycon sp . Id

var :: Span -> Text -> Tyvar'
var sp = Tyvar sp . Id

c :: Span -> Text -> Type'
c sp = TCon . con sp

v :: Span -> Text -> Type'
v sp = TVar . var sp

arr :: Span -> Type' -> Type' -> Type'
arr sp t1 t2 = app (c sp "->") [t1, t2]

app :: Type' -> [Type'] -> Type'
app = TApp

parens :: Span -> Type' -> Type'
parens = TParen

hd :: Span -> Text -> [Tyvar'] -> ADTHead'
hd sp constr = ADTHead (Tycon sp (Id constr))

body :: Span -> Text -> [Type'] -> ConDecl'
body sp constr = ConDecl sp (Id constr)

adt :: Span -> ADTHead' -> ADTBody' -> ADT'
adt = ADT

parse :: Text -> ParseResult ADT'
parse = mkParser parser

(==>) :: Text -> ADT' -> IO ()
a ==> b = parse a `shouldParse` b


spec_adtParseTest :: Spec
spec_adtParseTest = describe "ADT parser" $ parallel $ do
  it "can parse an ADT with empty body" $ do
    "data X " ==> adt (Span 0 6) (hd (Span 5 6) "X" []) []
    "data Abc123" ==> adt (Span 0 11) (hd (Span 5 11) "Abc123" []) []
    "data X y z "
      ==> adt (Span 0 10) (hd (Span 5 6) "X"
            [var (Span 7 8) "y", var (Span 9 10) "z"]) []

  it "can parse an ADT with 1 body" $ do
    let maybeInt = app (c (Span 12 17) "Maybe") [c (Span 18 21) "Int"]
        complexType = app (v (Span 25 26) "a")
          [ c (Span 27 30) "Int"
          , parens (Span 31 39) $ arr (Span 34 36) (v (Span 32 33) "b") (v (Span 37 38) "c")]
    "data Abc = Def "
      ==> adt (Span 0 14) (hd (Span 5 8) "Abc" []) [body (Span 11 14) "Def" []]
    "data Abc = Def X Y Z "
      ==> adt (Span 0 20) (hd (Span 5 8) "Abc" [])
          [body (Span 11 20) "Def" [c (Span 15 16) "X", c (Span 17 18) "Y", c (Span 19 20) "Z"]]
    "data X = X (Maybe Int -> a Int (b -> c)) "
      ==> adt (Span 0 40) (hd (Span 5 6) "X" [])
          [body (Span 9 40) "X" [parens (Span 11 40) $ arr (Span 22 24) maybeInt complexType]]

  it "can parse an ADT with multiple bodies" $ do
    "data A = A B C | D E F | G H I "
      ==> adt (Span 0 30) (hd (Span 5 6) "A" [])
          [ body (Span 9 14) "A" [c (Span 11 12) "B", c (Span 13 14) "C"]
          , body (Span 17 22) "D" [c (Span 19 20) "E", c (Span 21 22) "F"]
          , body (Span 25 30) "G" [c (Span 27 28) "H", c (Span 29 30) "I"]
          ]
    "data A = A B C | D (e -> f) | G H I"
      ==> adt (Span 0 35) (hd (Span 5 6) "A" [])
          [ body (Span 9 14) "A" [c (Span 11 12) "B", c (Span 13 14) "C"]
          , body (Span 17 27) "D" [parens (Span 19 27) $ arr (Span 22 24) (v (Span 20 21) "e")
                                                             (v (Span 25 26) "f")]
          , body (Span 30 35) "G" [c (Span 32 33) "H", c (Span 34 35) "I"]
          ]

  it "can parse ADT containing type variables" $ do
    "data Maybe a = Nothing | Just a "
      ==> adt (Span 0 31) (hd (Span 5 10) "Maybe" [var (Span 11 12) "a"])
          [body (Span 15 22) "Nothing" [], body (Span 25 31) "Just" [v (Span 30 31) "a"]]
    "data Either a b = Left a | Right b"
      ==> adt (Span 0 34) (hd (Span 5 11) "Either" [var (Span 12 13) "a", var (Span 14 15) "b"])
          [ body (Span 18 24) "Left" [v (Span 23 24) "a"]
          , body (Span 27 34) "Right" [v (Span 33 34) "b"]]

  it "can parse multiline ADT" $ do
    "data\n Maybe a = Nothing | Just a"
      ==> adt (Span 0 32) (hd (Span 6 11) "Maybe" [var (Span 12 13) "a"])
          [body (Span 16 23) "Nothing" [], body (Span 26 32) "Just" [v (Span 31 32) "a"]]
    "data Maybe a = Nothing\n | Just a"
      ==> adt (Span 0 32) (hd (Span 5 10) "Maybe" [var (Span 11 12) "a"])
          [body (Span 15 22) "Nothing" [], body (Span 26 32) "Just" [v (Span 31 32) "a"]]
    "data Maybe a = Nothing |\n Just a"
      ==> adt (Span 0 32) (hd (Span 5 10) "Maybe" [var (Span 11 12) "a"])
          [body (Span 15 22) "Nothing" [], body (Span 26 32) "Just" [v (Span 31 32) "a"]]

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
