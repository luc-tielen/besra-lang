
module Test.Besra.Parser.TraitSpec ( module Test.Besra.Parser.TraitSpec ) where

import Protolude hiding ( Type, pred )
import Test.Hspec.Megaparsec hiding (shouldFailWith, succeedsLeaving)
import Test.Hspec
import Test.Besra.Parser.Helpers
import Test.Besra.Helpers
import Besra.Types.Id
import Besra.Types.Ann
import Besra.Types.IR1 ( Trait(..), Pred(..), Scheme(..), TypeAnn(..)
                       , Type(..), Tycon(..), Tyvar(..) )
import Besra.Parser.Trait (parser)


type Trait' = Trait Testing
type Pred' = Pred Testing
type Type' = Type Testing
type Scheme' = Scheme Testing
type TypeAnn' = TypeAnn Testing

parse :: Text -> ParseResult (Trait Parsed)
parse = mkParser parser

con :: Text -> Type'
con = TCon . Tycon emptyAnn . Id

var :: Text -> Type'
var = TVar . Tyvar emptyAnn . Id

app :: Type' -> [Type'] -> Type'
app = TApp

parens :: Type' -> Type'
parens = TParen emptyAnn

(==>) :: Text -> Trait' -> IO ()
a ==> b = (stripAnns <$> parse a) `shouldParse` b

(-->) :: Type' -> Type' -> Type'
t1 --> t2 = app (con "->") [t1, t2]

typeAnn :: Text -> Scheme' -> TypeAnn'
typeAnn x = TypeAnn emptyAnn (Id x)

pred :: Text -> [Text] -> Pred'
pred clazz = IsIn emptyAnn (Id clazz) . map var

scheme :: [Pred'] -> Type' -> Scheme'
scheme = Scheme emptyAnn

trait :: [Pred'] -> Pred' -> [TypeAnn'] -> Trait'
trait = Trait emptyAnn


infixr 2 -->
infixr 1 ==>


spec :: Spec
spec = describe "trait parser" $ parallel $ do
  it "can parse trait without body" $ do
    "trait Eq a where" ==> trait [] (pred "Eq" ["a"]) []
    "trait Eq a where " ==> trait [] (pred "Eq" ["a"]) []

  it "can parse trait with no superclasses" $ do
    let mapType = parens (var "a" --> var "b")
                --> app (con "List") [var "a"]
                --> app (con "List") [var "b"]
    "trait Eq a where\n x : Int"
      ==> trait [] (pred "Eq" ["a"]) [typeAnn "x" $ scheme [] (con "Int")]
    "trait Eq a where\n x : Int\n y : (a -> b) -> List a -> List b"
      ==> trait [] (pred "Eq" ["a"])
          [ typeAnn "x" $ scheme [] (con "Int")
          , typeAnn "y" $ scheme [] mapType
          ]

  it "can parse trait with a single superclass" $ do
    let tInt = typeAnn "x" $ scheme [] (con "Int")
    "trait Eq a => Ord a where\n x : Int"
      ==> trait [pred "Eq" ["a"]] (pred "Ord" ["a"]) [tInt]
    "trait (Eq a) => Ord a where\n x : Int"
      ==> trait [pred "Eq" ["a"]] (pred "Ord" ["a"]) [tInt]

  it "can parse trait with multiple superclasses" $ do
    let tInt = typeAnn "x" $ scheme [] (con "Int")
    "trait (Eq a, Ord a) => MapLike a where\n x : Int"
      ==> trait [pred "Eq" ["a"], pred "Ord" ["a"]] (pred "MapLike" ["a"]) [tInt]
    "trait (A a, B a, C a) => D a where\n x : Int"
      ==> trait [pred "A" ["a"], pred "B" ["a"], pred "C" ["a"]]
                  (pred "D" ["a"]) [tInt]

  it "can parse trait with multiple type variables" $ do
    let tInt = typeAnn "x" $ scheme [] (con "Int")
    "trait A a b c => B a b c where\n x : Int"
      ==> trait [pred "A" ["a", "b", "c"]] (pred "B" ["a", "b", "c"]) [tInt]
    "trait (A a, B b, C c) => D a b c where\n x : Int"
      ==> trait [pred "A" ["a"], pred "B" ["b"], pred "C" ["c"]]
                  (pred "D" ["a", "b", "c"]) [tInt]

  it "can parse multiline traits" $ do
    let tInt = typeAnn "x" $ scheme [] (con "Int")
    "trait (Eq a)\n => Ord a\n where\n x : Int"
      ==> trait [pred "Eq" ["a"]] (pred "Ord" ["a"]) [tInt]
    "trait (Eq a) => Ord a where\n x\n  : \n  Int"
      ==> trait [pred "Eq" ["a"]] (pred "Ord" ["a"]) [tInt]

  it "fails with readable error message when part is missing" $ do
    let etrait = elabel "trait declaration"
    (parse, "") `shouldFailWith` err 0 (ueof <> etrait)
    (parse, "tra") `shouldFailWith` err 0 (utoks "tra" <> etrait)
    (parse, "trait") `shouldFailWith` err 5 (ueof <> elabel "whitespace")
    (parse, "trait ") `shouldFailWith` err 6
      (ueof <> etok '(' <> elabel "typeclass identifier")
    (parse, "trait Eq") `shouldFailWith` err 8
      (ueof <> elabel "rest of identifier" <> elabel "type variable")
    (parse, "trait Eq a") `shouldFailWith` err 10
      (ueof <> etoks "where" <> elabel "rest of identifier" <> elabel "type variable")
    (parse, "trait Eq a =>") `shouldFailWith` err 13
      (ueof <> elabel "typeclass identifier")
    (parse, "trait Eq a => Ord") `shouldFailWith` err 17
      (ueof <> elabel "rest of identifier" <> elabel "type variable")
    (parse, "trait Eq a => Ord a") `shouldFailWith` err 19
      (ueof <> etoks "where" <> elabel "rest of identifier" <> elabel "type variable")
    (parse, "trait Eq a => Ord a where\n x") `shouldFailWith` err 28
      (ueof <> elabel "rest of identifier" <> elabel "rest of type declaration")

  it "fails with readable error for bad linefolds" $ do
    (parser, "trait Eq a => Ord a where\nx : Int") `succeedsLeaving` "x : Int"
    (parse, "trait Eq a => Ord a where\n x\n : Int")
      `shouldFailWith` errFancy 30 (badIndent 2 2)

  it "fails with readable error for badly indented declarations in body" $ do
    (parse, "trait Eq a where\n  x : Int\n y : String") `shouldFailWith` err 28
      (utok 'y' <> elabel "properly indented type declaration in trait")
    (parse, "trait Eq a where\n x : Int\n  y : String") `shouldFailWith` err 30
      (utok ':' <> elabel "properly indented type declaration in trait")
    (parser, "trait Eq a where\n x : Int\ny : String")
      `succeedsLeaving` "y : String"
