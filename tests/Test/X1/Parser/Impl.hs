
module Test.X1.Parser.Impl ( module Test.X1.Parser.Impl ) where

import Protolude hiding ( Type, pred )
import Test.Tasty.Hspec
import Test.X1.Parser.Helpers
import Test.X1.Helpers
import X1.Types.Id
import X1.Types.Expr1.Lit
import X1.Types.Expr1.Number
import X1.Types.Expr1.String
import X1.Types.Expr1.Pattern
import X1.Types.Expr1.Pred
import X1.Types.Expr1.Type
import X1.Types.Expr1.Impl
import X1.Types.Expr1.Expr
import X1.Parser.Impl (parser)
import Test.Hspec.Megaparsec hiding (shouldFailWith, succeedsLeaving)


parse :: Text -> ParseResult Impl
parse = mkParser parser

num :: Int -> Expr1
num = E1Lit emptyAnn . LNumber . SInt

str :: Text -> Expr1
str = E1Lit emptyAnn . LString . String

con :: Text -> Type
con = TCon . Tycon . Id

var :: Text -> Type
var = TVar . Tyvar . Id

app :: Type -> [Type] -> Type
app = TApp

(==>) :: Text -> Impl -> IO ()
a ==> b = (stripAnns <$> parse a) `shouldParse` b

(-->) :: Type -> Type -> Type
t1 --> t2 = app (con "->") [t1, t2]

lam :: [Text] -> Expr1 -> Expr1
lam vars = E1Lam (PVar . Id <$> vars)

evar :: Text -> Expr1
evar = E1Var . Id

eapp :: Text -> [Text] -> Expr1
eapp x = E1App (evar x) . map evar

binding :: Text -> Expr1 -> Binding
binding x = Binding (Id x)

pred :: Text -> [Type] -> Pred
pred clazz = IsIn (Id clazz)

infixr 2 -->
infixr 1 ==>


spec_implParseTest :: Spec
spec_implParseTest = describe "impl parser" $ parallel $ do
  it "can parse impl without body" $ do
    "impl Eq Int where" ==> Impl [] (pred "Eq" [con "Int"]) []
    "impl Show String where " ==> Impl [] (pred "Show" [con "String"]) []

  it "can parse impl with no superclasses" $ do
    "impl Default Int where\n def = 0"
      ==> Impl [] (pred "Default" [con "Int"]) [binding "def" $ num 0]
    "impl Show Int where\n show x = \"<Number>\""
      ==> Impl [] (pred "Show" [con "Int"]) [binding "show" (lam ["x"] $ str "<Number>")]
    "impl Show Int where\n show a b c = \"<Number>\""
      ==> Impl [] (pred "Show" [con "Int"]) [binding "show"
                                              (lam ["a", "b", "c"] $ str "<Number>")]
    "impl Num Int where\n (+) = plus\n (*) a b = mul a b"
      ==> Impl [] (pred "Num" [con "Int"]) [ binding "+" (evar "plus")
                                           , binding "*" (lam ["a", "b"] $
                                               eapp "mul" ["a", "b"])
                                           ]

  it "can parse impl with a single superclass" $ do
    let result = Impl [pred "Show" [var "a"]]
                  (pred "Show" [app (con "Maybe") [var "a"]])
                  [binding "show" $ str "Nothing"]
    "impl Show a => Show (Maybe a) where\n show = \"Nothing\"" ==> result
    "impl (Show a) => Show (Maybe a) where\n show = \"Nothing\"" ==> result

  it "can parse impl with multiple superclasses" $ do
    "impl (A a, B a) => C (Maybe a) where\n x = 3"
      ==> Impl [pred "A" [var "a"], pred "B" [var "a"]]
                (pred "C" [app (con "Maybe") [var "a"]])
                [binding "x" $ num 3]
    "impl (A a, B a, C a) => D (Maybe a) where\n x = 3"
      ==> Impl [pred "A" [var "a"], pred "B" [var "a"], pred "C" [var "a"]]
                (pred "D" [app (con "Maybe") [var "a"]])
                [binding "x" $ num 3]
    "impl (A a, B a, C a) => D Maybe a where\n x = 3"
      ==> Impl [pred "A" [var "a"], pred "B" [var "a"], pred "C" [var "a"]]
                (pred "D" [app (con "Maybe") [var "a"]])
                [binding "x" $ num 3]

  it "can parse impl requiring multiple types" $ do
    let pred' x = pred x . map var
    "impl A a b c => B (C a b c) where\n x = 3"
      ==> Impl [pred' "A" ["a", "b", "c"]]
                (pred "B" [app (con "C") $ var <$> ["a", "b", "c"]])
                [binding "x" $ num 3]
    "impl (A a, B b, C c) => D (E a b c) where\n x = 3"
      ==> Impl [pred' "A" ["a"], pred' "B" ["b"], pred' "C" ["c"]]
                (pred "D" [app (con "E") $ var <$> ["a", "b", "c"]])
                [binding "x" $ num 3]

  it "can parse multiline impl" $ do
    "impl (Eq a)\n => Eq (Maybe a)\n where\n x = 3"
      ==> Impl [pred "Eq" [var "a"]]
            (pred "Eq" [app (con "Maybe") $ var <$> ["a"]])
            [binding "x" $ num 3]
    "impl (Eq a) => Eq (Maybe a) where\n x\n  = \n  3"
      ==> Impl [pred "Eq" [var "a"]]
            (pred "Eq" [app (con "Maybe") $ var <$> ["a"]])
            [binding "x" $ num 3]

  it "fails with readable error message when part is missing" $ do
    let impl = elabel "impl declaration"
    (parse, "") `shouldFailWith` err 0 (ueof <> impl)
    (parse, "imp") `shouldFailWith` err 0 (utoks "imp" <> impl)
    (parse, "impl") `shouldFailWith` err 4 (ueof <> elabel "whitespace")
    (parse, "impl ") `shouldFailWith` err 5
      (ueof <> etok '(' <> elabel "trait identifier" <> elabel "typeclass identifier")
    (parse, "impl Eq") `shouldFailWith` err 7
      (ueof <> elabel "rest of identifier" <> elabel "type")
    (parse, "impl Eq a") `shouldFailWith` err 8 (utok 'a' <> elabel "type" )
    (parse, "impl Eq a =>") `shouldFailWith` err 12
      (ueof <> elabel "trait identifier")
    (parse, "impl Eq a => Ord") `shouldFailWith` err 16
      (ueof <> elabel "rest of identifier" <> elabel "type")
    (parse, "impl Eq a => Ord a") `shouldFailWith` err 17 (utok 'a' <> elabel "type")
    (parse, "impl Eq X") `shouldFailWith` err 9
      (ueof <> etoks "where" <> elabel "rest of identifier"
        <> elabel "type" <> elabel "type variable")
    (parse, "impl Ord Int where\n x") `shouldFailWith` err 21
      (ueof <> elabel "pattern" <> elabel "rest of assignment" <> elabel "rest of identifier")

  it "fails with readable error for bad linefolds" $ do
    (parser, "impl Eq X where\nx = 3") `succeedsLeaving` "x = 3"
    (parse, "impl Eq X where\n x\n = 3")
      `shouldFailWith` errFancy 20 (badIndent 2 2)

  it "fails with readable error for badly indented declarations in body" $ do
    (parse, "impl Eq X where\n  x = 3\n y = 4") `shouldFailWith` err 25
      (utok 'y' <> elabel "properly indented binding declaration in impl")
    (parse, "impl Eq X where\n x = 3\n  y = 4") `shouldFailWith` err 25
      (utok 'y' <> elabel "properly indented binding declaration in impl")

