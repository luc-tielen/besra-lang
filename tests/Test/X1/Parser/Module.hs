
{-# LANGUAGE QuasiQuotes #-}

module Test.X1.Parser.Module ( module Test.X1.Parser.Module ) where

import Protolude hiding ( Type, Fixity, pred )
import Test.Tasty.Hspec
import Test.X1.Parser.Helpers
import Test.X1.Helpers
import X1.Types.Id
import X1.Types.Ann
import X1.Types.Span
import X1.Types.Fixity
import X1.Types.Expr1.Module
import X1.Types.Expr1.Expr
import X1.Types.Expr1.ADT
import X1.Types.Expr1.Lit
import X1.Types.Expr1.Pred
import X1.Types.Expr1.Type
import X1.Types.Expr1.Number
import X1.Types.Expr1.String
import X1.Types.Expr1.Scheme
import X1.Types.Expr1.Pattern
import X1.Types.Expr1.TypeAnn
import X1.Types.Expr1.Trait
import X1.Types.Expr1.Impl
import X1.Parser.Module (parser)
import Test.Hspec.Megaparsec hiding (shouldFailWith)
import NeatInterpolation


type Module' = Module 'Testing
type Expr1' = Expr1 'Testing
type Decl' = Decl 'Testing

parse :: Text -> ParseResult (Module 'Parsed)
parse = mkParser parser

(==>) :: Text -> Module' -> IO ()
a ==> b = (stripAnns <$> parse a) `shouldParse` b


(~~>) :: Text -> Module 'Parsed -> IO ()
a ~~> b = parse a `shouldParse` b

con :: Text -> Type
con = TCon . Tycon . Id

var :: Text -> Type
var = TVar . Tyvar . Id

app :: Type -> [Type] -> Type
app = TApp

(-->) :: Type -> Type -> Type
t1 --> t2 = app (con "->") [t1, t2]

num :: Int -> Expr1'
num = E1Lit emptyAnn . LNumber . SInt

str :: Text -> Expr1'
str = E1Lit emptyAnn . LString . String

char :: Char -> Expr1'
char = E1Lit emptyAnn . LChar

lam :: [Text] -> Expr1' -> Expr1'
lam vars = E1Lam emptyAnn (PVar . Id <$> vars)

typeAnn :: Id -> Scheme -> Decl'
typeAnn name scheme = TypeAnnDecl (TypeAnn name scheme)

binding :: Text -> Expr1' -> Decl'
binding x = BindingDecl . Binding emptyAnn (Id x)


infixr 2 -->
infixr 1 ==>


spec_moduleParseTest :: Spec
spec_moduleParseTest = describe "module parser" $ parallel $ do
  it "can parse empty module" $
    "" ==> Module []

  it "ignores whitespace at the beginning of a file" $
    "    \n\n   \n  \nx : Int"
      ==> Module [typeAnn (Id "x") (Scheme [] $ con "Int")]

  it "ignores whitespace at the end of a file" $
    "x : Int    \n\n   \n  \n"
      ==> Module [typeAnn (Id "x") (Scheme [] $ con "Int")]

  it "can parse multiple type level declarations" $ do
    "x : Int\nx = 5" ==> Module [ typeAnn (Id "x") (Scheme [] $ con "Int")
                                , binding "x" (num 5) ]
    "x = 5\nx : Int" ==> Module [ binding "x" (num 5)
                                , typeAnn (Id "x") (Scheme [] $ con "Int") ]

  describe "type declarations" $ parallel $ do
    it "can parse top level type declaration" $ do
      "x : Int" ==> Module [typeAnn (Id "x") (Scheme [] $ con "Int")]
      "x : a" ==> Module [typeAnn (Id "x") (Scheme [] $ var "a")]
      "x : Int -> Int" ==> Module [typeAnn (Id "x") (Scheme [] $ con "Int" --> con "Int")]

    it "can parse multiple type level declarations" $ do
      "x : Int \ny : String" ==> Module [ typeAnn (Id "x") (Scheme [] $ con "Int")
                                        , typeAnn (Id "y") (Scheme [] $ con "String")]
      "x : Int\ny : String" ==> Module [ typeAnn (Id "x") (Scheme [] $ con "Int")
                                       , typeAnn (Id "y") (Scheme [] $ con "String")]

    it "can parse multi-line declarations" $ do
      "x\n :\n Int" ==> Module [typeAnn (Id "x") (Scheme [] $ con "Int")]
      "x\n :\n Int -> Int" ==> Module [typeAnn (Id "x") (Scheme [] $ con "Int" --> con "Int")]
      "x\n :\n Eq a => a -> a" ==> Module [typeAnn (Id "x")
                                    (Scheme [IsIn (Id "Eq") [var "a"]] $ var "a" --> var "a")]

    it "can handle linefolds in type signatures correctly" $ do
      "x : Int -> Int\ny : String" ==> Module [ typeAnn (Id "x") (Scheme [] $ con "Int" --> con "Int")
                                      , typeAnn (Id "y") (Scheme [] $ con "String")]
      "x : Int \n  -> Int\ny : String" ==> Module [ typeAnn (Id "x") (Scheme [] $ con "Int" --> con "Int")
                                      , typeAnn (Id "y") (Scheme [] $ con "String")]
      "x : Int ->\n Int\ny : String" ==> Module [ typeAnn (Id "x") (Scheme [] $ con "Int" --> con "Int")
                                      , typeAnn (Id "y") (Scheme [] $ con "String")]
      "x\n : Int \n ->\n Int\ny : String" ==> Module [ typeAnn (Id "x") (Scheme [] $ con "Int" --> con "Int")
                                      , typeAnn (Id "y") (Scheme [] $ con "String")]
      "x :\n Int \n ->\n Int\ny : String" ==> Module [ typeAnn (Id "x") (Scheme [] $ con "Int" --> con "Int")
                                      , typeAnn (Id "y") (Scheme [] $ con "String")]

  describe "binding declarations" $ parallel $ do
    it "can parse top level constants" $ do
      "x = 5" ==> Module [binding "x" $ num 5]
      "x = \"abc123\"" ==> Module [binding "x" $ str "abc123"]
      "x = 'a'" ==> Module [binding "x" $ char 'a']

    it "can parse an assignment spanning multiple lines" $ do
      "x =\n 5" ==> Module [binding "x" $ num 5]
      "x\n =\n 5" ==> Module [binding "x" $ num 5]

    it "can parse multiple assignments" $
      "x = 5\ny = \"abc123\"\nz = 'a'" ==> Module [ binding "x" $ num 5
                                                  , binding "y" $ str "abc123"
                                                  , binding "z" $ char 'a']

    it "can parse top level named functions" $ do
      "f x = 5" ==> Module [binding "f" $ lam ["x"] (num 5)]
      "f x y = \"abc123\"" ==> Module [binding "f" $ lam ["x", "y"] (str "abc123")]

    it "can parse a named function spanning multiple lines" $ do
      "f x =\n 5" ==> Module [binding "f" $ lam ["x"] (num 5)]
      "f x y =\n \"abc123\"" ==> Module [binding "f" $ lam ["x", "y"] (str "abc123")]

    it "can parse a named function containing patterns" $ do
      "f 1 \"abc\" = 123"
        ==> Module [binding "f" (E1Lam emptyAnn [ PLit (LNumber (SInt 1))
                                                , PLit (LString (String "abc"))] (num 123))]
      "f a@(X y) = 123"
        ==> Module [binding "f" (E1Lam emptyAnn [ PAs (Id "a")
                                                  (PCon (Id "X") [PVar (Id "y")])] (num 123))]

    it "can parse multiple named functions" $
      "f x = 5\ng x y = \"abc123\""
        ==> Module [ binding "f" $ lam ["x"] (num 5)
                , binding "g" $ lam ["x", "y"] (str "abc123")
                ]

  it "fails with readable error message" $ do
    let labels = mconcat $ elabel <$> ["pattern", "rest of assignment", "rest of type declaration"]
    (parse, "x -") `shouldFailWith` err 2 (utok '-' <> labels)
    (parse, "1") `shouldFailWith` err 0 (utok '1' <> elabel "declaration" <> eeof)

  describe "operators" $ parallel $ do
    let v = E1Var emptyAnn . Id
        plusBinding = v "primitivePlus"
        complexBinding = lam ["a", "b"] $ E1App emptyAnn plusBinding [v "a", v "b"]

    it "can parse a top level type declaration for an operator" $ do
      "(+) : Int -> Int -> Int"
        ==> Module [typeAnn (Id "+") (Scheme [] $ con "Int" --> con "Int" --> con "Int")]
      "(==) : Eq a => a -> a -> a"
        ==> Module [typeAnn (Id "==")
                    (Scheme [IsIn (Id "Eq") [var "a"]] $
                      var "a" --> var "a" --> var "a")]

    it "can parse a top level prefix binding declaration for an operator" $ do
      "(+) = primitivePlus" ==> Module [binding "+" plusBinding]
      "(+) a b = primitivePlus a b"
        ==> Module [binding "+" complexBinding]

    it "fails with readable error message" $ do
      (parse, "(x) = 1") `shouldFailWith` err 1 (utok 'x' <> elabel "operator")
      (parse, "(x) a = 1") `shouldFailWith` err 1 (utok 'x' <> elabel "operator")
      (parse, "(") `shouldFailWith` err 1 (ueof <> elabel "operator")
      (parse, "(+") `shouldFailWith` err 2 (ueof <> etok ')' <> elabel "rest of operator")
      (parse, "(+)") `shouldFailWith` err 3
        (ueof <> elabel "pattern" <> elabel "rest of assignment"
              <> elabel "rest of type declaration")
      (parse, "(+) =") `shouldFailWith` err 5 (ueof <> elabel "expression")
      (parse, "(+) :") `shouldFailWith` err 5 (ueof <> elabel "typescheme")

    describe "fixity declarations" $ parallel $ do
      let expected op fixity prio = Module [FixityDecl emptyAnn fixity prio (Id op)]

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
        "infix 9 `plus`" ==> expected "plus" M 9

      it "can parse multiple fixity declarations" $
        "infixl 5 +\ninfixr 7 `plus`\ninfixl 6 *"
          ==> Module [ FixityDecl emptyAnn L 5 (Id "+")
                     , FixityDecl emptyAnn R 7 (Id "plus")
                     , FixityDecl emptyAnn L 6 (Id "*")]

      it "can parse multiline fixity declaration" $ do
        "infixl\n 7\n  ***" ==> expected "***" L 7
        "infixr\n 7\n ***" ==> expected "***" R 7
        "infix\n 7\n ***" ==> expected "***" M 7
        "infix 7\n ***" ==> expected "***" M 7
        "infix 7\n `mul`" ==> expected "mul" M 7

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

  describe "data declarations" $ parallel $ do
    let hd constr vars = ADTHead (con' constr) (var' <$> vars)
        con' = Tycon . Id
        var' = Tyvar . Id
        body constr = ConDecl (Id constr)
        adt adtHead adtBody = DataDecl (ADT emptyAnn adtHead adtBody)

    it "can parse multiple ADTs in a row" $ do
      "data X\ndata Y" ==> Module [adt (hd "X" []) [], adt (hd "Y" []) []]
      "data X a b\ndata Y c d" ==> Module [ adt (hd "X" ["a", "b"]) []
                                          , adt (hd "Y" ["c", "d"]) []]
      "data X = X\ndata Y = Y" ==> Module [ adt (hd "X" []) [body "X" []]
                                          , adt (hd "Y" []) [body "Y" []]]
      "data X = X Y Z\ndata A = A (b -> c)\ndata D = F"
        ==> Module [ adt (hd "X" []) [body "X" [con "Y", con "Z"]]
                   , adt (hd "A" []) [body "A" [var "b" --> var "c"]]
                   , adt (hd "D" []) [body "F" []]
                   ]

    it "can parse ADT followed by binding declaration" $
      "data X\na = X"
        ==> Module [ adt (hd "X" []) []
                   , BindingDecl $ Binding emptyAnn (Id "a") $ E1Con emptyAnn (Id "X")
                   ]

    it "fails with readable error message" $ do
      (parse, "dat") `shouldFailWith` err 3
        (ueof <> elabel "pattern" <> elabel "rest of assignment"
        <> elabel "rest of type declaration" <> elabel "rest of identifier")
      (parse, "data") `shouldFailWith` err 4 (ueof <> elabel "whitespace")
      (parse, "data ") `shouldFailWith` err 5 (ueof <> elabel "name of datatype")
      (parse, "data\nX = X") `shouldFailWith` errFancy 5 (badIndent 1 1)
      (parse, "data X\n= X") `shouldFailWith` err 7 (utok '=' <> elabel "declaration" <> eeof)
      (parse, "data X a\n= X") `shouldFailWith` err 9 (utok '=' <> elabel "declaration" <> eeof)
      (parse, "data X a =\nX") `shouldFailWith` errFancy 11 (badIndent 1 1)
      (parse, "data X a = X\na") `shouldFailWith` err 14
        (ueof <> elabel "pattern" <> elabel "rest of assignment"
        <> elabel "rest of identifier" <> elabel "rest of type declaration")

  describe "trait declarations" $ parallel $ do
    let trait p typeAnns = TraitDecl $ Trait [] p typeAnns
        pred clazz = IsIn (Id clazz) . map var
        typeAnn' x = TypeAnn (Id x)

    it "can parse multiple traits in a row" $ do
      let mapType = (var "a" --> var "b")
                  --> app (con "List") [var "a"]
                  --> app (con "List") [var "b"]
      [text|
        trait Eq a where
          x : Int
        trait Eq a where
          x : Int
          y : (a -> b) -> List a -> List b
        |] ==> Module [ trait (pred "Eq" ["a"]) [typeAnn' "x" $ Scheme [] (con "Int")]
                      , trait (pred "Eq" ["a"])
                           [ typeAnn' "x" $ Scheme [] (con "Int")
                           , typeAnn' "y" $ Scheme [] mapType
                           ]
                   ]
      [text|
        trait Eq a where
        trait Eq a where
        |] ==> Module [ trait (pred "Eq" ["a"]) []
                      , trait (pred "Eq" ["a"]) []
                      ]

    it "can parse trait followed by other declaration" $ do
      [text|
        trait Eq a where
          x : Int
        a = 3
        |] ==> Module [ trait (pred "Eq" ["a"]) [typeAnn' "x" $ Scheme [] (con "Int")]
                      , BindingDecl $ Binding emptyAnn (Id "a") $ num 3]
      [text|
        trait Eq a where
          x : Int
        a : Int
        |] ==> Module [ trait (pred "Eq" ["a"]) [typeAnn' "x" $ Scheme [] (con "Int")]
                      , typeAnn (Id "a") $ Scheme [] (con "Int")]
      [text|
        trait Eq a where
        a = 3
        |] ==> Module [ trait (pred "Eq" ["a"]) []
                      , BindingDecl $ Binding emptyAnn (Id "a") $ num 3]
      [text|
        trait Eq a where
        a : Int
        |] ==> Module [ trait (pred "Eq" ["a"]) []
                      , typeAnn (Id "a") $ Scheme [] (con "Int")]


    it "fails with readable error message" $ do
      (parse, "tra") `shouldFailWith` err 3
        (ueof <> elabel "pattern" <> elabel "rest of assignment"
        <> elabel "rest of identifier" <> elabel "rest of type declaration")
      (parse, "trait") `shouldFailWith` err 5 (ueof <> elabel "whitespace")
      (parse, "trait Eq a") `shouldFailWith` err 10
        (ueof <> etoks "where" <> elabel "rest of identifier" <> elabel "type variable")
      (parse, "trait Eq a => Ord a where\n x") `shouldFailWith` err 28
        (ueof <> elabel "rest of identifier" <> elabel "rest of type declaration")
      (parse, "trait Eq a => Ord a where\n x\n : Int")
        `shouldFailWith` errFancy 30 (badIndent 2 2)
      (parse, "trait Eq a where\n  x : Int\n y : String") `shouldFailWith` err 28
        (utok 'y' <> elabel "properly indented type declaration in trait")
      (parse, "trait Eq a where\n x : Int\n  y : String") `shouldFailWith` err 30
        (utok ':' <> elabel "properly indented type declaration in trait")

  describe "impl declarations" $ parallel $ do
    let impl p bindings = ImplDecl $ Impl [] p bindings
        pred clazz = IsIn (Id clazz)
        binding' x = Binding emptyAnn (Id x)

    it "can parse multiple impls in a row" $ do
      [text|
        impl Eq Int where
        impl Eq Int where
        |] ==> Module [ impl (pred "Eq" [con "Int"]) []
                      , impl (pred "Eq" [con "Int"]) []
                      ]
      [text|
        impl X Int where
          x = 42
        impl Y Int where
          x = 1234
          y = 100
        |] ==> Module [ impl (pred "X" [con "Int"]) [ binding' "x" $ num 42 ]
                      , impl (pred "Y" [con "Int"]) [ binding' "x" $ num 1234
                                                    , binding' "y" $ num 100]
                      ]

    it "can parse impl followed by other declaration" $ do
      [text|
        impl X Int where
          x = 42
        a = 3
        |] ==> Module [ impl (pred "X" [con "Int"]) [ binding' "x" $ num 42 ]
                      , binding "a" $ num 3
                      ]
      [text|
        impl X Int where
        a = 3
        |] ==> Module [ impl (pred "X" [con "Int"]) []
                      , binding "a" $ num 3
                      ]

    it "fails with readable error message" $ do
      (parse, "imp") `shouldFailWith` err 3
        (ueof <> elabel "pattern" <> elabel "rest of assignment"
        <> elabel "rest of type declaration" <> elabel "rest of identifier")
      (parse, "impl") `shouldFailWith` err 4 (ueof <> elabel "whitespace")
      (parse, "impl ") `shouldFailWith` err 5
        (ueof <> etok '(' <> elabel "trait identifier"
          <> elabel "typeclass identifier")
      (parse, "impl Eq a") `shouldFailWith` err 8 (utok 'a' <> elabel "type" )
      (parse, "impl Eq a =>") `shouldFailWith` err 12
        (ueof <> elabel "trait identifier")
      (parse, "impl Eq a => Ord a") `shouldFailWith` err 17
        (utok 'a' <> elabel "type")
      (parse, "impl Eq X") `shouldFailWith` err 9
        (ueof <> etoks "where" <> elabel "rest of identifier"
          <> elabel "type" <> elabel "type variable")
      (parse, "impl Eq X where\n x = 3\n  y = 4") `shouldFailWith` err 25
        (utok 'y' <> elabel "properly indented binding declaration in impl")

  describe "location information" $ parallel $ do
    let binding' :: Span -> Text -> Expr1 'Parsed -> Binding 'Parsed
        binding' sp name = Binding sp (Id name)
        num' ann = E1Lit ann . LNumber . SInt
        lam' ann vars = E1Lam ann (PVar . Id <$> vars)

    it "keeps track of location info for constant bindings" $ do
      "a = 3  " ~~> Module [BindingDecl $ binding' (Span 0 5) "a" (num' (Span 4 5) 3)]
      "abc = 123  " ~~> Module [BindingDecl $ binding' (Span 0 9) "abc" (num' (Span 6 9) 123)]

    it "keeps track of location info for named function bindings" $ do
      "a b c = 1 " ~~> Module [BindingDecl $ binding' (Span 0 9) "a"
                                          $ lam' (Span 0 9) ["b", "c"] $ num' (Span 8 9) 1]
      "abc def ghi = 1234 " ~~> Module [BindingDecl $ binding' (Span 0 18) "abc"
                                        $ lam' (Span 0 18) ["def", "ghi"] $ num' (Span 14 18) 1234]

    it "keeps track of location info for fixity declarations" $ do
      "infixl 6 +++ " ~~> Module [FixityDecl (Span 0 12) L 6 (Id "+++")]
      "infix 4 ** " ~~> Module [FixityDecl (Span 0 10) M 4 (Id "**")]
      "infixr 5 >> " ~~> Module [FixityDecl (Span 0 11) R 5 (Id ">>")]

