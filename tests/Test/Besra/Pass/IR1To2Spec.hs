
{-# LANGUAGE QuasiQuotes #-}

module Test.Besra.Pass.IR1To2Spec
  ( module Test.Besra.Pass.IR1To2Spec
  ) where

import Protolude hiding ( Type, pass )
import Test.Hspec
import NeatInterpolation
import Besra.Parser
import Besra.Pass.IR1To2
import Besra.Types.IR2
import Besra.Types.Ann
import Besra.Types.Span
import Besra.Types.Id


type Module' = Module 'Parsed
type Binding' = Binding 'Parsed
type Expr' = Expr 'Parsed
type TypeAnn' = TypeAnn 'Parsed
type Scheme' = Scheme 'Parsed
type Type' = Type 'Parsed
type Tycon' = Tycon 'Parsed
type Tyvar' = Tyvar 'Parsed
type ADT' = ADT 'Parsed
type ADTHead' = ADTHead 'Parsed
type ADTBody' = ADTBody 'Parsed

runPass :: Text -> (Module', PassState)
runPass input =
  let
    parseResult = parseFile "balance_operators.test" input
    passResult = case parseResult of
      Left err -> panic $ formatError err
      Right result -> pass result
  in
    passResult

class Testable a where
  (==>) :: Text -> a -> IO ()

infixr 0 ==>

instance Testable Module' where
  input ==> m = do
    let result = runPass input
    result `shouldBe` (m, PassState [] [] [])

instance Testable Binding' where
  input ==> b = input ==> Module [BindingDecl b]

instance Testable TypeAnn' where
  input ==> ta = input ==> Module [TypeAnnDecl ta]

instance Testable PassState where
  input ==> passState = do
    let result = runPass input
    result `shouldBe` (Module [], passState)

binding :: Span -> Text -> Expr' -> Binding'
binding sp x = Binding sp (Id x)

typeAnn :: Span -> Text -> Scheme' -> TypeAnn'
typeAnn sp x = TypeAnn sp (Id x)

lam :: Span -> [Pattern] -> Expr' -> Expr'
lam = ELam

evar :: Span -> Text -> Expr'
evar sp = EVar sp . Id

con :: Span -> Text -> Tycon'
con sp x = Tycon sp (Id x)

var :: Span -> Text -> Tyvar'
var sp x = Tyvar sp (Id x)

v :: Span -> Text -> Type'
v sp x = TVar (Tyvar sp (Id x))

c :: Span -> Text -> Type'
c sp x = TCon (Tycon sp (Id x))

tArrow :: Span -> Type' -> Type' -> Type'
tArrow sp a = TApp (TApp (c sp "->") a)

adtHead :: Tycon' -> [Tyvar'] -> ADTHead'
adtHead constr@(Tycon _ x) vars =
  let ty = foldl' TApp (TCon constr) (TVar <$> vars)
   in ADTHead x ty

adt :: Span -> ADTHead' -> ADTBody' -> ADT'
adt = ADT

spec :: Spec
spec = describe "IR1 -> IR2 pass" $ parallel $ do
  it "converts binary numbers to decimal numbers" $ do
    let num sp = ELit sp . LNumber . Number
    "x = 0b0" ==> binding (Span 0 7) "x" (num (Span 4 7) 0)
    "x = 0b1101" ==> binding (Span 0 10) "x" (num (Span 4 10) 13)
    "x = 0b101010" ==> binding (Span 0 12) "x" (num (Span 4 12) 42)

  it "converts hex numbers to decimal numbers" $ do
    let num sp = ELit sp . LNumber . Number
    "x = 0x0" ==> binding (Span 0 7) "x" (num (Span 4 7) 0)
    "x = 0x1101" ==> binding (Span 0 10) "x" (num (Span 4 10) 4353)
    "x = 0xDEADBEEF" ==> binding (Span 0 14) "x" (num (Span 4 14) 3735928559)
    "x = 0xC0FF33" ==> binding (Span 0 12) "x" (num (Span 4 12) 12648243)

  it "converts numbers in binding patterns" $ do
    let num sp = ELit sp . LNumber . Number
        pnum = PLit . LNumber . Number
    "x 0b0 = 0b0" ==> binding (Span 0 11) "x" (lam (Span 0 11) [pnum 0]
                                                (num (Span 8 11) 0))
    "x 0x101010 = 0b1101"
      ==> binding (Span 0 19) "x" (lam (Span 0 19) [pnum 1052688]
                                    (num (Span 13 19) 13))

  it "converts infix operators to normal function application" $ do
    let op sp x a = EApp sp (EApp sp x a)
        mulAB = op (Span 4 9)
                  (evar (Span 6 7) "*")
                  (evar (Span 4 5) "a")
                  (evar (Span 8 9) "b")
    "x = a * b"
      ==> binding (Span 0 9) "x" mulAB
    "x = a * b + c"
      ==> binding (Span 0 13) "x" (op (Span 4 13)
                                    (evar (Span 10 11) "+")
                                    mulAB
                                    (evar (Span 12 13) "c"))

  it "converts prefix negation to normal function application" $
    "x = -b"
      ==> binding (Span 0 6) "x" (EApp (Span 4 6)
                                    (evar (Span 4 6) "negate")
                                    (evar (Span 5 6) "b"))

  it "makes function application curried" $ do
    let mkfa sp = EApp sp (evar (Span 4 5) "f") (evar (Span 6 7) "a")
        mkfab sp = EApp sp (mkfa sp) (evar (Span 8 9) "b")
        fa = mkfa (Span 4 7)
        fab = mkfab (Span 4 9)
        fabc = EApp (Span 4 11) (mkfab (Span 4 11)) (evar (Span 10 11) "c")
    "x = f a" ==> binding (Span 0 7) "x" fa
    "x = f a b" ==> binding (Span 0 9) "x" fab
    "x = f a b c" ==> binding (Span 0 11) "x" fabc

  it "makes type application curried" $ do
    let tMaybe = TApp (c (Span 4 9) "Maybe") (v (Span 10 11) "a")
        tEither = TApp (TApp (c (Span 4 10) "Either")
                          (c (Span 11 14) "Int"))
                        (c (Span 15 21) "String")
        tFab = tArrow (Span 21 23) (v (Span 19 20) "a") (v (Span 24 25) "b")
        tFbc = tArrow (Span 11 13) (v (Span 9 10) "b") (v (Span 14 15) "c")
        tFabc = tArrow (Span 6 8) (v (Span 4 5) "a") tFbc
    "x : Maybe a" ==> typeAnn (Span 0 11) "x" (Scheme (Span 4 11) [] tMaybe)
    "x : Either Int String" ==> typeAnn (Span 0 21) "x" (Scheme (Span 4 21) [] tEither)
    "x : a -> b -> c" ==> typeAnn (Span 0 15) "x" (Scheme (Span 4 15) [] tFabc)
    "x : Convert a b => a -> b"
      ==> typeAnn (Span 0 25) "x"
            (Scheme (Span 4 25)
              [IsIn (Span 4 15) (Id "Convert") [ v (Span 12 13) "a"
                                               , v (Span 14 15) "b"]]
              tFab)

  it "returns all ADTs found in file" $ do
    let conDecl sp x = ConDecl sp (Id x)
        adtX = adt (Span 0 10)
         (adtHead (con (Span 5 6) "X") [])
         [conDecl (Span 9 10) "X" (c (Span 5 6) "X")]
        adtY = adt (Span 11 40)
         (adtHead (con (Span 16 17) "Y") [var (Span 18 19) "a", var (Span 20 21) "b"])
         [ conDecl (Span 24 36) "Y" tyConDeclY
         , conDecl (Span 39 40) "Z" tyY]
        tyConDeclY =
          tArrow (Span 24 36) (c (Span 26 27) "X") $
            tArrow (Span 24 36)
              (tArrow (Span 31 33) (v (Span 29 30) "a") (v (Span 34 35) "b"))
              tyY
        tyY = TApp (TApp (c (Span 16 17) "Y") (v (Span 18 19) "a"))
                    (v (Span 20 21) "b")
    [text|
      data X = X
      data Y a b = Y X (a -> b) | Z
      |] ==> PassState [adtY, adtX] [] []

  it "returns all traits found in file" $ do
    let traitEq = Trait (Span 0 40) [] eqA [typeAnn (Span 19 40) "==" eqTy]
        traitOrd = Trait (Span 41 97) [eqAInOrdA] ordA
          [typeAnn (Span 69 97) "compare" ordTy]
        eqA = IsIn (Span 6 10) (Id "Eq") [v (Span 9 10) "a"]
        eqAInOrdA = IsIn (Span 47 51) (Id "Eq") [v (Span 50 51) "a"]
        ordA = IsIn (Span 55 60) (Id "Ord") [v (Span 59 60) "a"]
        eqTy = Scheme (Span 26 40) [] $
          tArrow (Span 28 30) (v (Span 26 27) "a")
            (tArrow (Span 33 35) (v (Span 31 32) "a") (c (Span 36 40) "Bool"))
        ordTy = Scheme (Span 79 97) [] $
          tArrow (Span 81 83) (v (Span 79 80) "a")
            (tArrow (Span 86 88) (v (Span 84 85) "a") (c (Span 89 97) "Ordering"))
    [text|
      trait Eq a where
        (==) : a -> a -> Bool
      trait Eq a => Ord a where
        compare : a -> a -> Ordering
      |] ==> PassState [] [traitOrd, traitEq] []

  it "returns all impls found in file" $ do
    let impl = Impl (Span 0 40) [] p
          [binding (Span 28 40) "byteSize" (num (Span 39 40) 8)]
        p = IsIn (Span 5 19) (Id "ByteSize") [c (Span 14 19) "Int64"]
        num sp = ELit sp . LNumber . Number
    [text|
      impl ByteSize Int64 where
        byteSize = 8
      |] ==> PassState [] [] [impl]

