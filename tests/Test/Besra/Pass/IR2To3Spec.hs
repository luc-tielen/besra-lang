
{-# LANGUAGE BangPatterns, QuasiQuotes #-}

module Test.Besra.Pass.IR2To3Spec
  ( module Test.Besra.Pass.IR2To3Spec
  ) where

import Protolude hiding ( Type, pass )
import Test.Hspec
import qualified Data.Map as Map
import NeatInterpolation
import Besra.Types.IR3
import qualified Besra.Pass.IR1To2 as IR1To2
import qualified Besra.Pass.IR2To3 as IR2To3
import qualified Besra.Pass.InferKinds as InferKinds
import Besra.TypeSystem.KindSolver
import Besra.Types.CompilerState
import Besra.Types.Id
import Besra.Types.Ann
import Besra.Types.Span
import Besra.Types.Kind
import Besra.Parser

type KI = KindInferred

testKindEnv :: Env
testKindEnv =
  let sp = Span 0 0
      arrowK = IKArr sp (IStar sp) (IKArr sp (IStar sp) (IStar sp))
      kindEnv = Map.fromList [(Id "->", arrowK)]
   in Env kindEnv Map.empty

runPass :: Text -> Module KI
runPass input =
  let
    parseResult = parseFile "balance_operators.test" input
    (passResult, _) = case parseResult of
      Left err -> panic $ formatError err
      Right result -> do
        let (ir2, IR1To2.PassState adts traits impls) = IR1To2.pass result
            compState = CompilerState2 adts traits impls testKindEnv
        case InferKinds.pass compState ir2 of
          Left err -> panic $ show err
          Right result' -> uncurry (flip IR2To3.pass) result'
  in
    passResult

(==>) :: Text -> Module KI -> IO ()
input ==> ast = runPass input `shouldBe` ast

infixr 0 ==>

num :: Span -> Int -> Expr KI
num sp = ELit sp . LNumber . Number

v :: Span -> Kind -> Text -> Type KI
v sp k x = TVar (Tyvar (sp, k) (Id x))

c :: Span -> Kind -> Text -> Type KI
c sp k x = TCon (Tycon (sp, k) (Id x))

arrow :: Span -> Type KI -> Type KI -> Type KI
arrow sp t1 = TApp (TApp (c sp (KArr Star (KArr Star Star)) "->") t1)


spec :: Spec
spec = describe "IR2 -> IR3 pass" $ parallel $ do
  let schSomeType = ForAll (Span 36 44) [] ([] :=> c (Span 36 44) Star "SomeType")
  let schSomeType' = ForAll (Span 41 49) [] ([] :=> c (Span 41 49) Star "SomeType")

  describe "binding groups" $ parallel $ do
    it "puts bindings with type signatures in explicit part" $ do
      let expX = Explicit (Id "x") schX [([], eX)]
          expY = Explicit (Id "y") schY [([], eY)]
          schX = ForAll (Span 36 43) [] ([] :=> tX)
          schY = ForAll (Span 60 67) [] ([] :=> tY)
          tX = TApp (c (Span 36 41) (KArr Star Star) "Maybe")
                    (v (Span 42 43) Star "a")
          tY = TApp (c (Span 60 65) (KArr Star Star) "Maybe")
                    (v (Span 66 67) Star "a")
          eX = ECon (Span 48 55) (Id "Nothing") schNothing
          eY = EApp (Span 72 78) (ECon (Span 72 76) (Id "Just") schJust) eLit
          eLit = num (Span 77 78) 1
          schJust = ForAll (Span 15 21) [] ([] :=> tJust)
          schNothing = ForAll (Span 24 31) [] ([] :=> tMaybe)
          tJust = arrow (Span 15 21) (v (Span 20 21) Star "a") tMaybe
          tMaybe = TApp (c (Span 5 10) (KArr Star Star) "Maybe")
                        (v (Span 11 12) Star "a")
      [text|
        data Maybe a = Just a | Nothing
        x : Maybe a
        x = Nothing
        y : Maybe a
        y = Just 1
        |] ==> Module [expX, expY]

    it "groups implicit bindings topologically" $ do
      let expl = Explicit (Id "x") schSomeType [([], eLet)]
          eLet = ELet (Span 51 158) ([dE], [impA, impBC, impD]) (num (Span 157 158) 1)
          dE = Explicit (Id "e") schE [([], num (Span 149 151) 42)]
          schE = ForAll (Span 135 138) [] ([] :=> c (Span 135 138) Star "Int")
          impA = [Implicit (Id "a") [([], eNothing)]]
          impBC = [ Implicit (Id "b") [([], eIf)]
                  , Implicit (Id "c") [([], EVar (Span 123 124) (Id "b"))]]
          impD = [Implicit (Id "d") [([], EVar (Span 59 60) (Id "a"))]]
          eNothing = ECon (Span 71 78) (Id "Nothing") schNothing
          schNothing = ForAll (Span 24 31) [] ([] :=> tMaybe)
          tMaybe = TApp (c (Span 5 10) (KArr Star Star) "Maybe")
                        (v (Span 11 12) Star "a")
          eIf = EIf (Span 89 112) (EApp (Span 92 98) (EApp (Span 92 98)
                                                      (EVar (Span 94 96) (Id "=="))
                                                      (num (Span 92 93) 1))
                                  (num (Span 97 98) 1))
                  (EVar (Span 104 105) (Id "a"))
                  (EVar (Span 111 112) (Id "c"))
      [text|
        data Maybe a = Just a | Nothing
        x : SomeType
        x =
          let d = a
              a = Nothing
              b = if 1 == 1 then a else c
              c = b
              e : Int
              e = 42
          in 1
        |] ==> Module [expl]

  describe "expressions" $ parallel $ do
    it "attaches typescheme to constructor functions" $ do
      let expl = Explicit (Id "x") schSomeType [([], eJust)]
          eJust = EApp (Span 49 61) (ECon (Span 49 53) (Id "Just") schJust) eNothing
          schJust = ForAll (Span 15 21) [] ([] :=> tJust)
          tJust = arrow (Span 15 21) (v (Span 20 21) Star "a") tMaybe
          eNothing = ECon (Span 54 61) (Id "Nothing") schNothing
          schNothing = ForAll (Span 24 31) [] ([] :=> tMaybe)
          tMaybe = TApp (c (Span 5 10) (KArr Star Star) "Maybe")
                        (v (Span 11 12) Star "a")
      [text|
        data Maybe a = Just a | Nothing
        x : SomeType
        x = Just Nothing
        |] ==> Module [expl]

    it "converts nested expression in lambda" $ do
      let expl = Explicit (Id "x") schSomeType [([PVar (Span 47 48) (Id "y")], eLam)]
          eLam = ELam (Span 51 64) ([PVar (Span 52 53) (Id "z")], body)
          body = EApp (Span 57 64) (ECon (Span 57 61) (Id "Just") schJust) eLit
          eLit = num (Span 62 64) 42
          schJust = ForAll (Span 15 21) [] ([] :=> tJust)
          tJust = arrow (Span 15 21) (v (Span 20 21) Star "a") tMaybe
          tMaybe = TApp (c (Span 5 10) (KArr Star Star) "Maybe")
                        (v (Span 11 12) Star "a")
      [text|
        data Maybe a = Just a | Nothing
        x : SomeType
        x y = \z -> Just 42
        |] ==> Module [expl]

    it "converts nested exprs in ifs" $ do
      let expl = Explicit (Id "x") schSomeType [([], eIf)]
          eIf = EIf (Span 49 96) (EApp (Span 52 70) (EApp (Span 52 70)
                                                    (EVar (Span 60 62) (Id "=="))
                                                    eJust)
                                  (eNothing (Span 63 70)))
                    (eNothing (Span 76 83))
                    (eNothing (Span 89 96))
          eJust = EApp (Span 52 59) (ECon (Span 52 56) (Id "Just") schJust) e42
          e42 = num (Span 57 59) 42
          schJust = ForAll (Span 15 21) [] ([] :=> tJust)
          tJust = arrow (Span 15 21) (v (Span 20 21) Star "a") tMaybe
          eNothing sp = ECon sp (Id "Nothing") schNothing
          schNothing = ForAll (Span 24 31) [] ([] :=> tMaybe)
          tMaybe = TApp (c (Span 5 10) (KArr Star Star) "Maybe")
                        (v (Span 11 12) Star "a")
      [text|
        data Maybe a = Just a | Nothing
        x : SomeType
        x = if Just 42 == Nothing then Nothing else Nothing
        |] ==> Module [expl]

    it "converts nested exprs in case" $ do
      let expl = Explicit (Id "x") schSomeType [([], eCase)]
          eCase = ECase (Span 49 89) (ECon (Span 54 61) (Id "Nothing") schNothing)
                                     [(pNothing, eJust42)]
          pNothing = PCon (Span 71 79) (Id "Nothing") schNothing []
          eJust42 = EApp (Span 82 89) (ECon (Span 82 86) (Id "Just") schJust) e42
          e42 = num (Span 87 89) 42
          schJust = ForAll (Span 15 21) [] ([] :=> tJust)
          schNothing = ForAll (Span 24 31) [] ([] :=> tMaybe)
          tJust = arrow (Span 15 21) (v (Span 20 21) Star "a") tMaybe
          tMaybe = TApp (c (Span 5 10) (KArr Star Star) "Maybe")
                        (v (Span 11 12) Star "a")
      [text|
        data Maybe a = Just a | Nothing
        x : SomeType
        x = case Nothing of
              Nothing -> Just 42
        |] ==> Module [expl]

    it "converts decls in let to binding group" $ do
      let expl = Explicit (Id "x") schSomeType [([], eLet)]
          eLet = ELet (Span 51 158) ([dE], [impA, impBC, impD]) (num (Span 157 158) 1)
          dE = Explicit (Id "e") schE [([], num (Span 149 151) 42)]
          schE = ForAll (Span 135 138) [] ([] :=> c (Span 135 138) Star "Int")
          impA = [Implicit (Id "a") [([], eNothing)]]
          impBC = [ Implicit (Id "b") [([], eIf)]
                  , Implicit (Id "c") [([], EVar (Span 111 112) (Id "b"))]]
          impD = [Implicit (Id "d") [([], EVar (Span 123 124) (Id "a"))]]
          eNothing = ECon (Span 59 66) (Id "Nothing") schNothing
          schNothing = ForAll (Span 24 31) [] ([] :=> tMaybe)
          tMaybe = TApp (c (Span 5 10) (KArr Star Star) "Maybe")
                        (v (Span 11 12) Star "a")
          eIf = EIf (Span 77 100) (EApp (Span 80 86) (EApp (Span 80 86)
                                                      (EVar (Span 82 84) (Id "=="))
                                                      (num (Span 80 81) 1))
                                  (num (Span 85 86) 1))
                  (EVar (Span 92 93) (Id "a"))
                  (EVar (Span 99 100) (Id "c"))
      [text|
        data Maybe a = Just a | Nothing
        x : SomeType
        x =
          let a = Nothing
              b = if 1 == 1 then a else c
              c = b
              d = a
              e : Int
              e = 42
          in 1
        |] ==> Module [expl]

    it "converts nested expr in let" $ do
      let expl = Explicit (Id "x") schSomeType [([], eLet)]
          eLet = ELet (Span 51 73) ([], [[dY]]) eNothing
          dY = Implicit (Id "y") [([], num (Span 59 60) 1)]
          eNothing = ECon (Span 66 73) (Id "Nothing") schNothing
          schNothing = ForAll (Span 24 31) [] ([] :=> tMaybe)
          tMaybe = TApp (c (Span 5 10) (KArr Star Star) "Maybe")
                        (v (Span 11 12) Star "a")
      [text|
        data Maybe a = Just a | Nothing
        x : SomeType
        x =
          let y = 1
          in Nothing
        |] ==> Module [expl]

  it "attaches typescheme to pattern constructors" $ do
    let expl = Explicit (Id "isJust") schSomeType' [aJust, aNothing]
        aJust = ([PCon (Span 58 64) (Id "Just")
                        (ForAll (Span 15 21) [] ([] :=> tJust))
                        [PWildcard (Span 63 64)]]
                , num (Span 68 69) 0)
        aNothing = ([PCon (Span 77 85) (Id "Nothing")
                    (ForAll (Span 24 31) [] ([] :=> tMaybe)) []]
                   , num (Span 87 88) 1)
        tMaybe = TApp (c (Span 5 10) (KArr Star Star) "Maybe")
                      (v (Span 11 12) Star "a")
        tJust = arrow (Span 15 21) (v (Span 20 21) Star "a") tMaybe
    [text|
      data Maybe a = Just a | Nothing
      isJust : SomeType
      isJust (Just _) = 0
      isJust Nothing = 1
      |] ==> Module [expl]

  it "adds typeschemes to nested pattern constructors" $ do
    let expl = Explicit (Id "isJust") schSomeType' [aJust, aNothing]
        aJust = ([PCon (Span 58 70) (Id "Just")
                        (ForAll (Span 15 21) [] ([] :=> tJust))
                        [aJust']]
                 , num (Span 75 76) 0)
        aJust' = PCon (Span 64 70) (Id "Just")
                      (ForAll (Span 15 21) [] ([] :=> tJust))
                      [PWildcard (Span 69 70)]
        aNothing = ([PCon (Span 84 92) (Id "Nothing")
                    (ForAll (Span 24 31) [] ([] :=> tMaybe)) []]
                    , num (Span 94 95) 1)
        tMaybe = TApp (c (Span 5 10) (KArr Star Star) "Maybe")
                      (v (Span 11 12) Star "a")
        tJust = arrow (Span 15 21) (v (Span 20 21) Star "a") tMaybe
    [text|
      data Maybe a = Just a | Nothing
      isJust : SomeType
      isJust (Just (Just _)) = 0
      isJust Nothing = 1
      |] ==> Module [expl]

