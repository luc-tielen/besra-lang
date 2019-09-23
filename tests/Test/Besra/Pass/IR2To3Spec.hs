
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
    passResult = case parseResult of
      Left err -> panic $ formatError err
      Right result -> do
        let (ir2, IR1To2.PassState adts traits impls) = IR1To2.pass result
            compState = CompilerState adts traits impls testKindEnv
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
        |] ==> Module ([expX, expY], [])

    it "groups implicit bindings topologically" $ do
      let imp = Implicit (Id "x") [([], eLet)]
          eLet = ELet (Span 38 145) ([dE], [impA, impBC, impD]) (num (Span 144 145) 1)
          dE = Explicit (Id "e") schE [([], num (Span 136 138) 42)]
          schE = ForAll (Span 122 125) [] ([] :=> c (Span 122 125) Star "Int")
          impA = [Implicit (Id "a") [([], eNothing)]]
          impBC = [ Implicit (Id "b") [([], eIf)]
                  , Implicit (Id "c") [([], EVar (Span 110 111) (Id "b"))]]
          impD = [Implicit (Id "d") [([], EVar (Span 46 47) (Id "a"))]]
          eNothing = ECon (Span 58 65) (Id "Nothing") schNothing
          schNothing = ForAll (Span 24 31) [] ([] :=> tMaybe)
          tMaybe = TApp (c (Span 5 10) (KArr Star Star) "Maybe")
                        (v (Span 11 12) Star "a")
          eIf = EIf (Span 76 99) (EApp (Span 79 85) (EApp (Span 79 85)
                                                      (EVar (Span 81 83) (Id "=="))
                                                      (num (Span 79 80) 1))
                                  (num (Span 84 85) 1))
                  (EVar (Span 91 92) (Id "a"))
                  (EVar (Span 98 99) (Id "c"))
      [text|
        data Maybe a = Just a | Nothing
        x =
          let d = a
              a = Nothing
              b = if 1 == 1 then a else c
              c = b
              e : Int
              e = 42
          in 1
        |] ==> Module ([], [[imp]])

  describe "expressions" $ parallel $ do
    it "attaches typescheme to constructor functions" $ do
      let imp = Implicit (Id "x") [([], eJust)]
          eJust = EApp (Span 36 48) (ECon (Span 36 40) (Id "Just") schJust) eNothing
          schJust = ForAll (Span 15 21) [] ([] :=> tJust)
          tJust = arrow (Span 15 21) (v (Span 20 21) Star "a") tMaybe
          eNothing = ECon (Span 41 48) (Id "Nothing") schNothing
          schNothing = ForAll (Span 24 31) [] ([] :=> tMaybe)
          tMaybe = TApp (c (Span 5 10) (KArr Star Star) "Maybe")
                        (v (Span 11 12) Star "a")
      [text|
        data Maybe a = Just a | Nothing
        x = Just Nothing
        |] ==> Module ([], [[imp]])

    it "converts nested expression in lambda" $ do
      let imp = Implicit (Id "x") [([PVar (Id "y")], eLam)]
          eLam = ELam (Span 38 51) ([PVar (Id "z")], body)
          body = EApp (Span 44 51) (ECon (Span 44 48) (Id "Just") schJust) eLit
          eLit = num (Span 49 51) 42
          schJust = ForAll (Span 15 21) [] ([] :=> tJust)
          tJust = arrow (Span 15 21) (v (Span 20 21) Star "a") tMaybe
          tMaybe = TApp (c (Span 5 10) (KArr Star Star) "Maybe")
                        (v (Span 11 12) Star "a")
      [text|
        data Maybe a = Just a | Nothing
        x y = \z -> Just 42
        |] ==> (Module ([], [[imp]]) :: Module KI)

    it "converts nested exprs in ifs" $ do
      let imp = Implicit (Id "x") [([], eIf)]
          eIf = EIf (Span 36 83) (EApp (Span 39 57) (EApp (Span 39 57)
                                                    (EVar (Span 47 49) (Id "=="))
                                                    eJust)
                                  (eNothing (Span 50 57)))
                    (eNothing (Span 63 70))
                    (eNothing (Span 76 83))
          eJust = EApp (Span 39 46) (ECon (Span 39 43) (Id "Just") schJust) e42
          e42 = num (Span 44 46) 42
          schJust = ForAll (Span 15 21) [] ([] :=> tJust)
          tJust = arrow (Span 15 21) (v (Span 20 21) Star "a") tMaybe
          eNothing sp = ECon sp (Id "Nothing") schNothing
          schNothing = ForAll (Span 24 31) [] ([] :=> tMaybe)
          tMaybe = TApp (c (Span 5 10) (KArr Star Star) "Maybe")
                        (v (Span 11 12) Star "a")
      [text|
        data Maybe a = Just a | Nothing
        x = if Just 42 == Nothing then Nothing else Nothing
        |] ==> Module ([], [[imp]])

    it "converts nested exprs in case" $ do
      let imp = Implicit (Id "x") [([], eCase)]
          eCase = ECase (Span 36 76) (ECon (Span 41 48) (Id "Nothing") schNothing)
                                     [(pNothing, eJust42)]
          pNothing = PCon (Id "Nothing") schNothing []
          eJust42 = EApp (Span 69 76) (ECon (Span 69 73) (Id "Just") schJust) e42
          e42 = num (Span 74 76) 42
          schJust = ForAll (Span 15 21) [] ([] :=> tJust)
          schNothing = ForAll (Span 24 31) [] ([] :=> tMaybe)
          tJust = arrow (Span 15 21) (v (Span 20 21) Star "a") tMaybe
          tMaybe = TApp (c (Span 5 10) (KArr Star Star) "Maybe")
                        (v (Span 11 12) Star "a")
      [text|
        data Maybe a = Just a | Nothing
        x = case Nothing of
              Nothing -> Just 42
        |] ==> Module ([], [[imp]])

    it "converts decls in let to binding group" $ do
      let imp = Implicit (Id "x") [([], eLet)]
          eLet = ELet (Span 38 145) ([dE], [impA, impBC, impD]) (num (Span 144 145) 1)
          dE = Explicit (Id "e") schE [([], num (Span 136 138) 42)]
          schE = ForAll (Span 122 125) [] ([] :=> c (Span 122 125) Star "Int")
          impA = [Implicit (Id "a") [([], eNothing)]]
          impBC = [ Implicit (Id "b") [([], eIf)]
                  , Implicit (Id "c") [([], EVar (Span 98 99) (Id "b"))]]
          impD = [Implicit (Id "d") [([], EVar (Span 110 111) (Id "a"))]]
          eNothing = ECon (Span 46 53) (Id "Nothing") schNothing
          schNothing = ForAll (Span 24 31) [] ([] :=> tMaybe)
          tMaybe = TApp (c (Span 5 10) (KArr Star Star) "Maybe")
                        (v (Span 11 12) Star "a")
          eIf = EIf (Span 64 87) (EApp (Span 67 73) (EApp (Span 67 73)
                                                      (EVar (Span 69 71) (Id "=="))
                                                      (num (Span 67 68) 1))
                                  (num (Span 72 73) 1))
                  (EVar (Span 79 80) (Id "a"))
                  (EVar (Span 86 87) (Id "c"))
      [text|
        data Maybe a = Just a | Nothing
        x =
          let a = Nothing
              b = if 1 == 1 then a else c
              c = b
              d = a
              e : Int
              e = 42
          in 1
        |] ==> Module ([], [[imp]])

    it "converts nested expr in let" $ do
      let imp = Implicit (Id "x") [([], eLet)]
          eLet = ELet (Span 38 60) ([], [[dY]]) eNothing
          dY = Implicit (Id "y") [([], num (Span 46 47) 1)]
          eNothing = ECon (Span 53 60) (Id "Nothing") schNothing
          schNothing = ForAll (Span 24 31) [] ([] :=> tMaybe)
          tMaybe = TApp (c (Span 5 10) (KArr Star Star) "Maybe")
                        (v (Span 11 12) Star "a")
      [text|
        data Maybe a = Just a | Nothing
        x =
          let y = 1
          in Nothing
        |] ==> Module ([], [[imp]])

  it "attaches typescheme to pattern constructors" $ do
    let imp = Implicit (Id "isJust") [aJust, aNothing]
        aJust = ([PCon (Id "Just")
                        (ForAll (Span 15 21) [] ([] :=> tJust))
                        [PWildcard]]
                 , num (Span 50 51) 0)
        aNothing = ([PCon (Id "Nothing")
                    (ForAll (Span 24 31) [] ([] :=> tMaybe)) []]
                    , num (Span 69 70) 1)
        tMaybe = TApp (c (Span 5 10) (KArr Star Star) "Maybe")
                      (v (Span 11 12) Star "a")
        tJust = arrow (Span 15 21) (v (Span 20 21) Star "a") tMaybe
    [text|
      data Maybe a = Just a | Nothing
      isJust (Just _) = 0
      isJust Nothing = 1
      |] ==> Module ([], [[imp]])

  it "adds typeschemes to nested pattern constructors" $ do
    let imp = Implicit (Id "isJust") [aJust, aNothing]
        aJust = ([PCon (Id "Just")
                        (ForAll (Span 15 21) [] ([] :=> tJust))
                        [aJust']]
                 , num (Span 57 58) 0)
        aJust' = PCon (Id "Just")
                      (ForAll (Span 15 21) [] ([] :=> tJust))
                      [PWildcard]
        aNothing = ([PCon (Id "Nothing")
                    (ForAll (Span 24 31) [] ([] :=> tMaybe)) []]
                    , num (Span 76 77) 1)
        tMaybe = TApp (c (Span 5 10) (KArr Star Star) "Maybe")
                      (v (Span 11 12) Star "a")
        tJust = arrow (Span 15 21) (v (Span 20 21) Star "a") tMaybe
    [text|
      data Maybe a = Just a | Nothing
      isJust (Just (Just _)) = 0
      isJust Nothing = 1
      |] ==> Module ([], [[imp]])

