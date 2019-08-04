
{-# LANGUAGE NoOverloadedLists, QuasiQuotes #-}

module Test.Besra.Pass.InferKindsSpec
  ( module Test.Besra.Pass.InferKindsSpec
  ) where

import Protolude hiding ( Type, pass )
import Test.Hspec
import qualified Data.Map as Map
import qualified Besra.Pass.IR1To2 as IR1To2
import qualified Besra.Pass.InferKinds as IK
import Besra.Parser
import Besra.TypeSystem.KindSolver
import Besra.Types.IR2
import Besra.Types.Kind
import Besra.Types.Ann
import Besra.Types.Span
import Besra.Types.Id
import NeatInterpolation


type Ann' = Ann 'KindInferred
type ADTHead' = ADTHead 'KindInferred
type ADTBody' = ADTBody 'KindInferred
type ADT' = ADT 'KindInferred
type Module' = Module 'KindInferred
type CompilerState' = IK.CompilerState 'KindInferred

runPass :: Text -> Either KindError (Module', CompilerState')
runPass input =
  let
    parseResult = parseFile "balance_operators.test" input
    passResult = case parseResult of
      Left err -> panic $ formatError err
      Right result ->
        let (ast, IR1To2.PassState{..}) = IR1To2.pass result
            kindEnv = Map.fromList [(Id "->", IKArr IStar (IKArr IStar IStar))]
            compState = IK.CompilerState adts traits impls kindEnv
         in runExcept $ IK.pass compState ast
  in
    passResult

class Testable a where
  (==>) :: Text -> a -> IO ()

infixr 0 ==>

instance Testable [(Id, Kind)] where
  input ==> adtHeads = do
    let result = runPass input
    case result of
      Left err -> panic $ show err
      Right (_, IK.CompilerState adts _ _ _) ->
        getAdtKind <$> adts `shouldBe` adtHeads
    where getAdtKind (ADT _ (ADTHead name ty) _) = (name, getKind ty)
          getKind = \case
            TCon (Tycon (_, k) _) -> k
            TVar (Tyvar (_, k) _) -> k
            TApp t1 _ -> getKind t1

instance Testable [ADT 'KindInferred] where
  input ==> adts = do
    let result = runPass input
    case result of
      Left err -> panic $ show err
      Right (_, IK.CompilerState adts' _ _ _) ->
        adts' `shouldBe` adts

instance Testable KindError where
  input ==> err = do
    let result = runPass input
    case result of
      Left err' -> err' `shouldBe` err
      Right res -> panic $ show res

adt :: Ann' -> ADTHead' -> ADTBody' -> ADT'
adt = ADT


spec :: Spec
spec = describe "kind inference algorithm" $ parallel $ do
  describe "ADT kind inference" $ parallel $ do
    it "can infer kinds for ADT with no type vars" $ do
      "data X = X" ==> [(Id "X", Star)]
      "data X = X Int" ==> [(Id "X", Star)]

    it "can infer kinds for ADT with type vars" $
      "data X a = X a" ==> [(Id "X", KArr Star Star)]

    it "can infer kinds for ADT with phantom type vars" $
      "data X a = X" ==> [(Id "X", KArr Star Star)]

    it "can infer kinds for ADTs with no body" $ do
      "data X" ==> [(Id "X", Star)]
      "data Y a" ==> [(Id "Y", KArr Star Star)]

    it "can infer kinds for ADT with multiple variants" $ do
      "data X = X | Y" ==> [(Id "X", Star)]
      "data X a = X a | Y a" ==> [(Id "X", KArr Star Star)]
      "data Maybe a = Just a | Nothing" ==> [(Id "Maybe", KArr Star Star)]
      "data Either a b = Left a | Right b"
        ==> [(Id "Either", KArr Star (KArr Star Star))]
      "data X f a = X (f a) | Y a"
        ==> [(Id "X", KArr (KArr Star Star) (KArr Star Star))]

    it "can infer kinds for 'Fix' data type" $
      "data Fix f = Fix (f (Fix f))"
        ==> [(Id "Fix", KArr (KArr Star Star) Star)]

    it "can infer kinds for ADT depending on another ADT" $ do
      [text|
        data X = X
        data Y = Y X
        |] ==> [(Id "X", Star), (Id "Y", Star)]
      [text|
        data X a = X
        data Y = Y (X Y)
        |] ==> [(Id "X", KArr Star Star), (Id "Y", Star)]

    it "can infer kinds for mutually referring ADTs" $ do
      [text|
        data A = Empty | A B
        data B = B A
        |] ==> [(Id "A", Star), (Id "B", Star)]
      [text|
        data A a = Empty | A B
        data B = B (A C)
        data C c = C
        |] ==> [ (Id "C", KArr Star Star)
               , (Id "A", KArr (KArr Star Star) Star)
               , (Id "B", Star) ]
      [text|
        data A a = Empty | A B
        data B = B (A C)
        data C a = C
        |] ==> [ (Id "C", KArr Star Star)
               , (Id "A", KArr (KArr Star Star) Star)
               , (Id "B", Star) ]

    it "enriches ADT with kind information" $ do
      let name = Id "Either"
          eitherCon = TCon $ Tycon (Span 5 11, KArr Star (KArr Star Star)) name
          aTy sp = TVar $ Tyvar (sp, Star) (Id "a")
          bTy sp = TVar $ Tyvar (sp, Star) (Id "b")
          eitherTy = TApp (TApp eitherCon (aTy (Span 12 13))) (bTy (Span 14 15))
          arrowTy sp a = TApp (TApp (TCon $ Tycon (sp,
                           KArr Star (KArr Star Star)) (Id "->")) a)
          leftTy = arrowTy (Span 18 24) (aTy (Span 23 24)) eitherTy
          rightTy = arrowTy (Span 27 34) (bTy (Span 33 34)) eitherTy
          conDecls = [ ConDecl (Span 18 24) (Id "Left") leftTy
                     , ConDecl (Span 27 34) (Id "Right") rightTy]
      "data Either a b = Left a | Right b"
        ==> [adt (Span 0 34) (ADTHead name eitherTy) conDecls]

    it "returns an error when kind unification failed" $ do
      "data X f a = X (f a) | Y f"
        ==> UnificationFail (IKArr (IKVar $ Id "k7") IStar) IStar
      [text|
        data X a = X
        data Y = Y (X X)
        |] ==> UnificationFail IStar (IKArr IStar IStar)

    it "returns an error for trying to create an infinite kind" $ do
      "data X a = X (X X)"
        ==> InfiniteKind (Id "k5")
                         (IKArr (IKVar $ Id "k5") IStar)
      "data X a = X (a X)"
        ==> InfiniteKind (Id "k9")
                         (IKArr (IKArr (IKVar $ Id "k9") IStar) IStar)

  describe "trait kind inference" $ parallel $ do

    it "TODO" pending

  describe "impl kind inference" $ parallel $ do
    it "TODO" pending

  describe "type signature kind inference" $ parallel $ do
    it "TODO" pending

