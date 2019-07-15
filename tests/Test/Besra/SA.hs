module Test.Besra.SA ( module Test.Besra.SA ) where

import           Besra.SA
import           Protolude
import           Test.Tasty.Hspec


data Expr = Number Int
          | Str Text
          | Add Expr Expr
          deriving (Eq, Show)

data ValidationError = AddError Expr Expr
                     | NegativeValue Expr
                     -- Etc.
  deriving (Eq, Show)

type Validation' = Validation [ValidationError] Expr

-- real example would recursively check the tree also (not used here in tests).
addCheck :: Validation'
addCheck = \case
  Add e1@(Str _) e2@(Number _) -> Err [AddError e1 e2]
  Add e1@(Number _) e2@(Str _) -> Err [AddError e1 e2]
  Add e1@(Str _) e2@(Str _)    -> Err [AddError e1 e2]
  _                            -> Ok

noNegativesCheck :: Validation'
noNegativesCheck e1@(Number x)
  | x > 0     = Ok
  | otherwise = Err [NegativeValue e1]
noNegativesCheck (Add e1 e2) =
    noNegativesCheck e1 <> noNegativesCheck e2
noNegativesCheck _ = Ok


(==>) :: (Eq a, Show a) => a -> a -> IO ()
(==>) = shouldBe


spec_semanticAnalysis :: Spec
spec_semanticAnalysis = describe "Semantic analysis" $ parallel $ do
  it "does nothing when no checks provided" $ do
    analyze ([] :: [Validation']) (Number 1 `Add` Number 2) ==> Ok
    analyze ([] :: [Validation']) (Number 1 `Add` Str "abc") ==> Ok

  it "can do a single validation check" $ do
    analyze [addCheck] (Number 1 `Add` Number 2) ==> Ok
    analyze [addCheck] (Number 1 `Add` Str "abc") ==> Err [AddError (Number 1) (Str "abc")]
    analyze [addCheck] (Str "abc" `Add` Str "abc") ==> Err [AddError (Str "abc") (Str "abc")]
    analyze [addCheck] (Str "abc" `Add` Number 1) ==> Err [AddError (Str "abc") (Number 1)]

  it "can do multiple validation checks" $ do
    let checks = [addCheck, noNegativesCheck]
    analyze checks (Number 1 `Add` Number 2) ==> Ok
    analyze checks (Number 1 `Add` Str "abc") ==> Err [AddError (Number 1) (Str "abc")]
    analyze checks (Str "abc" `Add` Str "abc") ==> Err [AddError (Str "abc") (Str "abc")]
    analyze checks (Str "abc" `Add` Number 1) ==> Err [AddError (Str "abc") (Number 1)]
    analyze checks (Number (-1) `Add` Number 2) ==> Err [NegativeValue (Number (-1))]
    analyze checks (Number (-1) `Add` Str "abc")
      ==> Err [AddError (Number (-1)) (Str "abc"), NegativeValue (Number (-1))]

