module TestSuite ( module TestSuite ) where

import Test.Tasty.Hspec
import Prelude

testSuite :: Spec
testSuite = describe "test suite" $
  it "can run tests" $
    1 `shouldBe` 1
