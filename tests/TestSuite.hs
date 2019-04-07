module TestSuite ( module TestSuite ) where

import Test.Tasty.Hspec
import Prelude

spec_testSuite :: Spec
spec_testSuite = describe "test suite" $
  it "can run tests" $
    1 + 1 `shouldBe` 2
