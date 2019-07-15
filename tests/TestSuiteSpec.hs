module TestSuiteSpec ( module TestSuiteSpec ) where

import Test.Hspec
import Prelude

spec :: Spec
spec = describe "test suite" $
  it "can run tests" $
    1 + 1 `shouldBe` 2
