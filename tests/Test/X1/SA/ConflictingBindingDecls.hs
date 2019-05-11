
module Test.X1.SA.ConflictingBindingDecls ( module Test.X1.SA.ConflictingBindingDecls ) where

import Protolude hiding ( Type )
import X1.SA.ConflictingBindingDecls
import X1.SA.Helpers
import X1.SA.Types
import X1.Types.Id
import X1.Types.Module
import X1.Types.Expr1
import X1.Types.Expr1.String
import X1.Types.Expr1.Number
import X1.Types.Expr1.Lit
import X1.Parser
import Test.Tasty.Hspec


file :: FilePath
file = "Test.x1"

analyze' :: Validation [SAError] (Module Decl)
analyze' = analyze [validate file]

conflict :: Text -> [Expr1] -> SAError
conflict var exprs =
  let toBindingDecls = map (BindingDecl (Id var))
      err = ConflictingBindingDeclErr . ConflictingBindingDecl file . toBindingDecls
   in err exprs

num :: Int -> Expr1
num = E1Lit . LNumber . SInt

str :: Text -> Expr1
str = E1Lit . LString . String

(==>) :: Text -> ValidationResult [SAError] -> IO ()
txt ==> b =
  let result = parseFile file txt
   in case result of
     Left err -> panic $ "Error during parsing: " <> show err
     Right a -> analyze' a `shouldBe` b


spec_conflictingBindingDecls :: Spec
spec_conflictingBindingDecls = describe "SA: ConflictingBindingDecls" $ parallel $ do
  it "reports no errors for empty module" $
    "" ==> Ok

  it "reports no errors when no conflicts found" $ do
    "x = 5\ny = 3" ==> Ok
    "x = 5\nx1 = 3" ==> Ok

  it "reports an error when a conflict is found" $ do
    "x = 5\nx = \"abc123\"" ==> Err [conflict "x" [num 5, str "abc123"]]
    "x = 5\nx = 3" ==> Err [conflict "x" [num 5, num 3]]

  it "reports an error when a duplicate is found" $ do
    "x = 5\nx = 5" ==> Err [conflict "x" [num 5, num 5]]
    "x = \"abc\"\nx = \"abc\"" ==> Err [conflict "x" [str "abc", str "abc"]]

  it "reports multiple errors for each found conflict" $
    "x = 5\nx = \"abc\"\ny = \"123\"\ny = 123"
      ==> Err [conflict "x" [num 5, str "abc"], conflict "y" [str "123", num 123]]

  it "reports multiple errors for each conflict for a specific var" $
    "x = 1\nx = 2\nx = 3" ==> Err [conflict "x" (num <$> [1, 2, 3])]

