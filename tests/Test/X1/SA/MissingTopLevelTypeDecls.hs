
module Test.X1.SA.MissingTopLevelTypeDecls ( module Test.X1.SA.MissingTopLevelTypeDecls ) where

import Protolude hiding ( Type )
import X1.SA.MissingTopLevelTypeDecls
import X1.SA.Helpers
import X1.SA.Types
import X1.Parser.Types.String
import X1.Parser.Types.Number
import X1.Types.Module
import X1.Types.Expr1
import X1.Types.Lit
import X1.Types.Id
import X1.Parser
import Test.Tasty.Hspec


file :: FilePath
file = "Test.x1"

analyze' :: Validation [SAError] (Module Decl)
analyze' = analyze [validate file]

conflict :: Text -> Expr1 -> SAError
conflict var expr =
  let bindingDecl = BindingDecl (Id var) expr
   in MissingTopLevelTypeDeclErr $ MissingTopLevelTypeDecl file bindingDecl

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


spec_missingTopLevelDecls :: Spec
spec_missingTopLevelDecls = describe "SA: MissingTopLevelDecls" $ parallel $ do
  it "reports no errors for empty module" $
    "" ==> Ok

  it "reports no errors when all bindings have type signatures" $ do
    "x : Int\nx = 5" ==> Ok
    "x : Int\nx = 5\ny : String\ny = \"abc\"" ==> Ok

  it "reports an error for each missing type signature" $ do
    "x = 5" ==> Err [conflict "x" (num 5)]
    "x = 5\ny = \"abc\"" ==> Err [conflict "x" (num 5), conflict "y" (str "abc")]
