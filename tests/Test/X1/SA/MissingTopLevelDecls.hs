
module Test.X1.SA.MissingTopLevelDecls ( module Test.X1.SA.MissingTopLevelDecls ) where

import Protolude hiding ( Type )
import X1.SA.MissingTopLevelDecls
import X1.SA.Helpers
import X1.SA.Types
import X1.Types.Id
import X1.Types.Module
import X1.Types.Expr1
import X1.Types.Expr1.Lit
import X1.Types.Expr1.Type
import X1.Types.Expr1.String
import X1.Types.Expr1.Number
import X1.Types.Expr1.Scheme
import X1.Parser
import Test.Tasty.Hspec


file :: FilePath
file = "Test.x1"

analyze' :: Validation [SAError] (Module Decl)
analyze' = analyze [validate file]

missingType :: Text -> Expr1 -> SAError
missingType var expr =
  let bindingDecl = BindingDecl (Id var) expr
   in MissingTopLevelTypeDeclErr $ MissingTopLevelTypeDecl file bindingDecl

missingBinding :: Text -> Type -> SAError
missingBinding var ty =
  let toTypeDecl = TypeDecl (Id var) . Scheme []
      err = MissingTopLevelBindingDeclErr . MissingTopLevelBindingDecl file . toTypeDecl
   in err ty

num :: Int -> Expr1
num = E1Lit . LNumber . SInt

str :: Text -> Expr1
str = E1Lit . LString . String

c :: Text -> Type
c = TCon . Tycon . Id

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

  it "reports no errors when all bindings have type signatures and vice versa" $ do
    "x : Int\nx = 5" ==> Ok
    "x : Int\nx = 5\ny : String\ny = \"abc\"" ==> Ok

  it "reports an error for each missing type signature" $ do
    "x = 5" ==> Err [missingType "x" (num 5)]
    "x = 5\ny = \"abc\"" ==> Err [missingType "x" (num 5), missingType "y" (str "abc")]

  it "reports an error for each missing binding" $ do
    "x : Int" ==> Err [missingBinding "x" (c "Int")]
    "x : Int\ny : String" ==> Err [ missingBinding "x" (c "Int")
                                  , missingBinding "y" (c "String")]

