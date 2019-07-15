
module Test.X1.SA.MissingTopLevelDecls ( module Test.X1.SA.MissingTopLevelDecls ) where

import Protolude hiding ( Type )
import X1.SA.MissingTopLevelDecls
import X1.SA.Helpers
import X1.SA.Types
import X1.Types.Id
import X1.Types.Ann
import X1.Types.Span
import X1.Types.Expr1.Module
import X1.Types.Expr1.Expr
import X1.Types.Expr1.Lit
import X1.Types.Expr1.Type
import X1.Types.Expr1.String
import X1.Types.Expr1.Number
import X1.Types.Expr1.Scheme
import X1.Types.Expr1.TypeAnn
import X1.Parser
import Test.Tasty.Hspec


type Module' = Module 'Parsed
type Expr1' = Expr1 'Parsed
type Type' = Type 'Parsed
type Ann' = Ann 'Parsed

file :: FilePath
file = "Test.x1"

analyze' :: Validation [SAError] Module'
analyze' = analyze [validate file]

missingType :: Span -> Text -> Expr1' -> SAError
missingType sp var expr =
  let bindingDecl = BindingDecl $ Binding sp (Id var) expr
   in MissingTopLevelTypeAnnDeclErr $ MissingTopLevelTypeAnnDecl file bindingDecl

missingBinding :: Span -> Text -> Type' -> SAError
missingBinding sp var ty =
  let toTypeAnnDecl = TypeAnnDecl . TypeAnn sp (Id var) . scheme
      scheme t = Scheme (span t) [] t
      err = MissingTopLevelBindingDeclErr . MissingTopLevelBindingDecl file . toTypeAnnDecl
   in err ty

num :: Ann' -> Int -> Expr1'
num ann = E1Lit ann . LNumber . SInt

str :: Ann' -> Text -> Expr1'
str ann = E1Lit ann . LString . String

c :: Span -> Text -> Type'
c ann = TCon . Tycon ann . Id


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
    "x = 5" ==> Err [missingType (Span 0 5) "x" (num (Span 4 5) 5)]
    "x = 5\ny = \"abc\"" ==> Err [ missingType (Span 0 5) "x" (num (Span 4 5) 5)
                                 , missingType (Span 6 15) "y" (str (Span 10 15) "abc")]

  it "reports an error for each missing binding" $ do
    "x : Int" ==> Err [missingBinding (Span 0 7) "x" (c (Span 4 7) "Int")]
    "x : Int\ny : String" ==> Err [ missingBinding (Span 0 7) "x" (c (Span 4 7) "Int")
                                  , missingBinding (Span 8 18) "y" (c (Span 12 18) "String")]

