
module Test.Besra.SA.MissingTopLevelDeclsSpec
  ( module Test.Besra.SA.MissingTopLevelDeclsSpec
  ) where

import Protolude hiding ( Type )
import Besra.SA.MissingTopLevelDecls
import Besra.SA.Helpers
import Besra.SA.Types
import Besra.Types.Id
import Besra.Types.Ann
import Besra.Types.Span
import Besra.Types.IR1 ( Module(..), Decl(..), Expr(..), Lit(..)
                       , Type(..), Tycon(..), String(..), Number(..)
                       , Scheme(..), TypeAnn(..), Binding(..) )
import Besra.Parser
import Test.Hspec


type Module' = Module Parsed
type Expr' = Expr Parsed
type Type' = Type Parsed
type Ann' = Ann Parsed

file :: FilePath
file = "Test"

analyze' :: Validation [SAError] Module'
analyze' = analyze [validate file]

missingType :: Span -> Text -> Expr' -> SAError
missingType sp var expr =
  let bindingDecl = BindingDecl $ Binding sp (Id var) expr
   in MissingTopLevelTypeAnnDeclErr $ MissingTopLevelTypeAnnDecl file bindingDecl

missingBinding :: Span -> Text -> Type' -> SAError
missingBinding sp var ty =
  let toTypeAnnDecl = TypeAnnDecl . TypeAnn sp (Id var) . scheme
      scheme t = Scheme (span t) [] t
      err = MissingTopLevelBindingDeclErr . MissingTopLevelBindingDecl file . toTypeAnnDecl
   in err ty

num :: Ann' -> Int -> Expr'
num ann = ELit ann . LNumber . SInt

str :: Ann' -> Text -> Expr'
str ann = ELit ann . LString . String

c :: Span -> Text -> Type'
c ann = TCon . Tycon ann . Id


(==>) :: Text -> ValidationResult [SAError] -> IO ()
txt ==> b =
  let result = parseFile file txt
   in case result of
     Left err -> panic $ "Error during parsing: " <> show err
     Right a -> analyze' a `shouldBe` b


spec :: Spec
spec = describe "SA: MissingTopLevelDecls" $ parallel $ do
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

