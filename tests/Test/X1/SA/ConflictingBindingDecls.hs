
module Test.X1.SA.ConflictingBindingDecls ( module Test.X1.SA.ConflictingBindingDecls ) where

import Protolude hiding ( Type )
import X1.SA.ConflictingBindingDecls
import X1.SA.Helpers
import X1.SA.Types
import X1.Types.Id
import X1.Types.Ann
import X1.Types.Span
import X1.Types.Expr1.Module
import X1.Types.Expr1.Expr
import X1.Types.Expr1.String
import X1.Types.Expr1.Number
import X1.Types.Expr1.Lit
import X1.Parser
import Test.Tasty.Hspec


type Module' = Module 'Parsed
type Expr1' = Expr1 'Parsed
type Ann' = Ann 'Parsed

file :: FilePath
file = "Test.x1"

analyze' :: Validation [SAError] Module'
analyze' = analyze [validate file]

conflict :: Text -> [Expr1'] -> SAError
conflict var exprs =
  let toBindingDecls = map (BindingDecl . Binding (Id var))
      err = ConflictingBindingDeclErr . ConflictingBindingDecl file . toBindingDecls
   in err exprs

num :: Ann' -> Int -> Expr1'
num ann = E1Lit ann . LNumber . SInt

str :: Ann' -> Text -> Expr1'
str ann = E1Lit ann . LString . String


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
    "x = 5\nx = \"abc123\"" ==> Err [conflict "x" [ num (Span 4 5) 5
                                                  , str (Span 10 18) "abc123"]]
    "x = 5\nx = 3" ==> Err [conflict "x" [num (Span 4 5) 5, num (Span 10 11) 3]]

  it "reports an error when a duplicate is found" $ do
    "x = 5\nx = 5" ==> Err [conflict "x" [num (Span 4 5) 5, num (Span 10 11) 5]]
    "x = \"abc\"\nx = \"abc\"" ==> Err [conflict "x" [str (Span 4 9) "abc"
                                                     , str (Span 14 19) "abc"]]

  it "reports multiple errors for each found conflict" $
    "x = 5\nx = \"abc\"\ny = \"123\"\ny = 123"
      ==> Err [ conflict "x" [num (Span 4 5) 5, str (Span 10 15) "abc"]
              , conflict "y" [str (Span 20 25) "123", num (Span 30 33) 123]]

  it "reports multiple errors for each conflict for a specific var" $ do
    let locations = uncurry num <$> [(Span 4 5, 1), (Span 10 11, 2), (Span 16 17, 3)]
    "x = 1\nx = 2\nx = 3" ==> Err [conflict "x" locations]

