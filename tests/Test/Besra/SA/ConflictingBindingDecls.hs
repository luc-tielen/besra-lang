
module Test.Besra.SA.ConflictingBindingDecls ( module Test.Besra.SA.ConflictingBindingDecls ) where

import Protolude hiding ( Type )
import Besra.SA.ConflictingBindingDecls
import Besra.SA.Helpers
import Besra.SA.Types
import Besra.Types.Id
import Besra.Types.Ann
import Besra.Types.Span
import Besra.Types.IR1.Module
import Besra.Types.IR1.Expr
import Besra.Types.IR1.String
import Besra.Types.IR1.Number
import Besra.Types.IR1.Lit
import Besra.Parser
import Test.Tasty.Hspec


type Module' = Module 'Parsed
type Expr' = Expr 'Parsed
type Ann' = Ann 'Parsed

file :: FilePath
file = "Test"

analyze' :: Validation [SAError] Module'
analyze' = analyze [validate file]

conflict :: [Span] -> Text -> [Expr'] -> SAError
conflict spans var exprs =
  let toBindingDecls = zipWith combine spans
      combine sp = BindingDecl . Binding sp (Id var)
      err = ConflictingBindingDeclErr . ConflictingBindingDecl file . toBindingDecls
   in err exprs

num :: Ann' -> Int -> Expr'
num ann = ELit ann . LNumber . SInt

str :: Ann' -> Text -> Expr'
str ann = ELit ann . LString . String


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
    "x = 5\nx = \"abc123\"" ==> Err [conflict [Span 0 5, Span 6 18] "x"
                                      [ num (Span 4 5) 5, str (Span 10 18) "abc123"]]
    "x = 5\nx = 3" ==> Err [conflict [Span 0 5, Span 6 11] "x"
                            [num (Span 4 5) 5, num (Span 10 11) 3]]

  it "reports an error when a duplicate is found" $ do
    "x = 5\nx = 5" ==> Err [conflict [Span 0 5, Span 6 11] "x"
                            [num (Span 4 5) 5, num (Span 10 11) 5]]
    "x = \"abc\"\nx = \"abc\"" ==> Err [conflict [Span 0 9, Span 10 19] "x"
                                        [str (Span 4 9) "abc", str (Span 14 19) "abc"]]

  it "reports multiple errors for each found conflict" $
    "x = 5\nx = \"abc\"\ny = \"123\"\ny = 123"
      ==> Err [ conflict [Span 0 5, Span 6 15] "x"
                [num (Span 4 5) 5, str (Span 10 15) "abc"]
              , conflict [Span 16 25, Span 26 33] "y"
                [str (Span 20 25) "123", num (Span 30 33) 123]]

  it "reports multiple errors for each conflict for a specific var" $ do
    let locations = uncurry num <$> [(Span 4 5, 1), (Span 10 11, 2), (Span 16 17, 3)]
    "x = 1\nx = 2\nx = 3" ==> Err [conflict [Span 0 5, Span 6 11, Span 12 17] "x" locations]

