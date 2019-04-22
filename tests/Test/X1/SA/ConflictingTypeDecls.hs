
module Test.X1.SA.ConflictingTypeDecls ( module Test.X1.SA.ConflictingTypeDecls ) where

import Protolude hiding ( Type )
import X1.SA.ConflictingTypeDecls
import X1.SA.Helpers
import X1.SA.Types
import X1.Parser.Types.Type
import X1.Parser.Types.Scheme
import X1.Types.Module
import X1.Types.Id
import X1.Parser
import Test.Tasty.Hspec


file :: FilePath
file = "Test.x1"

analyze' :: Validation [SAError] (Module Decl)
analyze' = analyze [validate file]

conflict :: Text -> [Type] -> SAError
conflict var types =
  let toTypeDecls = map (TypeDecl (Id var) . Scheme [])
      err = ConflictingTypeDeclErr . ConflictingTypeDecl file . toTypeDecls
   in err types

c :: Text -> Type
c = TCon . Tycon . Id

v :: Text -> Type
v = TVar . Tyvar . Id

app :: Type -> [Type] -> Type
app = TApp

(-->) :: Type -> Type -> Type
t1 --> t2 = app (c "->") [t1, t2]

(==>) :: Text -> ValidationResult [SAError] -> IO ()
txt ==> b =
  let result = parseFile file txt
   in case result of
     Left err -> panic $ "Error during parsing: " <> show err
     Right a -> analyze' a `shouldBe` b


spec_conflictingTypeDecls :: Spec
spec_conflictingTypeDecls = describe "SA: ConflictingTypeDecls" $ parallel $ do
  it "reports no errors for empty module" $
    "" ==> Ok

  it "reports no errors when no conflicts found" $ do
    "x : Int\ny : String" ==> Ok
    "x : Int -> Int\ny : Int -> String" ==> Ok
    "x : Int -> Int\nx1 : String" ==> Ok

  it "reports an error when a conflict is found" $ do
    "x : Int -> Int\nx : String" ==> Err [conflict "x" [c "Int" --> c "Int", c "String"]]
    "x : Int -> Int\nx : Int -> String" ==> Err [conflict "x" [ c "Int" --> c "Int"
                                                              , c "Int" --> c "String"]]

  it "reports an error when a duplicate is found" $ do
    "x : Int\nx : Int" ==> Err [conflict "x" [c "Int", c "Int"]]
    "x : Bool -> Float\nx: Bool -> Float" ==> Err [conflict "x" [ c "Bool" --> c "Float"
                                                                , c "Bool" --> c "Float"]]

  it "reports multiple errors for each found conflict" $
    "x : Int -> Int\nx : String\ny : Bool\ny : Int -> String"
      ==> Err [ conflict "x" [c "Int" --> c "Int", c "String"]
              , conflict "y" [c "Bool", c "Int" --> c "String"]]

  it "reports multiple errors for each conflict for a specific ID" $
    "x : Int -> Int\nx : String\nx : a -> String"
      ==> Err [conflict "x" [ c "Int" --> c "Int"
                            , c "String"
                            , v "a" --> c "String"]]

