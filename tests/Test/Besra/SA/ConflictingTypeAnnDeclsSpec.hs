
module Test.Besra.SA.ConflictingTypeAnnDeclsSpec
  ( module Test.Besra.SA.ConflictingTypeAnnDeclsSpec
  ) where

import Protolude hiding ( Type )
import qualified Data.Text as T
import Besra.SA.ConflictingTypeAnnDecls
import Besra.SA.Helpers
import Besra.SA.Types
import Besra.Types.Id
import Besra.Types.Ann
import Besra.Types.Span
import Besra.Types.IR1.Module
import Besra.Types.IR1.Type
import Besra.Types.IR1.Scheme
import Besra.Types.IR1.TypeAnn
import Besra.Parser
import Test.Hspec


type Module' = Module 'Parsed
type Type' = Type 'Parsed

file :: FilePath
file = "Test"

analyze' :: Validation [SAError] Module'
analyze' = analyze [validate file]

conflict :: Text -> [Type'] -> SAError
conflict var types =
  let toTypeAnnDecls = map (TypeAnnDecl . typeAnn (Id var) . scheme)
      typeAnn name sch = TypeAnn (taSpan name $ span sch) name sch
      taSpan (Id name) sp = (beginPos sp - T.length name - 3) .> sp
      scheme t = Scheme (span t) [] t
      err = ConflictingTypeAnnDeclErr . ConflictingTypeAnnDecl file . toTypeAnnDecls
   in err types

c :: Span -> Text -> Type'
c sp = TCon . Tycon sp . Id

v :: Span -> Text -> Type'
v sp = TVar . Tyvar sp . Id

app :: Type' -> [Type'] -> Type'
app = TApp

arr :: Span -> Type' -> Type' -> Type'
arr sp t1 t2 = app (c sp "->") [t1, t2]

(==>) :: Text -> ValidationResult [SAError] -> IO ()
txt ==> b =
  let result = parseFile file txt
   in case result of
     Left err -> panic $ "Error during parsing: " <> show err
     Right a -> analyze' a `shouldBe` b


spec :: Spec
spec = describe "SA: ConflictingTypeAnnDecls" $ parallel $ do
  it "reports no errors for empty module" $
    "" ==> Ok

  it "reports no errors when no conflicts found" $ do
    "x : Int\ny : String" ==> Ok
    "x : Int -> Int\ny : Int -> String" ==> Ok
    "x : Int -> Int\nx1 : String" ==> Ok

  it "reports an error when a conflict is found" $ do
    "x : Int -> Int\nx : String"
      ==> Err [conflict "x"
            [ arr (Span 8 10) (c (Span 4 7) "Int") (c (Span 11 14) "Int")
            , c (Span 19 25) "String"]]
    "x : Int -> Int\nx : Int -> String"
      ==> Err [conflict "x"
            [ arr (Span 8 10) (c (Span 4 7) "Int") (c (Span 11 14) "Int")
            , arr (Span 23 25) (c (Span 19 22) "Int") (c (Span 26 32) "String")]]

  it "reports an error when a duplicate is found" $ do
    "x : Int\nx : Int"
      ==> Err [conflict "x" [c (Span 4 7) "Int", c (Span 12 15) "Int"]]
    "x : Bool -> Float\nx : Bool -> Float"
      ==> Err [conflict "x" [ arr (Span 9 11) (c (Span 4 8) "Bool") (c (Span 12 17) "Float")
                            , arr (Span 27 29) (c (Span 22 26) "Bool") (c (Span 30 35) "Float")]]

  it "reports multiple errors for each found conflict" $
    "x : Int -> Int\nx : String\ny : Bool\ny : Int -> String"
      ==> Err [ conflict "x" [ arr (Span 8 10) (c (Span 4 7) "Int") (c (Span 11 14) "Int")
                             , c (Span 19 25) "String"]
              , conflict "y" [ c (Span 30 34) "Bool"
                             , arr (Span 43 45) (c (Span 39 42) "Int") (c (Span 46 52) "String")]]

  it "reports multiple errors for each conflict for a specific var" $
    "x : Int -> Int\nx : String\nx : a -> String"
      ==> Err [conflict "x" [ arr (Span 8 10) (c (Span 4 7) "Int") (c (Span 11 14) "Int")
                            , c (Span 19 25) "String"
                            , arr (Span 32 34) (v (Span 30 31) "a") (c (Span 35 41) "String")]]

