
module Test.Besra.SA.ConflictingBindingDeclsSpec
  ( module Test.Besra.SA.ConflictingBindingDeclsSpec
  ) where

import Protolude hiding ( Type )
import Besra.SA.ConflictingBindingDecls
import Besra.SA.Helpers
import Besra.SA.Types
import Besra.Types.Id
import Besra.Types.Ann
import Besra.Types.Span
import Besra.Types.IR1 ( Module(..), Expr(..), Binding(..)
                       , String(..), Number(..), Lit(..), Pattern(..), Ann )
import Besra.Parser
import Test.Hspec


type Module' = Module Parsed
type Binding' = Binding Parsed
type Expr' = Expr Parsed
type Ann' = Ann Parsed

file :: FilePath
file = "Test"

analyze' :: Validation [SAError] Module'
analyze' = analyze [validate file]


conflictErr :: [Span] -> [Span] -> Text -> [Expr'] -> [Expr'] -> SAError
conflictErr spans1 spans2 var exprs1 exprs2 =
  let toBindings = zipWith combine
      combine sp = Binding sp (Id var)
      bs1 = toBindings spans1 exprs1
      bs2 = toBindings spans2 exprs2
   in ConflictingBindingDeclsErr $ ConflictingBindingDecls file bs1 bs2

constantErr :: [Span] -> Text -> [Expr'] -> SAError
constantErr = mkErr $ \path bindings ->
  ConflictingConstantDeclErr $ ConflictingConstantDecl path bindings

argCountErr :: [Span] -> Text -> [Expr'] -> SAError
argCountErr = mkErr $ \path bindings ->
  ConflictingArgCountsErr $ ConflictingArgCounts path bindings

mkErr :: (FilePath -> [Binding'] -> SAError)
      -> [Span] -> Text -> [Expr'] -> SAError
mkErr wrap spans var exprs =
  let toBindings = zipWith combine spans
      combine sp = Binding sp (Id var)
      err = wrap file . toBindings
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


spec :: Spec
spec = describe "SA: bindings" $ parallel $ do
  it "reports no errors for empty module" $
    "" ==> Ok

  it "reports no errors when no conflicting bindings found" $ do
    "x = 5\ny = 3" ==> Ok
    "x = 5\nx1 = 3" ==> Ok

  describe "conflicting bindings" $ parallel $ do
    it "reports no errors for multiple grouped function decls" $ do
      "f False = 0\nf True = 1" ==> Ok
      "f False = 0\n\nf True = 1" ==> Ok
      "f _ = 0\n\nf _ = 1\ng _ = 3\ng _ = 4\nh _ = 5" ==> Ok

    it "reports error if bindings are not grouped together" $ do
      "f _ = 0\nf _ = 1\ng = 2\nf _ = 3"
        ==> Err [conflictErr [Span 0 7, Span 8 15] [Span 22 29] "f"
                  [ ELam (Span 0 7) [PWildcard (Span 2 3)] $ num (Span 6 7) 0
                  , ELam (Span 8 15) [PWildcard (Span 10 11)] $ num (Span 14 15) 1]
                  [ ELam (Span 22 29) [PWildcard (Span 24 25)] $ num (Span 28 29) 3]
                ]
      "f _ = 0\nf _ = 1\ng 0 = 1\ng _ = 2\nf _ = 3"
        ==> Err [conflictErr [Span 0 7, Span 8 15] [Span 32 39] "f"
                  [ ELam (Span 0 7) [PWildcard (Span 2 3)] $ num (Span 6 7) 0
                  , ELam (Span 8 15) [PWildcard (Span 10 11)] $ num (Span 14 15) 1]
                  [ ELam (Span 32 39) [PWildcard (Span 34 35)] $ num (Span 38 39) 3]
                ]
      "f _ = 0\nf _ = 1\ng = 2\nf _ = 3\nh = 4\nf _ = 5"
        ==> Err [conflictErr [Span 0 7, Span 8 15] [Span 22 29] "f"
                  [ ELam (Span 0 7) [PWildcard (Span 2 3)] $ num (Span 6 7) 0
                  , ELam (Span 8 15) [PWildcard (Span 10 11)] $ num (Span 14 15) 1]
                  [ ELam (Span 22 29) [PWildcard (Span 24 25)] $ num (Span 28 29) 3],
                conflictErr [Span 0 7, Span 8 15] [Span 36 43] "f"
                  [ ELam (Span 0 7) [PWildcard (Span 2 3)] $ num (Span 6 7) 0
                  , ELam (Span 8 15) [PWildcard (Span 10 11)] $ num (Span 14 15) 1]
                  [ ELam (Span 36 43) [PWildcard (Span 38 39)] $ num (Span 42 43) 5]
                ]

    it "reports error if multiple grouped decls separated by other decl" $ do
      "f _ = 0\nf : Bool -> Int\nf _ = 1"
        ==> Err [conflictErr [Span 0 7] [Span 24 31] "f"
                  [ ELam (Span 0 7) [PWildcard (Span 2 3)] $ num (Span 6 7) 0 ]
                  [ ELam (Span 24 31) [PWildcard (Span 26 27)] $ num (Span 30 31) 1 ]
                ]
      "f _ = 0\ndata Bool = True | False\nf _ = 1"
        ==> Err [conflictErr [Span 0 7] [Span 33 40] "f"
                  [ ELam (Span 0 7) [PWildcard (Span 2 3)] $ num (Span 6 7) 0 ]
                  [ ELam (Span 33 40) [PWildcard (Span 35 36)] $ num (Span 39 40) 1 ]
                ]

  describe "conflicting arg counts" $ parallel $ do
    it "reports no errors if all bindings have same arg count" $ do
      "x a = 0" ==> Ok
      "x 0 = 0\nx _ = 1" ==> Ok
      "x = \\a -> 0" ==> Ok
      "x = \\0 -> 0\nx = \\_ -> 1" ==> Ok
      "x 0 0 = 0\nx 1 1 = 2" ==> Ok
      "x 0 0 = 0\nx = \\1 1 -> 2" ==> Ok

    it "reports errors if bindings have different arg count" $ do
      "x _ = 1\nx _ _ = 2"
        ==> Err [argCountErr [Span 0 7, Span 8 17] "x"
                  [ ELam (Span 0 7) [PWildcard (Span 2 3)] $ num (Span 6 7) 1
                  , ELam (Span 8 17) [PWildcard (Span 10 11), PWildcard (Span 12 13)] $ num (Span 16 17) 2]]
      "x = \\_ -> 1\nx = \\_ _ -> 2"
        ==> Err [argCountErr [Span 0 11, Span 12 25] "x"
                  [ ELam (Span 4 11) [PWildcard (Span 5 6)] $ num (Span 10 11) 1
                  , ELam (Span 16 25) [PWildcard (Span 17 18), PWildcard (Span 19 20)] $ num (Span 24 25) 2]]
      "x _ = 1\nx = \\_ _ -> 2"
        ==> Err [argCountErr [Span 0 7, Span 8 21] "x"
                  [ ELam (Span 0 7) [PWildcard (Span 2 3)] $ num (Span 6 7) 1
                  , ELam (Span 12 21) [PWildcard (Span 13 14), PWildcard (Span 15 16)] $ num (Span 20 21) 2]]

  describe "constant binding decls" $ parallel $ do
    it "reports an error when 2 assignments to same constant are found" $ do
      "x = 5\nx = \"abc123\"" ==> Err [constantErr [Span 0 5, Span 6 18] "x"
                                        [ num (Span 4 5) 5, str (Span 10 18) "abc123"]]
      "x = 5\nx = 3" ==> Err [constantErr [Span 0 5, Span 6 11] "x"
                              [num (Span 4 5) 5, num (Span 10 11) 3]]

    it "reports an error when a duplicate is found" $ do
      "x = 5\nx = 5" ==> Err [constantErr [Span 0 5, Span 6 11] "x"
                              [num (Span 4 5) 5, num (Span 10 11) 5]]
      "x = \"abc\"\nx = \"abc\"" ==> Err [constantErr [Span 0 9, Span 10 19] "x"
                                          [str (Span 4 9) "abc", str (Span 14 19) "abc"]]

    it "reports multiple errors for each error found" $
      "x = 5\nx = \"abc\"\ny = \"123\"\ny = 123"
        ==> Err [ constantErr [Span 0 5, Span 6 15] "x"
                  [num (Span 4 5) 5, str (Span 10 15) "abc"]
                , constantErr [Span 16 25, Span 26 33] "y"
                  [str (Span 20 25) "123", num (Span 30 33) 123]]

    it "reports multiple errors for each error for a specific var" $ do
      let locations = uncurry num <$> [(Span 4 5, 1), (Span 10 11, 2), (Span 16 17, 3)]
      "x = 1\nx = 2\nx = 3" ==> Err [constantErr [Span 0 5, Span 6 11, Span 12 17] "x" locations]

