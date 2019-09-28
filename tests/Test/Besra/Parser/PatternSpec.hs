
module Test.Besra.Parser.PatternSpec ( module Test.Besra.Parser.PatternSpec ) where

import Protolude hiding ( Type )
import Test.Hspec
import Test.Besra.Parser.Helpers
import Besra.Parser.Pattern ( parser )
import Besra.Types.Span
import Besra.Types.Ann
import Besra.Types.Id
import Besra.Types.IR1 ( Pattern(..), Lit(..), Number(..), String(..) )
import Test.Hspec.Megaparsec hiding (shouldFailWith, succeedsLeaving)


type Pattern' = Pattern Parsed

parse :: Text -> ParseResult Pattern'
parse = mkParser parser

num :: Int -> Lit
num = LNumber . SInt

str :: Text -> Lit
str = LString . String

var :: Text -> Id
var = Id

(==>) :: Text -> Pattern' -> IO ()
a ==> b = parse a `shouldParse` b


spec :: Spec
spec = describe "pattern parser" $ parallel $ do
  it "can parse wildcard patterns" $
    "_" ==> PWildcard (Span 0 1)

  it "can parse literal patterns" $ do
    "123" ==> PLit (Span 0 3) (num 123)
    "\"abc\"" ==> PLit (Span 0 5) (str "abc")

  it "can parse variables in pattern" $ do
    "abc" ==> PVar (Span 0 3) (var "abc")
    "abc123" ==> PVar (Span 0 6) (var "abc123")

  it "can parse constructors in pattern" $ do
    let pcon sp x = PCon sp (Id x)
        pvar sp = PVar sp . var
    "True" ==> pcon (Span 0 4) "True" []
    "(True)" ==> pcon (Span 1 5) "True" []
    "(A b)" ==> pcon (Span 1 4) "A" [pvar (Span 3 4) "b"]
    "(A b c)" ==> pcon (Span 1 6) "A" [pvar (Span 3 4) "b", pvar (Span 5 6) "c"]
    "(A b c D)" ==> pcon (Span 1 8) "A" [ pvar (Span 3 4) "b"
                                        , pvar (Span 5 6) "c"
                                        , pcon (Span 7 8) "D" [] ]

  it "can parse as-patterns" $ do
    "abc@def" ==> PAs (Span 0 7) (var "abc") (PVar (Span 4 7) (var "def"))
    "a@1" ==> PAs (Span 0 3) (var "a") (PLit (Span 2 3) $ num 1)
    "a@_" ==> PAs (Span 0 3) (var "a") (PWildcard (Span 2 3))

  it "can parse mix of everything" $ do
    let aPat = PAs (Span 0 17) (var "a") aInnerPat
        aInnerPat = PCon (Span 3 16) (Id "A") [bPat]
        bPat = PAs (Span 5 16) (var "b") bInnerPat
        bInnerPat = PCon (Span 8 15) (Id "B")
                      [ PWildcard (Span 10 11)
                      , PLit (Span 12 13) $ num 1
                      , PVar (Span 14 15) $ var "c"]
    "a@(A b@(B _ 1 c))" ==> aPat

  it "fails with readable error message" $ do
    (parse, "") `shouldFailWith` err 0 (ueof <> elabel "pattern")
    (parse, "_abc") `shouldFailWith` err 1 (utok 'a')
    (parse, "_123") `shouldFailWith` err 1 (utok '1')
    (parse, "()") `shouldFailWith` err 1 (utok ')' <> elabel "constructor")
    (parse, "(_a)") `shouldFailWith` err 1 (utok '_' <> elabel "constructor")
    (parse, "(_ a)") `shouldFailWith` err 1 (utok '_' <> elabel "constructor")
    (parse, "(a b)") `shouldFailWith` err 1 (utok 'a' <> elabel "constructor")
    (parse, "(A b") `shouldFailWith` err 4
      (ueof <> etok ')' <> elabel "pattern" <> elabel "rest of identifier")
    (parse, "a@(A b") `shouldFailWith` err 6
      (ueof <> etok ')' <> elabel "pattern" <> elabel "rest of identifier")

