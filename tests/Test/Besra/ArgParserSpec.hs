
module Test.Besra.ArgParserSpec ( module Test.Besra.ArgParserSpec ) where

import Protolude hiding ( WriteMode )
import Prelude ( String )
import System.IO.Silently
import Test.Hspec
import Besra.ArgParser
import qualified Data.Text as T


(==>) :: Text -> Args -> IO ()
a ==> b = do
  parsed <- parse $ toArgs a
  parsed `shouldBe` b

infixr 0 ==>

shouldFail :: Text -> IO ()
shouldFail str = hSilence [stderr] $ do
  let args = toArgs str
  catch (void $ parse args) handler
  where
    handler = \case
      ExitFailure 1 -> return ()
      e -> panic $ "Unknown error: " <> show e

toArgs :: Text -> [String]
toArgs = map T.unpack . T.split (== ' ')


spec :: Spec
spec = describe "argument parser" $ parallel $ do
  it "parses 'besra fmt FILE' correctly" $
    "fmt /path/to/file" ==> Fmt (FromFile "/path/to/file" (WriteMode Inplace))

  it "parses 'besra fmt FILE --check' correctly" $
    "fmt /path/to/file --check" ==> Fmt (FromFile "/path/to/file" CheckMode)

  it "parses 'besra fmt FILE --stdout' correctly" $
    "fmt /path/to/file --stdout" ==> Fmt (FromFile "/path/to/file" (WriteMode Stdout))

  it "parses 'besra fmt --stdin' correctly" $
    "fmt --stdin" ==> Fmt (FromStdIn NoCheck)

  it "parses 'besra fmt --stdin --check' correctly" $
    "fmt --stdin --check" ==> Fmt (FromStdIn DoCheck)

  it "fails to parse empty input" $
    shouldFail ""
