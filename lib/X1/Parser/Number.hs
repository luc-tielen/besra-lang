
module X1.Parser.Number ( parser ) where

import Protolude
import qualified Data.Text as T
import qualified Data.Text.Read as TR
import qualified Data.Vector.Unboxed as V
import X1.Parser.Types.Number
import X1.Parser.Helpers
import GHC.Unicode ( isDigit )

decimal :: Parser Number
decimal = SInt <$> do
  firstDigit <- satisfy (`V.elem` ['1'..'9']) <?> "non-zero digit"
  digits <- takeWhileP Nothing isDigit
  notFollowedBy letterChar
  case TR.decimal $ T.cons firstDigit digits of
    Right (result, _) -> pure result
    Left err -> panic . T.pack $ "Error occurred during parsing of decimal number: " <> err

-- NOTE: the parser below is bigger than you would expect for more detailed error reporting
parser :: Parser Number
parser = number <?> "number" where
  number = decimal <|> numberWithPrefixOrZero
  numberWithPrefixOrZero = do
    _ <- char '0'
    c <- optional (char 'x' <|> char 'b')
         <?> "constant number prefix: hex (0x) or binary (0b)"
    case c of
      Just 'x' -> SHex . ("0x" <>) <$> hexDigitChars
      Just 'b' -> SBin . ("0b" <>) <$> binDigitChars
      _        -> notFollowedBy digitChar $> SInt 0

