
module X1.Parser.Number ( parser ) where

import Protolude
import Prelude (read)
import qualified Data.Text as T
import X1.Parser.Types.Number
import X1.Parser.Helpers


decimal :: Parser Number
decimal = SInt <$> do
  firstDigit <- oneOf ['1'..'9'] <?> "non-zero digit"
  digits <- many digitChar
  pure . read $ [firstDigit] <> digits

-- NOTE: the parser below is bigger than you would expect for more detailed error reporting
parser :: Parser Number
parser = number <?> "number" where
  number = decimal <|> numberWithPrefixOrZero
  numberWithPrefixOrZero = do
    _ <- char '0'
    c <- optional (char 'x' <|> char 'b')
         <?> "constant number prefix: hex (0x) or binary (0b)"
    case c of
      Just 'x' -> SHex . T.pack . ("0x" <>) <$> some hexDigitChar
      Just 'b' -> SBin . T.pack . ("0b" <>) <$> some binDigitChar
      _        -> notFollowedBy digitChar *> pure (SInt 0)

