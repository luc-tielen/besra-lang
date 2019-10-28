
module Besra.Types.Lit
  ( Lit(..)
  , String(..)
  , Number(..)
  ) where

import Protolude

-- NOTE: After desugaring literals, these stay in the same form
-- through rest of the compiler


data Lit = LChar Char
         | LString String
         | LNumber Number
         deriving (Eq, Show)

newtype String = String Text
  deriving (Eq, Show)

newtype Number = Number Int
  deriving (Eq, Show)

