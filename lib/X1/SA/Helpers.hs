
module X1.SA.Helpers ( analyze, Validation, ValidationResult(..) ) where

import Protolude
import Control.Parallel.Strategies

-- | A validation check can return either no errors,
--   or an error value.
data ValidationResult e = Ok | Err e
  deriving (Eq, Show, Functor)

-- | A validation is a function that takes a program as input,
--   and either returns ok or some kind of error.
type Validation err prog = prog -> ValidationResult err


instance Semigroup e => Semigroup (ValidationResult e) where
  Ok <> Ok = Ok
  (Err e1) <> (Err e2) = Err (e1 <> e2)
  Ok <> (Err e) = Err e
  (Err e) <> Ok = Err e

instance Semigroup e => Monoid (ValidationResult e) where
  mempty = Ok


-- | Function used for running semantic analysis.
--   It is setup in such a way that it can safely run validations in parallel.
--   After running all checks they are combined together into a final result.
analyze :: Semigroup e => [Validation e a] -> Validation e a
analyze validations prog =
  let parMap' = parMap rpar
      runValidation f = f prog
      results = parMap' runValidation validations
   in mconcat results

