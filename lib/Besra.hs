
module Besra ( compile ) where

import Protolude
import Control.Monad.Except
import Data.Text.IO as TIO
import Besra.Parser ( ParseError, parseFile )
import Besra.Types.IR1.Module
import Besra.SA
import Besra.Types.Ann
import qualified Besra.Pass.BalanceOperators as BalanceOperators


type Module' = Module Parsed
type BalanceError' = BalanceOperators.BalanceError Parsed

data BesraError
  = ParseErr ParseError
  | BalanceErr BalanceError'
  | SemanticErr SemanticError
  deriving (Eq, Show)


class ToError e where
  toError :: e -> BesraError

instance ToError ParseError where
  toError = ParseErr

instance ToError BalanceError' where
  toError = BalanceErr

instance ToError SemanticError where
  toError = SemanticErr

instance ToError BesraError where
  toError = identity


-- | Operator for combining multiple steps in a pipeline together,
--   converting each step to have a common error type.
(>->) :: (Monad m, ToError e1, ToError e2)
      => (a -> ExceptT e1 m b)
      -> (b -> ExceptT e2 m c)
      -> (a -> ExceptT BesraError m c)
f >-> g = wrap f >=> wrap g where
  wrap h = withExceptT toError . h


parse :: FilePath -> ExceptT ParseError IO Module'
parse path = do
  content <- liftIO $ TIO.readFile path
  liftEither $ parseFile path content

semanticAnalysis :: FilePath -> Module' -> ExceptT SemanticError IO Module'
semanticAnalysis path decls =
  case runSA path decls of
    Ok -> pure decls
    Err e -> throwError e


compile :: FilePath -> IO ()
compile path = runExceptT pipeline $> () where
  pipeline = runPipeline path
  runPipeline =  parse
             >-> BalanceOperators.pass
             >-> semanticAnalysis path
  --         >-> ...

