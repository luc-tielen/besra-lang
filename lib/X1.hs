module X1 ( compile ) where

import Protolude
import Control.Monad.Except
import Data.Text.IO as TIO
import X1.Parser ( ParseError, parseFile )
import X1.Types.Module
import X1.SA


data X1Error = ParseErr ParseError
             | SemanticErr SemanticError
             deriving (Eq, Show)

class ToError e where
  toError :: e -> X1Error

instance ToError ParseError where
  toError = ParseErr

instance ToError SemanticError where
  toError = SemanticErr

-- | Operator for combining multiple steps in a pipeline together,
--   converting each step to have a common error type.
(>->) :: (Monad m, ToError e1, ToError e2)
      => (a -> ExceptT e1 m b)
      -> (b -> ExceptT e2 m c)
      -> (a -> ExceptT X1Error m c)
f >-> g = wrap f >=> wrap g where
  wrap h = withExceptT toError . h


parse :: FilePath -> ExceptT ParseError IO (Module Decl)
parse path = do
  content <- liftIO $ TIO.readFile path
  liftEither $ parseFile path content

semanticAnalysis :: FilePath -> Module Decl -> ExceptT SemanticError IO (Module Decl)
semanticAnalysis path decls =
  case runSA path decls of
    Ok -> pure decls
    Err e -> throwError e


compile :: FilePath -> IO ()
compile path = runExceptT pipeline $> () where
  pipeline = pipeline path
  runPipeline = parse
             >-> semanticAnalysis path
  --         >-> typeCheck
  --         >-> ...

