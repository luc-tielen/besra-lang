
module Besra ( compile ) where

import Protolude
import Control.Monad.Except
import Data.Text.IO as TIO
import qualified Data.Map as Map
import Besra.Parser ( ParseError, parseFile )
import qualified Besra.Types.IR1 as IR1
import qualified Besra.Types.IR2 as IR2
import Besra.SA
import Besra.Types.Id
import Besra.Types.Ann
import Besra.Types.Span
import qualified Besra.Pass.BalanceOperators as BalanceOperators
import qualified Besra.Pass.IR1To2 as IR1To2
import qualified Besra.Pass.InferKinds as InferKinds
import Besra.Types.CompilerState
import Besra.TypeSystem.KindSolver ( Env(..), IKind(..), KindError(..) )


type Module1' = IR1.Module Parsed
type BalanceError' = BalanceOperators.BalanceError Parsed

data BesraError
  = ParseErr ParseError
  | BalanceErr BalanceError'
  | SemanticErr SemanticError
  | InferKindErr KindError
  deriving (Eq, Show)


class ToError e where
  toError :: e -> BesraError

instance ToError ParseError where
  toError = ParseErr

instance ToError BalanceError' where
  toError = BalanceErr

instance ToError SemanticError where
  toError = SemanticErr

instance ToError KindError where
  toError = InferKindErr

instance ToError BesraError where
  toError = identity


parse :: FilePath -> ExceptT ParseError IO Module1'
parse path = do
  content <- liftIO $ TIO.readFile path
  liftEither $ parseFile path content

semanticAnalysis :: FilePath -> Module1' -> ExceptT SemanticError IO Module1'
semanticAnalysis path decls =
  case runSA path decls of
    Ok -> pure decls
    Err e -> throwError e

ir1To2 :: Module1' -> (IR2.Module Parsed, CompilerState Parsed)
ir1To2 x =
  let (ast, IR1To2.PassState adts traits impls) = IR1To2.pass x
      sp = Span  0 0
      arrowK = IKArr sp (IStar sp) (IKArr sp (IStar sp) (IStar sp))
      kindEnv = Map.fromList [(Id "->", arrowK)]
      kEnv = Env kindEnv Map.empty
  in (ast, CompilerState adts traits impls kEnv)

wrapErr :: ToError e => ExceptT e IO a -> ExceptT BesraError IO a
wrapErr = withExceptT toError

compile :: FilePath
        -> IO (Either BesraError
                      ( IR2.Module KindInferred
                      , CompilerState KindInferred))
compile path = runExceptT pipeline where
  pipeline = runPipeline
  runPipeline = do
    parsed <- wrapErr $ parse path
    balanced <- wrapErr $ BalanceOperators.pass parsed
    analyzed <- wrapErr $ semanticAnalysis path balanced
    let (ir2, compState) = ir1To2 analyzed
    wrapErr $ InferKinds.pass compState ir2

