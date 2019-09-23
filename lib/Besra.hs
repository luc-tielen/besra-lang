
module Besra
  ( compile
  , compileFile
  , wrapErr
  , BesraError(..)
  ) where

import Protolude
import Control.Monad.Except
import Data.Text.IO as TIO
import qualified Data.Map as Map
import Besra.Parser ( ParseError, parseFile )
import qualified Besra.Types.IR1 as IR1
import qualified Besra.Types.IR2 as IR2
import qualified Besra.Types.IR3 as IR3
import Besra.SA
import Besra.Types.Id
import Besra.Types.Ann
import Besra.Types.Span
import qualified Besra.Pass.BalanceOperators as BalanceOperators
import qualified Besra.Pass.IR1To2 as IR1To2
import qualified Besra.Pass.IR2To3 as IR2To3
import qualified Besra.Pass.InferKinds as InferKinds
import Besra.Types.CompilerState
import Besra.TypeSystem.KindSolver ( Env(..), IKind(..), KindError(..) )


type Module1' = IR1.Module Parsed
type BalanceError' = BalanceOperators.BalanceError Parsed

-- TODO pretty print error
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

semanticAnalysis :: Monad m => FilePath -> Module1' -> ExceptT SemanticError m Module1'
semanticAnalysis path decls =
  case runSA path decls of
    Ok -> pure decls
    Err e -> throwError e

ir1To2 :: Module1' -> (IR2.Module Parsed, CompilerState Parsed)
ir1To2 x =
  let (ast, IR1To2.PassState adts traits impls) = IR1To2.pass x
      sp = Span 0 0
      arrowK = IKArr sp (IStar sp) (IKArr sp (IStar sp) (IStar sp))
      kindEnv = Map.fromList [(Id "->", arrowK)]
      kEnv = Env kindEnv Map.empty
  in (ast, CompilerState adts traits impls kEnv)

wrapErr :: (Monad m, ToError e) => ExceptT e m a -> ExceptT BesraError m a
wrapErr = withExceptT toError

compileFile
  :: FilePath
  -> IO (Either BesraError
                ( IR3.Module KindInferred
                , CompilerState KindInferred))
compileFile path = runExceptT pipeline where
  pipeline = do
    parsed <- wrapErr $ parse path
    compile path parsed

compile
  :: Monad m
  => FilePath
  -> IR1.Module Parsed
  -> ExceptT BesraError m
            ( IR3.Module KindInferred
            , CompilerState KindInferred)
compile path parsed = do
    balanced <- wrapErr $ BalanceOperators.pass parsed
    analyzed <- wrapErr $ semanticAnalysis path balanced
    let (ir2, compState) = ir1To2 analyzed
    (mod2, compState') <- wrapErr $ InferKinds.pass compState ir2
    let ir3 = IR2To3.pass compState' mod2
    pure (ir3, compState')

