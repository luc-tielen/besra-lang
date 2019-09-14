
module Besra.Repl ( run ) where

import Protolude hiding ( StateT, evalStateT, mod )
import Control.Monad.State.Strict hiding ( runState )
import System.IO (hFlush, stdout)
import Besra.Repl.Internal
import Besra.Repl.Parser
import Besra.PrettyPrinter
import Besra.Types.IR1 ( Module(..), Decl, Expr )
import Besra.Types.Ann
import qualified Besra
import qualified Besra.Pass.IR1To2 as IR1To2
import qualified Besra.Pass.InferKinds as IK
import qualified Besra.TypeSystem.KindSolver as K
import Besra.Types.CompilerState
import Besra.Types.Kind


type Expr' = Expr Parsed
type Decl' = Decl Parsed
type Module' = Module Parsed

type LineNum = Int

data ReplState
  = ReplState
  { lineNum :: LineNum
  , decls :: Module'
  }

type Repl a = HaskelineT (StateT ReplState IO) a

type Handler a = Text -> Repl a


run :: IO ()
run = flip evalStateT initialState
    $ evalRepl banner handleInput (toReplOptions options) cmdPrefix initializer
  where
    initialState = ReplState 1 (Module [])
    cmdPrefix = Just ':'
    options = [ (["q", "quit"], const quit)
              , (["p", "prettyprint"], prettyPrint)
              , (["d", "debug"], debug)
              , (["k", "kind"], kindInfo)
              -- TODO "t" / "type" commands
              ]
    initializer = pure ()
    banner = do
      lineNr <- gets lineNum
      pure $ "λ " <> show lineNr <> "> "

toReplOptions :: [([Text], Handler ())] -> [(Text, Handler ())]
toReplOptions = concatMap toReplOption where
  toReplOption (xs, handler) = map (, handler) xs

handleInput :: Handler ()
handleInput = withParsedInput $ either handleExpr handleDecl where
  handleExpr _ = do
    incrReplLine
    pure ()  -- TODO actual implementation later
  handleDecl decl = do
    -- TODO run SA before appending blindly
    incrReplLine
    modify $ \s -> s { decls = Module $ decl : unModule (decls s) }

quit :: Repl ()
quit = printlnRepl "Quitting Besra REPL." *> abort

incrReplLine :: Repl ()
incrReplLine = modify $ \s -> s { lineNum = lineNum s + 1 }

printRepl :: Handler ()
printRepl text = putStr text *> liftIO (hFlush stdout)

printlnRepl :: Handler ()
printlnRepl = printRepl . (<> "\n")

prettyPrint :: Handler ()
prettyPrint = withParsedInput $ either pp pp where
  pp :: Pretty a => a -> Repl ()
  pp = printlnRepl . prettyFormat

debug :: Handler ()
debug = withParsedInput $ either debug' debug' where
  debug' ast = do
    printlnRepl "Debug info:"
    printlnRepl $ "Parsed AST: " <> show ast
    printlnRepl $ "Pretty printed AST: " <> prettyFormat ast

-- TODO refactor/cleanup
kindInfo :: Handler ()
kindInfo input = do
  let parseResult = parse typeParser "<interactive>" input
  case parseResult of
    Left err -> handleParseError err
    Right ty -> do
      mod <- gets decls
      runExceptT (Besra.compile "<interactive>" mod) >>= \case
        Left err -> printlnRepl $ show err
        Right (_, CompilerState _ _ _ (K.Env kEnv _)) -> do
          let (ty2, _) = runState (IR1To2.desugar ty) (IR1To2.PassState [] [] [])
          printlnRepl $ prettyFormat $ kind $ runReader (IK.enrich ty2) kEnv


withParsedInput :: (Either Expr' Decl' -> Repl ()) -> Handler ()
withParsedInput f input = do
  let parseResult = parse exprOrDeclParser "<interactive>" input
  case parseResult of
    Left err -> handleParseError err
    Right ast -> f ast

handleParseError :: ParseError -> Repl ()
handleParseError = printlnRepl . formatError

