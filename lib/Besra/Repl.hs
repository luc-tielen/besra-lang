
module Besra.Repl ( run ) where

import Protolude hiding ( StateT, evalStateT )
import Control.Monad.State.Strict
import System.IO (hFlush, stdout)
import Besra.Repl.Internal
import Besra.PrettyPrinter
import Besra.Parser
import Besra.Parser.Module ( declParser )
import Besra.Types.IR1.Module ( Decl )
import Besra.Types.Ann


type LineNum = Int

newtype ReplState = ReplState { lineNum :: LineNum }

type Repl a = HaskelineT (StateT ReplState IO) a

type Handler a = Text -> Repl a


run :: IO ()
run = flip evalStateT initialState
    $ evalRepl banner interpretInput (toReplOptions options) cmdPrefix initializer
  where initialState = ReplState 1
        interpretInput = const (pure ())  -- TODO actual handling of new decls / exprs
        cmdPrefix = Just ':'
        options = [ (["q", "quit"], const quit)
                  , (["p", "prettyprint"], prettyPrintDecl)
                  , (["d", "debug"], debugDecl)
                  -- TODO "k" / "kind" and "t" / "type" commands
                  ]
        initializer = pure ()
        banner = do
          lineNr <- gets lineNum
          pure $ "λ " <> show lineNr <> "> "

toReplOptions :: [([Text], Handler ())] -> [(Text, Handler ())]
toReplOptions = concatMap toReplOption where
  toReplOption (xs, handler) = map (, handler) xs

quit :: Repl ()
quit = printlnRepl "Quitting Besra REPL." *> abort

{-
TODO increment line number when actual line is entered (not with options prefix)

incrReplLine :: Repl ()
incrReplLine = modify $ \s -> s { lineNum = lineNum s + 1 }
-}

printRepl :: Handler ()
printRepl text = putStr text *> liftIO (hFlush stdout)

printlnRepl :: Handler ()
printlnRepl = printRepl . (<> "\n")

prettyPrintDecl :: Handler ()
prettyPrintDecl = withParsedDecl $ printlnRepl . prettyFormat

debugDecl :: Handler ()
debugDecl = withParsedDecl $ \decl -> do
  printlnRepl "Debug info:"
  printlnRepl $ "Parsed AST: " <> show decl
  printlnRepl $ "Pretty printed AST: " <> prettyFormat decl

withParsedDecl :: (Decl 'Parsed -> Repl ()) -> Text -> Repl ()
withParsedDecl f input = do
  let parseResult = parse declParser "<interactive>" input
  case parseResult of
    Left err -> handleParseError err
    Right decl -> f decl

handleParseError :: ParseError -> Repl ()
handleParseError = printlnRepl . formatError

