
module Besra.Repl ( run ) where

import Protolude hiding ( StateT, evalStateT )
import Control.Monad.State.Strict
import System.IO (hFlush, stdout)
import Besra.Repl.Internal
import Besra.PrettyPrinter
import Besra.Parser
import Besra.Parser.Module ( declParser )


type LineNum = Int

newtype ReplState = ReplState { lineNum :: LineNum }

type Repl a = HaskelineT (StateT ReplState IO) a

type Handler a = Text -> Repl a


run :: IO ()
run = flip evalStateT initialState
    $ evalRepl banner interpretInput (toReplOptions options) cmdPrefix initializer
  where initialState = ReplState 1
        interpretInput = const (pure ())
        cmdPrefix = Just ':'
        options = [ (["q", "quit"], const quit)
                  , (["p", "prettyprint"], prettyPrintDecl)
                  ]
        initializer = pure ()
        banner = do
          lineNr <- gets lineNum
          pure $ "λ " <> show lineNr <> "> "

toReplOptions :: [([Text], Handler ())] -> [(Text, Handler ())]
toReplOptions = concatMap toReplOption where
  toReplOption (xs, handler) = map (, handler) xs

quit :: Repl ()
quit = printRepl "Quitting Besra REPL.\n" *> abort

printRepl :: Handler ()
printRepl text = putStr text *> liftIO (hFlush stdout)


prettyPrintDecl :: Handler ()
prettyPrintDecl input = do
  let parseResult = parse declParser "<interactive>" input
  case parseResult of
    Left err -> putStrLn $ formatError err
    Right decl -> putStrLn $ prettyFormat decl

