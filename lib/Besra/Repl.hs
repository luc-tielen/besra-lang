
module Besra.Repl ( run ) where

import Protolude hiding ( StateT, evalStateT )
import Control.Monad.State.Strict
import System.IO (hFlush, stdout)
import Besra.Repl.Internal


type LineNum = Int

newtype ReplState = ReplState { lineNum :: LineNum }

type Repl a = HaskelineT (StateT ReplState IO) a


run :: IO ()
run = flip evalStateT initialState
    $ evalRepl banner interpretLine options cmdPrefix completer initializer
  where initialState = ReplState 1
        interpretLine = const (pure ())
        cmdPrefix = Just ':'
        options = [ ("q", const quit)
                  , ("quit", const quit)
                  ]
        initializer = pure ()
        banner = do
          lineNr <- gets lineNum
          pure $ "λ " <> show lineNr <> "> "

quit :: Repl ()
quit = printRepl "Quitting Besra REPL.\n" *> abort

printRepl :: Text -> Repl ()
printRepl text = putStr text *> liftIO (hFlush stdout)

completer :: Monad m => WordCompleter m
completer _ = pure []

