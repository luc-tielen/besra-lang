
{-# LANGUAGE GeneralizedNewtypeDeriving, UndecidableInstances #-}

module Besra.Repl.Internal
  ( HaskelineT
  , runHaskelineT

  , Cmd
  , Options
  , Command

  , evalRepl
  , abort
  , tryAction

  ) where

-- NOTE: this is pretty much a stripped down version of `repline` package
-- @ https://github.com/sdiehl/repline/, with some minor modifications to
-- user interactions (quitting the REPL, ...)

import Protolude hiding ( StateT, handle, throwIO )
import Prelude (String)  -- TODO remove
import qualified Data.Text as T
import qualified Data.List as List
import qualified System.Console.Haskeline as H
import System.Console.Haskeline.MonadException
import Control.Monad.Reader
import Control.Monad.State.Strict


-------------------------------------------------------------------------------
-- Haskeline Transformer
-------------------------------------------------------------------------------

newtype HaskelineT (m :: Type -> Type) a
  = HaskelineT { unHaskeline :: H.InputT m a }
 deriving ( Monad, Functor, Applicative, MonadIO
          , MonadException, MonadTrans, MonadHaskeline
          )

runHaskelineT :: MonadException m => H.Settings m -> HaskelineT m a -> m a
runHaskelineT s = H.runInputT s . H.withInterrupt . unHaskeline

class MonadException m => MonadHaskeline m where
  getInputLine :: String -> m (Maybe String)
  getInputChar :: String -> m (Maybe Char)
  outputStr    :: String -> m ()
  outputStrLn  :: String -> m ()

instance MonadException m => MonadHaskeline (H.InputT m) where
  getInputLine = H.getInputLine
  getInputChar = H.getInputChar
  outputStr    = H.outputStr
  outputStrLn  = H.outputStrLn

instance MonadState s m => MonadState s (HaskelineT m) where
  get = lift get
  put = lift . put

instance MonadReader r m => MonadReader r (HaskelineT m) where
  ask = lift ask
  local f (HaskelineT m) = HaskelineT $ H.mapInputT (local f) m

instance (MonadHaskeline m) => MonadHaskeline (StateT s m) where
  getInputLine = lift . getInputLine
  getInputChar = lift . getInputChar
  outputStr    = lift . outputStr
  outputStrLn  = lift . outputStrLn


-------------------------------------------------------------------------------
-- Repl
-------------------------------------------------------------------------------

type Cmd m = Text -> m ()
type Options m = [(Text, Cmd m)]
type Command m = Text -> m ()


-- | Wrap a HasklineT action so that if an interrupt is thrown the shell continues as normal.
tryAction :: MonadException m => HaskelineT m a -> HaskelineT m a
tryAction (HaskelineT f) = HaskelineT (H.withInterrupt loop)
    where loop = handle (\H.Interrupt -> loop) f

-- | Abort the current REPL loop, and continue.
abort :: MonadIO m => HaskelineT m a
abort = throwIO H.Interrupt

-- | Completion loop.
replLoop :: MonadException m
         => HaskelineT m String
         -> Command (HaskelineT m)
         -> Options (HaskelineT m)
         -> Maybe Char
         -> HaskelineT m ()
replLoop banner cmdM opts optsPrefix = loop where
  handleInput input = H.handleInterrupt exit $ cmdM input
  exit = pure ()
  loop = do
    bannerPrefix <- banner
    minput <- H.handleInterrupt (pure Nothing) $ getInputLine bannerPrefix
    case minput of
      Nothing -> exit
      Just "" -> loop
      Just (prefix:cmds)
        | null cmds -> handleInput (T.singleton prefix) *> loop
        | Just prefix == optsPrefix ->
          case List.words cmds of
            [] -> loop
            (cmd:args) -> do
              let optAction = optMatcher (T.pack cmd) opts args
              result <- H.handleInterrupt (pure Nothing) $ Just <$> optAction
              maybe exit (const loop) result
      Just input -> do
        handleInput (T.pack input)
        loop

-- | Match the options.
optMatcher :: MonadHaskeline m => Text -> Options m -> [String] -> m ()
optMatcher s [] _ = outputStrLn $ "No such command :" <> T.unpack s
optMatcher s ((x, m):xs) args
  | s `T.isPrefixOf` x = m (mconcat $ T.pack <$> List.intersperse " " args)
  | otherwise = optMatcher s xs args

-- | Evaluate the REPL logic into a MonadException context.
evalRepl :: MonadException m             -- Terminal monad ( often IO ).
         => HaskelineT m String          -- ^ Banner
         -> Command (HaskelineT m)       -- ^ Command function
         -> Options (HaskelineT m)       -- ^ Options list and commands
         -> Maybe Char                   -- ^ Optional command prefix ( passing Nothing ignores the Options argument )
         -> HaskelineT m a               -- ^ Initializer
         -> m ()
evalRepl banner cmd opts optsPrefix initz =
  runHaskelineT _readline (initz >> loop)
  where
    loop = replLoop banner cmd opts optsPrefix
    _readline = H.Settings
      { H.complete       = H.noCompletion
      , H.historyFile    = Just ".history"
      , H.autoAddHistory = True
      }

