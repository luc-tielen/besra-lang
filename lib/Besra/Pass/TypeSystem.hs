
module Besra.Pass.TypeSystem ( pass ) where

import Protolude hiding ( pass )
import qualified Besra.TypeSystem as TS
import qualified Besra.TypeSystem.Monad as TS
import qualified Besra.Types.IR3 as IR3
import Besra.Types.Ann


type KI = KindInferred

pass :: Monad m => IR3.Module KI -> ExceptT TS.TypeError m (IR3.Module TC)
pass m =
  let (_subst, result) = TS.runTC initialEnv (TS.checkModule m)
   in either throwError pure result  -- TODO fix return type, fix runTC
   -- TODO cleanup pass (get rid of unknowns / simplify ast?)
   -- TODO hook up to main pipeline
  where initialEnv = mempty
