
{-# LANGUAGE UndecidableInstances #-}

module X1.Types.Expr1.Module ( Decl(..), Module(..) ) where

import Protolude hiding ( Fixity(..) )
import X1.Types.Id
import X1.Types.Ann
import X1.Types.Fixity
import X1.Types.Expr1.Expr
import X1.Types.Expr1.ADT
import X1.Types.Expr1.Trait
import X1.Types.Expr1.Impl
import X1.Types.Expr1.TypeAnn


data Decl (ph :: Phase)
  = TypeAnnDecl TypeAnn
  | DataDecl ADT
  | TraitDecl Trait
  | ImplDecl (Impl ph)
  | BindingDecl (Binding ph)
  | FixityDecl Fixity Int Id

newtype Module (ph :: Phase)
  = Module [Decl ph]

deriving instance Eq (Ann ph) => Eq (Decl ph)
deriving instance Show (Ann ph) => Show (Decl ph)
deriving instance Eq (Ann ph) => Eq (Module ph)
deriving instance Show (Ann ph) => Show (Module ph)

