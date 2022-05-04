
module Data.Emptiable (Emptiable (..)) where

import Control.Applicative hiding (Alternative (..))
import qualified Control.Applicative as Alternative
import Data.Monoid
import qualified Data.Functor.Compose as Functor
import qualified Data.Functor.Product as Functor
import Data.Proxy
import Text.ParserCombinators.ReadP as ReadP
import Text.ParserCombinators.ReadPrec as ReadPrec
-- GHC
import GHC.Generics
-- Containers
import qualified Data.IntMap as IntMap
import qualified Data.Map as Map
import qualified Data.Sequence as Seq
import qualified Data.Set as Set


class Emptiable m where
{-^ @empty :: m a@ does not contain any @a@s.
All of the laws should be obvious. Most of the laws are "free" if @m@ isn't a GADT.
Laws:

prop> 'seq' empty () = ()
@empty@ can't be bottom in weak head normal form. It can evaluate to bottom (nonterminating or error) in a specific context. (For example, 'throwIO' could be used to create a valid @empty@ for @IO@; 'error' could not.)

> Foldable m
prop> 'null' empty == True -- "free"

@'Functor' m@
prop> \ f -> fmap f empty = empty -- "free"

> Applicative m
prop> \ mx -> empty '<*>' mx  =  empty '<*' mx
prop> \ mf -> mf '<*>' empty  =  mf '*>' empty
prop> \ mx -> empty '<*>' mx '<*>' empty  =  empty
'empty' must always be an anihilative zero for 'Applicative' multiplication rather than a unit, but effects performed before reaching 'empty' may be impossible to undo.

> Monad m
prop> \ f -> empty '>>=' f = empty -- "free"

> Alternative m
prop> \ mx -> empty '<|>' mx  =  mx  =  mx '<|>' empty
The 'Alternative' requirement implies that 'empty' = 'Control.Applicative.empty'.
> MonadPlus m
prop> \ mx -> 'mplus' empty mx = mx = 'mplus' mx empty
The 'MonadPlus' requirement implies that 'empty' = 'Control.Monad.mzero'.
-}
   empty :: m a


instance Emptiable Proxy where empty = Proxy
instance Emptiable Maybe where empty = Nothing
instance Emptiable [] where empty = []
instance Emptiable ZipList where empty = ZipList empty
-- instance Emptiable IO where empty = Alternative.empty -- This instance will be invalid until (<|>) and mplus have special handling for mzero.
instance Emptiable ReadP.ReadP where empty = ReadP.pfail
instance Emptiable ReadPrec.ReadPrec where empty = ReadPrec.pfail
instance (Emptiable m)=> Emptiable (Alt m) where empty = Alt empty
instance (Emptiable m)=> Emptiable (Ap m) where empty = Ap empty
instance (Emptiable m)=> Emptiable (Functor.Compose m n) where empty = Functor.Compose empty
instance (Emptiable m, Emptiable n)=> Emptiable (Functor.Product m n) where empty = Functor.Pair empty empty

--- Generics Instances ---
instance Emptiable U1 where empty = U1
instance (Emptiable m)=> Emptiable (Rec1 m) where empty = Rec1 empty
instance (Emptiable m)=> Emptiable (M1 i c m) where empty = M1 empty
-- no instance for (:+:) because we don't know which empty to use.
instance (Emptiable m, Emptiable n)=> Emptiable ((:*:) m n) where empty = empty :*: empty
instance (Emptiable m)=> Emptiable ((:.:) m n) where empty = Comp1 empty

--- Containers Instances ---
instance Emptiable IntMap.IntMap where empty = IntMap.empty
instance Emptiable (Map.Map i) where empty = Map.empty
instance Emptiable Seq.Seq where empty = Seq.empty
instance Emptiable Set.Set where empty = Set.empty
