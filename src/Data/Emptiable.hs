
module Data.Emptiable (Emptiable (..)) where

import Control.Applicative hiding (Alternative (..))
import qualified Control.Applicative as Alternative
import qualified Data.Functor.Compose as Functor
import qualified Data.Functor.Product as Functor
import Data.Proxy
import Text.ParserCombinators.ReadP as ReadP
import Text.ParserCombinators.ReadPrec as ReadPrec
import GHC.Generics


class Emptiable m where
{-^ @empty :: m a@ does not contain any @a@s.
All of the laws should be obvious. Most of the laws are "free" if @m@ isn't a GADT.
Laws:
> Functor m => fmap f empty = empty -- "free"

> Applicative m =>
> empty '<*>' mx  =  empty '<*' mx
> mf '<*>' empty  =  mf '*>' empty
> empty '<*>' mx '<*>' empty  =  empty
The last one tells you that 'empty' must always be an anihilative zero for 'Applicative' multiplication rather than a unit, but acknowledges that effects performed before reaching 'empty' may be impossible to undo. So, for example, we'd need a (hypothetical) @class (Semigroup a)=> Anihilative a where nihil :: a@ to have @empty :: (Anihilative c)=> Const c a@.

> Monad m => empty '>>=' f = empty -- "free"

> Foldable m => 'null' empty == True -- "free"

> Alternative m => empty '<|>' mx = mx
This can't require @mx <|> empty = mx@, because, e.g., @'throwIO' "Whoops!" <|> empty@ is @empty@ rather than @'throwIO' "Whoops!"@.

> MonadPlus m => mplus empty mx = mx
This can't require @mplus mx empty = mx@, because, e.g., @mplus ('throwIO' "Whoops!") empty@ is @empty@ rather than @'throwIO' "Whoops!"@.
-}
   empty :: m a


instance Emptiable Proxy where empty = Proxy
instance Emptiable Maybe where empty = Nothing
instance Emptiable [] where empty = []
instance Emptiable ZipList where empty = ZipList empty
instance Emptiable IO where empty = Alternative.empty
instance Emptiable ReadP.ReadP where empty = ReadP.pfail
instance Emptiable ReadPrec.ReadPrec where empty = ReadPrec.pfail
instance (Emptiable m)=> Emptiable (Functor.Compose m n) where empty = Functor.Compose empty
instance (Emptiable m, Emptiable n)=> Emptiable (Functor.Product m n) where empty = Functor.Pair empty empty

--- Generics Instances ---
instance Emptiable U1 where empty = U1
instance (Emptiable m)=> Emptiable (Rec1 m) where empty = Rec1 empty
instance (Emptiable m)=> Emptiable (M1 i c m) where empty = M1 empty
-- no instance for (:+:) because we don't know which empty to use.
instance (Emptiable m, Emptiable n)=> Emptiable ((:*:) m n) where empty = empty :*: empty
instance (Emptiable m)=> Emptiable ((:.:) m n) where empty = Comp1 empty
