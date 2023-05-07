> module SimpleDigit where
> import Simple qualified as S
> import Data.Ord (comparing)
> import Control.Arrow ((>>>))
> import Data.List qualified as DL
> import Data.Maybe (catMaybes)
> import Data.Monoid qualified as DM
> import Data.Functor ((<&>))
> import BatchHybrid qualified as B
  
Our module Simple already has something like a digit, a sorted list.Arrow

Now we need to implement increment, add and carry etc.

> newtype Digit unc pool a = Digit [S.Cons unc pool a]
>   deriving (Eq, Ord, Show, Functor)
