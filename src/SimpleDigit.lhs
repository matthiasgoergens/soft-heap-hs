> module SimpleDigit where
> import Simple qualified as S
> import Data.Ord (comparing)
> import Control.Arrow ((>>>))
> import Data.List qualified as DL
> import Data.Maybe (catMaybes)
> import Data.Monoid qualified as DM
> import Data.Functor ((<&>))
> import BatchHybrid qualified as B
  import qualified Control.Arrow as later
  
Our module Simple already has something like a digit, a sorted list.Arrow

Now we need to implement increment, add and carry etc.

% > newtype Digit unc pool a = Digit [S.Cons unc pool a]
% >   deriving (Eq, Ord, Show, Functor)

Implement addition, increment etc, but worry about mendable and findable order.

> type Digit = Int

Can we understand meldable vs findable order in terms of zippers?

% > data Findable a = Findable { ranked :: [a], here :: a, rest :: Findable a }
% >   deriving (Eq, Ord, Show, Functor)


% > find = here

% % > popMin :: Ord a => Findable a -> (a, Maybe (Findable a))
% % > popMin (Findable [] _ _) = error "popMin: empty"

% > meld :: Ord a => (a -> a -> a) -> Findable a -> Findable a -> Findable a
% > meld op xx yy = case (ranked xx, ranked yy) of
% >   (x:xs, y:ys) ->
% >       let _ = op x y
% >       in meld op (xx {ranked = xs}) (yy {ranked = ys})
% >   ([], y:ys) ->
% >   

Ok, this is too hard.  Do findable order later.

> type Meldable a = [a]

> meld :: (a -> a -> a) -> Meldable a -> Meldable a -> Meldable a
> meld op x [] = x
> meld op [] y = y
> meld op (x:xs) (y:ys) = op x y : meld op xs ys

> normalise :: (a -> (a, a)) -> Meldable a -> Meldable a
> normalise op [] = []
> normalise op (x:xs) = let (here, carry) = op x in here : normalise op (meld op [carry] xs)
