-- | This module defines a monad of _distributions_, which generalizes the probability monad
-- | to an arbitrary `Semiring`.

module Data.Distribution
  ( Dist(..)
  , dist
  , observe
  ) where

import Prelude
import Data.List.NonEmpty as NEL
import Control.MonadPlus (class MonadPlus, class MonadZero, class Alternative)
import Control.Plus (class Plus, class Alt, empty, (<|>))
import Data.Foldable (foldMap)
import Data.Function (on)
import Data.List (List, sortBy, groupBy)
import Data.Monoid.Additive (Additive(..))
import Data.Newtype (unwrap)
import Data.Tuple (Tuple(..), fst, snd)

-- | A distribution of values of type `a`, with "probabilities" in some `Semiring p`.
data Dist p a = Dist (List (Tuple p a))

-- | Create a distribution from a list of values and probabilities.
dist :: forall p a. List (Tuple p a) -> Dist p a
dist = Dist

runDist :: forall p a. Dist p a -> List (Tuple p a)
runDist (Dist d) = d

-- | Unpack the observations in a distribution, combining any probabilities for
-- | duplicate observations.
observe :: forall p a. (Semiring p, Ord a) => Dist p a -> List (Tuple p a)
observe =
    map collect <<< groupBy (eq `on` snd) <<< sortBy (comparing snd) <<< runDist
  where
    collect :: NEL.NonEmptyList (Tuple p a) -> Tuple p a
    collect d = case NEL.head d of
      Tuple _ a ->
        let p = unwrap (foldMap (Additive <<< fst) d)
        in Tuple p a

instance functorDist :: Functor (Dist p) where
  map f (Dist d) = Dist (map (map f) d)

instance applyDist :: Semiring p => Apply (Dist p) where
  apply = ap

instance applicativeDist :: Semiring p => Applicative (Dist p) where
  pure a = Dist (pure (Tuple one a))

instance bindDist :: Semiring p => Bind (Dist p) where
  bind d f = Dist do
    Tuple p1 a <- runDist d
    Tuple p2 b <- runDist (f a)
    pure (Tuple (p1 * p2) b)

instance monadDist :: Semiring p => Monad (Dist p)

instance altDist :: Alt (Dist p) where
  alt d1 d2 = Dist (runDist d1 <|> runDist d2)

instance plusDist :: Plus (Dist p) where
  empty = Dist empty

instance alternativeDist :: Semiring p => Alternative (Dist p)

instance monadZeroDist :: Semiring p => MonadZero (Dist p)

instance monadPlusDist :: Semiring p => MonadPlus (Dist p)
