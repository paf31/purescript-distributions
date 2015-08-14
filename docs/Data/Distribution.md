## Module Data.Distribution

This module defines a monad of _distributions_, which generalizes the probability monad
to an arbitrary `Semiring`.

#### `Dist`

``` purescript
data Dist p a
```

A distribution of values of type `a`, with "probabilities" in some `Semiring p`.

##### Instances
``` purescript
instance functorDist :: Functor (Dist p)
instance applyDist :: (Semiring p) => Apply (Dist p)
instance applicativeDist :: (Semiring p) => Applicative (Dist p)
instance bindDist :: (Semiring p) => Bind (Dist p)
instance monadDist :: (Semiring p) => Monad (Dist p)
instance altDist :: Alt (Dist p)
instance plusDist :: Plus (Dist p)
instance alternativeDist :: (Semiring p) => Alternative (Dist p)
instance monadPlusDist :: (Semiring p) => MonadPlus (Dist p)
```

#### `dist`

``` purescript
dist :: forall p a. List (Tuple p a) -> Dist p a
```

Create a distribution from a list of values and probabilities.

#### `observe`

``` purescript
observe :: forall p a. (Semiring p, Eq a) => Dist p a -> List (Tuple p a)
```

Unpack the observations in a distribution, combining any probabilities for
duplicate observations.


