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
Functor (Dist p)
(Semiring p) => Apply (Dist p)
(Semiring p) => Applicative (Dist p)
(Semiring p) => Bind (Dist p)
(Semiring p) => Monad (Dist p)
Alt (Dist p)
Plus (Dist p)
(Semiring p) => Alternative (Dist p)
(Semiring p) => MonadZero (Dist p)
(Semiring p) => MonadPlus (Dist p)
```

#### `dist`

``` purescript
dist :: forall p a. List (Tuple p a) -> Dist p a
```

Create a distribution from a list of values and probabilities.

#### `observe`

``` purescript
observe :: forall p a. (Semiring p, Ord a) => Dist p a -> List (Tuple p a)
```

Unpack the observations in a distribution, combining any probabilities for
duplicate observations.


