module Test.Main where

import Prelude

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, logShow)
import Control.MonadPlus (guard, (<|>))
import Data.Distribution (Dist, observe)

type Die = Int

die :: Dist Number Die
die = pure 1
  <|> pure 2
  <|> pure 3
  <|> pure 4
  <|> pure 5
  <|> pure 6

rollSeven :: Dist Number Die
rollSeven = do
  d1 <- die
  d2 <- die
  guard (d1 + d2 >= 7)
  pure (d1 + d2)

main :: forall eff. Eff (console :: CONSOLE | eff) Unit
main = logShow (observe rollSeven)
