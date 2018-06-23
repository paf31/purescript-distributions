module Test.Main where

import Prelude

import Control.MonadPlus (guard, (<|>))
import Data.Distribution (Dist, observe)
import Effect (Effect)
import Effect.Console (logShow)

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

main :: Effect Unit
main = logShow (observe rollSeven)
