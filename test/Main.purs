module Test.Main where

import Prelude

import Data.Int
import Data.Tuple
import Data.Distribution

import Control.Alt
import Control.MonadPlus

import Control.Monad.Eff.Console

type Die = Int

die :: Dist Int Die
die = pure 1
  <|> pure 2
  <|> pure 3
  <|> pure 4 
  <|> pure 5 
  <|> pure 6
  
twoDice :: Dist Int (Tuple Die Die)
twoDice = Tuple <$> die <*> die

rollSeven :: Dist Int (Tuple Die Die)
rollSeven = do
  t@(Tuple d1 d2) <- twoDice
  guard $ d1 + d2 == 7
  return t

main = print (observe rollSeven)
