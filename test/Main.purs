module Test.Main where

import Data.Int
import Data.Tuple
import Data.Distribution

import Control.Alt
import Control.MonadPlus

import Debug.Trace

type Die = Int

die :: Dist Number Die
die = pure (fromNumber 1) 
  <|> pure (fromNumber 2) 
  <|> pure (fromNumber 3)
  <|> pure (fromNumber 4) 
  <|> pure (fromNumber 5) 
  <|> pure (fromNumber 6)
  
twoDice :: Dist Number (Tuple Die Die)
twoDice = Tuple <$> die <*> die

rollSeven :: Dist Number (Tuple Die Die)
rollSeven = do
  t@(Tuple d1 d2) <- twoDice
  guard $ d1 + d2 == fromNumber 7
  return t

main = do
  print (observe rollSeven)
