module Main where

import Prelude

import Control.Alternative (guard)
import Data.Array (any, group, length, nub, sort, (..))
import Data.Array.NonEmpty (length) as NArr
import Data.Int (decimal, toStringAs)
import Data.String (Pattern(..), split)
import Effect (Effect)
import Effect.Console (log)

main :: Effect Unit
main = do
  log $ "Silver: " <> (show silver)
      <> "\nGold: " <> (show gold)

silver :: Int
silver = length $ findNumbers 372037 905157

gold :: Int
gold = length $ findNumbersG 372037 905157

findNumbers :: Int -> Int -> Array Int
findNumbers x y = do
  r <- x .. y
  guard $ checkNumber r
  pure r

checkNumber :: Int -> Boolean
checkNumber n =
    (length an > length (nub an) && sort an == an)
  where
    an = split (Pattern "") $ toStringAs decimal n

findNumbersG :: Int -> Int -> Array Int
findNumbersG x y = do
  r <- x .. y
  guard $ checkNumberG r
  pure r

checkNumberG :: Int -> Boolean
checkNumberG n =
    (sort an == an && length an > length (nub an) && pair)
  where
    an = split (Pattern "") $ toStringAs decimal n
    pair = any (\v -> NArr.length v == 2) $ group an
