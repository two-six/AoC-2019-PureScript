module Main where

import Prelude

import Control.Alternative (guard)
import Data.Array (head, index, length, modifyAt, (..))
import Data.Int (fromString)
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.String (Pattern(..), split)
import Effect (Effect)
import Effect.Console (log)
import Node.Encoding (Encoding(..))
import Node.FS.Sync (readTextFile)

main :: Effect Unit
main = do
  text <- readTextFile UTF8 "input.txt"
  log $ "Silver: " <> (show $ silver text)
      <> "\nGold: " <> (show $ gold text)

readNumbers :: String -> Array Int
readNumbers = map (maybe 0 identity <<< fromString) <<< split (Pattern ",")

opcode :: Array Int -> Int -> (Int -> Int -> Int) -> Array Int
opcode arr n f =
  fromMaybe [] opcodeMaybe
  where
    opcodeMaybe :: Maybe (Array Int)
    opcodeMaybe = do
      a <- index arr (n+1)
      b <- index arr (n+2)
      c <- index arr (n+3)
      x <- index arr a
      y <- index arr b
      modifyAt c (\_ -> f x y) arr

cmt :: Array Int -> Int -> Array Int
cmt arr' n =
  case index arr' n of
  Just(1) -> cmt (opcode arr' n (+)) (n+4)
  Just(2) -> cmt (opcode arr' n (*)) (n+4)
  _       -> arr'

computeS :: Array Int -> Array Int
computeS arr =
  fromMaybe [] computeSMaybe
  where
    computeSMaybe :: Maybe (Array Int)
    computeSMaybe = do
      arr1 <- modifyAt 1 (\_ -> 12) arr
      arr2 <- modifyAt 2 (\_ -> 2) arr1
      pure $ cmt arr2 0

silver :: String -> Int
silver str = fromMaybe 0 (head $ computeS $ readNumbers str)

modifiedArray :: Int -> Int -> Array Int -> Array Int
modifiedArray x y arr =
  fromMaybe [] modifiedArrayMaybe
  where
    modifiedArrayMaybe :: Maybe (Array Int)
    modifiedArrayMaybe = do
      arr1 <- modifyAt 1 (\_ -> x) arr
      arr2 <- modifyAt 2 (\_ -> y) arr1
      pure arr2

computeG :: Array Int -> Array Int
computeG arr = do
  x <- 0 .. (ln-1)
  y <- 0 .. (ln-1)
  guard $ (head $ cmt(modifiedArray x y arr) 0) == Just(19690720)
  pure (100 * x + y)
  where
    ln = length arr

gold :: String -> Int
gold str = fromMaybe 0 $ head (computeG $ readNumbers str)
