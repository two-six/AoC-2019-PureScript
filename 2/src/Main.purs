module Main where

import Prelude

import Control.Alternative (guard)
import Control.Monad.Writer (Writer, runWriter, tell)
import Data.Array (head, index, length, modifyAt, (..))
import Data.Int (fromString)
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.String (Pattern(..), split)
import Data.Tuple (fst)
import Effect (Effect)
import Effect.Console (log)
import Node.Encoding (Encoding(..))
import Node.FS.Sync (readTextFile)

main :: Effect Unit
main = do
  text <- readTextFile UTF8 "input.txt"
  log $ "Silver: " <> (show $ fst $ runWriter $ silver text)
      <> "\nGold: " <> (show $ gold text)

readNumbers :: String -> Array Int
readNumbers = map (maybe 0 identity <<< fromString) <<< split (Pattern ",")

opcode :: Array Int -> Int -> (Int -> Int -> Int) -> Array Int
opcode arr n f =
  fromMaybe [] (opcodeMaybe arr n f)
  where
    opcodeMaybe :: Array Int -> Int -> (Int -> Int -> Int) -> Maybe (Array Int)
    opcodeMaybe arr' n' f' = do
      a <- index arr' (n'+1)
      b <- index arr' (n'+2)
      c <- index arr' (n'+3)
      x <- index arr' a
      y <- index arr' b
      modifyAt c (\_ -> f' x y) arr

cmt :: Array Int -> Int -> Array Int
cmt arr' n =
  case index arr' n of
  Just(1) -> cmt (opcode arr' n (+)) (n+4)
  Just(2) -> cmt (opcode arr' n (*)) (n+4)
  _       -> arr'

computeS :: Array Int -> Array Int
computeS arr =
  cmt (fromMaybe [] (modifyAt 2 (\_ -> 2) (fromMaybe [] (modifyAt 1 (\_ -> 12) arr)))) 0

silver :: String -> Writer (Array Int) Int
silver str = do
  tell nums
  pure
    $ fromMaybe 0
    $ head
    $ nums
  where
    nums = computeS $ readNumbers str

modifiedArray :: Int -> Int -> Array Int -> Array Int
modifiedArray x y arr =
  fromMaybe [] (modifyAt 2 (\_ -> y) (fromMaybe [] (modifyAt 1 (\_ -> x) arr)))

computeG :: Array Int -> Array Int
computeG arr = do
  x <- 0 .. (ln-1)
  y <- 0 .. (ln-1)
  guard $ (fromMaybe 0 $ head $ cmt(modifiedArray x y arr) 0) == 19690720
  pure (100 * x + y)
  where
    ln = length arr

gold :: String -> Int
gold str = fromMaybe 0 $ head (computeG $ readNumbers str)
