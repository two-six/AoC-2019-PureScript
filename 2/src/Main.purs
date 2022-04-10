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
opcode a n f = do
  fromMaybe []
    $ modifyAt (fromMaybe 0 $ index a (n+3))
    (\_ -> f (fromMaybe 0 $ (index a (fromMaybe 0 $ index a (n+1)))) (fromMaybe 0 $ (index a (fromMaybe 0 $ index a (n+2))))) a

cmt :: Array Int -> Int -> Array Int
cmt arr' n =
  case index arr' n of
  Just(1) -> cmt (opcode arr' n (+)) (n+4)
  Just(2) -> cmt (opcode arr' n (*)) (n+4)
  _       -> arr'

computeS :: Array Int -> Array Int
computeS arr =
  cmt (fromMaybe [] (modifyAt 2 (\_ -> 2) (fromMaybe [] (modifyAt 1 (\_ -> 12) arr)))) 0

silver :: String -> Int
silver str = fromMaybe 0 (head $ computeS $ readNumbers str)

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
