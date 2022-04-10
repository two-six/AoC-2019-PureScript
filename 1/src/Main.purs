module Main where

import Prelude

import Control.Monad.State (State, execState)
import Control.Monad.State.Class (modify)
import Data.Array (filter)
import Data.Foldable (traverse_)
import Data.Int (fromString)
import Data.Maybe (maybe)
import Data.String (Pattern(..), split)
import Effect (Effect)
import Effect.Class.Console (log)
import Node.Encoding (Encoding(..))
import Node.FS.Sync (readTextFile)

main :: Effect Unit
main = do
  text <- readTextFile UTF8 "input.txt"
  log $ "Silver: " <> (show $ silver text)
               <> "\nGold: " <> (show $ gold text)

readNumbers :: String -> Array Int
readNumbers = map (maybe 0 identity <<< fromString) <<< filter (\c -> c /= "") <<< split (Pattern "\n")

calculate :: Array Int -> State Int Unit
calculate = traverse_ \n -> modify \sum -> sum + (n/3-2)

silver :: String -> Int
silver str = execState (calculate $ readNumbers str) 0

gold :: String -> Int
gold str = execState (calculate' $ readNumbers str) 0

parseElementG :: Int -> Int
parseElementG x =
  let
    el = x/3-2
  in
    if el > 0 then
      (+) el $ parseElementG el
    else
      0

calculate' :: Array Int -> State Int Unit
calculate' = traverse_ \n -> modify \sum -> sum + (parseElementG n)
