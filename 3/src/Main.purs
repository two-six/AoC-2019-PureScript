module Main where

import Prelude

import Data.Array (filter, head, sort, tail, (..))
import Data.Foldable (foldMap)
import Data.Int (fromString)
import Data.Map (Map)
import Data.Map (empty, filter, fromFoldable, union, unionWith, keys) as Map
import Data.Maybe (fromMaybe)
import Data.Ord (abs)
import Data.String (Pattern(..), drop, split, take)
import Data.Tuple (Tuple(..), fst, snd)
import Effect (Effect)
import Effect.Console (log)
import Node.Encoding (Encoding(..))
import Node.FS.Sync (readTextFile)

main :: Effect Unit
main = do
  text <- readTextFile UTF8 "input.txt"
  log $ "Silver: " <> (show $ silver text)
      <> "\nGold: " <> (show $ gold text)

silver :: String -> Int
silver str =
        fromMaybe 0
        $ head
        $ sort
        $ foldMap (\v -> [(+) (abs $ fst v) (abs $ snd v)])
        $ Map.keys
        $ Map.filter (\v -> v == 2)
        $ placeWires
        $ (readWires str)

gold :: String -> Int
gold str =
   fromMaybe 0 $ head $ sort $ lenghts wires crosses
  where
    wires = readWires str
    crosses = foldMap (\v -> [v])
              $ Map.keys
              $ Map.filter (\v -> v == 2)
              $ placeWires
              $ wires
    lenghts :: Array (Array (Tuple String Int)) -> Array (Tuple Int Int) -> Array Int
    lenghts ains xy = map (\v -> addStepsToCross ains v) xy

addStepsToCross :: Array (Array (Tuple String Int)) -> Tuple Int Int -> Int
addStepsToCross [] _ = 0
addStepsToCross ains fxy =
    (+) (stepsToCross ins (Tuple 0 0) (Tuple x y)) (addStepsToCross r fxy)
  where
    x = fst fxy
    y = snd fxy
    ins = fromMaybe [] $ head ains
    r = fromMaybe [] $ tail ains

stepsToCross :: Array (Tuple String Int) -> Tuple Int Int -> Tuple Int Int -> Int
stepsToCross ains xy fxy =
    if y == fy && ((nx >= fx && x <= fx) || (nx <= fx && x >= fx)) then
      if x > fx then x - fx else fx - x
    else if  x == fx && ((ny >= fy && y < fy) || (ny <= fy && y > fy)) then
      if y > fy then y - fy else fy - y
    else
      v + (stepsToCross r (Tuple nx ny) fxy)
  where
    x = fst xy
    y = snd xy
    ins = fromMaybe (Tuple "" 0) $ head ains
    r = fromMaybe [] $ tail ains
    d = fst ins
    v = snd ins
    fx = fst fxy
    fy = snd fxy
    nx = case d of
         "R" -> x + v
         "L" -> x - v
         _   -> x
    ny = case d of
         "U" -> y + v
         "D" -> y - v
         _   -> y


readWires :: String -> Array (Array (Tuple String Int))
readWires str =
  map (\l -> extractTuples l) lines
  where
    lines = filter (\el -> el /= "") $ split (Pattern "\n") str
    extractTuples :: String -> Array (Tuple String Int)
    extractTuples line =
      map (\el -> extractTuple el) elements
      where
        elements = split (Pattern ",") line

extractTuple :: String -> (Tuple String Int)
extractTuple str = Tuple (take 1 str) (fromMaybe 0 $ fromString $ drop 1 str)

placeWires :: Array (Array (Tuple String Int)) -> Map (Tuple Int Int) Int
placeWires [] = Map.empty
placeWires wires =
  Map.unionWith (\_ _ -> 2) (placeWire wire (Tuple 0 0)) (placeWires r)
  where
    wire = fromMaybe [] $ head wires
    r = fromMaybe [] $ tail wires

placeWire :: Array (Tuple String Int) -> Tuple Int Int -> Map (Tuple Int Int) Int
placeWire [] _ = Map.empty
placeWire line xy =
  Map.union (placeLine xy ins) (placeWire r (Tuple nx ny))
  where
    ins = fromMaybe (Tuple "" 0) $ head line
    r = fromMaybe [] $ tail line
    d = fst ins
    v = snd ins
    nx = case d of
           "L" -> (fst xy) - v
           "R" -> (fst xy) + v
           _   -> fst xy
    ny = case d of
           "U" -> (snd xy) + v
           "D" -> (snd xy) - v
           _   -> snd xy

placeLine :: Tuple Int Int -> Tuple String Int -> Map (Tuple Int Int) Int
placeLine xy dv = Map.fromFoldable arr
  where
  x = fst xy
  y = snd xy
  d = fst dv
  v = snd dv
  arr = map (\el -> case d of
                       "L" -> Tuple (Tuple (x-el) y) 1
                       "R" -> Tuple (Tuple (x+el) y) 1
                       "U" -> Tuple (Tuple x (y+el)) 1
                       "D" -> Tuple (Tuple x (y-el)) 1
                       _   -> Tuple (Tuple 0 0) 0) (1 .. v)
