module Main where

import Prelude

import Data.Array (drop, filter, head, length, take, (:))
import Data.Int (fromString)
import Data.Maybe (fromMaybe)
import Data.String (Pattern(..), split, trim)
import Effect (Effect)
import Effect.Console (log)
import Node.Encoding (Encoding(..))
import Node.FS.Sync (readTextFile)

main :: Effect Unit
main = do
  text <- readTextFile UTF8 "input.txt"
  log $ "Silver: " <> (show $ silver text)

readNumbers :: String -> Array Int
readNumbers = map (fromMaybe 0 <<< fromString) <<< split (Pattern "") <<< trim

silver :: String -> Int
silver str =
  let
    arr = groupArray (readNumbers str) (6*25)
    fArr = largestArrWithout0 arr
    oneArr = filter (_ == 1) fArr
    twoArr = filter (_ == 2) fArr
  in
   (length oneArr) * (length twoArr)

groupArray :: Array Int -> Int -> Array(Array Int)
groupArray arr n =
  let
    insFunc :: Array Int -> Int -> Array(Array Int) -> Array(Array Int)
    insFunc [] _ res = res
    insFunc arr' n' res =
      insFunc (drop n' arr') n' ((take n' arr'):res)
  in
    insFunc arr n []

largestArrWithout0 :: Array (Array Int) -> Array Int
largestArrWithout0 arr =
  let
    findArr :: Array (Array Int) -> Array Int -> Int -> Array Int
    findArr [] arr' _ = arr'
    findArr aArr arr' n =
      findArr
        (drop 1 aArr)
        (if length curArr > n then
           curArr
         else
           arr')
        (if length curArr > n then
           length curArr
         else
           n)
      where
        curArr = fromMaybe [] $ head aArr
    filtArr = map (filter (_ /= 0)) arr
  in
   findArr filtArr [] 0
