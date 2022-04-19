module Main where

import Prelude

import Data.Array (drop, filter, head, length, snoc, take)
import Data.Int (decimal, fromString, toStringAs)
import Data.Maybe (fromMaybe)
import Data.String (Pattern(..), Replacement(..), joinWith, replaceAll, split, trim)
import Effect (Effect)
import Effect.Console (log)
import Node.Encoding (Encoding(..))
import Node.FS.Sync (readTextFile)

main :: Effect Unit
main = do
  text <- readTextFile UTF8 "input.txt"
  log $ "Silver: " <> (show $ silver text)
      <> "\nGold:\n" <> gold text

readNumbers :: String -> Array Int
readNumbers = map (fromMaybe 0 <<< fromString) <<< split (Pattern "") <<< trim

silver :: String -> Int
silver str =
  (length oneArr) * (length twoArr)
  where
    arr = groupArray (readNumbers str) (6*25)
    fArr = largestArrWithout0 arr
    oneArr = filter (_ == 1) fArr
    twoArr = filter (_ == 2) fArr

gold :: String -> String
gold str =
  let
    res = replaceAll (Pattern "1") (Replacement "█")
        $ replaceAll (Pattern "0") (Replacement " ")
        $ joinWith "\n"
        $ map (joinWith "")
        $ map (map (toStringAs decimal))
        $ groupArray
        ( map (\n -> fromMaybe 2 $ head n)
        $ map (filter (_ /= 2))
        $ extractPixels
        $ groupArray (readNumbers str) (6*25)
        )
        25
  in
    res

groupArray :: forall a. Array a -> Int -> Array(Array a)
groupArray arr n =
  insFunc arr n []
  where
    insFunc :: forall a. Array a -> Int -> Array(Array a) -> Array(Array a)
    insFunc [] _ res = res
    insFunc arr' n' res =
      insFunc (drop n' arr') n' (snoc res (take n' arr'))

largestArrWithout0 :: Array (Array Int) -> Array Int
largestArrWithout0 arr =
  findArr filtArr [] 0
  where
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

extractPixels :: Array (Array Int) -> Array (Array Int)
extractPixels arr =
  exPixels arr []
  where
    exPixels :: Array (Array Int) -> Array (Array Int) -> Array (Array Int)
    exPixels [] nArr = nArr
    exPixels oArr nArr =
      exPixels nOArr nNArr
      where
        tmpOArr = map (drop 1) oArr
        nOArr = filter (\n -> length n /= 0) tmpOArr
        nNArr = snoc nArr (map (\n -> fromMaybe 0 $ head n) oArr)
