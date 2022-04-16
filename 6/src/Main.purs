module Main where

import Prelude

import Control.Comonad.Cofree ((:<))
import Control.Monad.State (State, execState, modify)
import Data.Array (head, last)
import Data.Foldable (traverse_)
import Data.List (List(..), (:))
import Data.List (length, singleton) as Li
import Data.Map (Map)
import Data.Map (empty, filterKeys, insertWith) as Map
import Data.Maybe (Maybe(..), fromMaybe)
import Data.String (Pattern(..), split, trim)
import Data.Tree (Tree, showTree)
import Data.Tree.Zipper (findDown, fromTree, insertChild, parents, root, toTree)
import Effect (Effect)
import Effect.Console (log)
import Node.Encoding (Encoding(..))
import Node.FS.Sync (readTextFile)

main :: Effect Unit
main = do
  text <- readTextFile UTF8 "input.txt"
  log $ (show $ createMap text)

showTreeMaybe :: Maybe (Tree String) -> String
showTreeMaybe (Just x) = showTree x
showTreeMaybe Nothing = "Nothing"

sampleTree :: Tree Int
sampleTree =
  1 :<
      (2 :< Nil)
    : (3 :< Nil)
    : (4 :<
          (5 :< Nil)
        : (6 :<
            (7 :< Nil) : Nil)
        : (8 :< Nil)
        : Nil
      )
    : Nil

mySampleTree :: Tree String
mySampleTree =
    "COM" :<
      ("B" :<
              ("G" :<
                    ("H" :< Nil)
                    : Nil)
            : ("C" :<
                    ("D" :<
                            ("I" :< Nil)
                          : ("E" :<
                                  ("F" :< Nil)
                                : Nil)
                          : Nil)
                  : Nil)
            : Nil
      ) : Nil


modifyTree :: Tree String -> String -> String -> Maybe (Tree String)
modifyTree t f i = do
  nd <- findDown f $ fromTree t
  let ins = insertChild (i :< Nil) nd
  pure $ toTree $ root ins

myModifiedTree :: Maybe (Tree String)
myModifiedTree = do
  f1 <- modifyTree mySampleTree "E" "J"
  f2 <- modifyTree f1 "J" "K"
  f3 <- modifyTree f2 "K" "L"
  pure f3

checkParents :: String -> Maybe Int
checkParents str = do
  let x = mySampleTree
  nd <- findDown str $ fromTree x
  pure $ Li.length $ parents nd

createMap :: String -> Map String (List String)
createMap str =
  let
    ins = split (Pattern "\n") $ trim str
    tup = map (\v -> split(Pattern ")") v) ins
    crtMap :: Array(Array String) -> State (Map String (List String)) Unit
    crtMap = traverse_ \n -> modify \sum -> Map.insertWith
                                             (\e _ -> (fromMaybe "" $ last n):e)
                                             (fromMaybe " " $ head n)
                                             (Li.singleton $ fromMaybe " " $ last n)
                                             sum
  in
    execState (crtMap tup) Map.empty
