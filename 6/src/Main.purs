module Main where

import Prelude

import Control.Comonad.Cofree ((:<))
import Control.Monad.State (State, execState, modify)
import Data.Array (head, last)
import Data.Foldable (foldl, traverse_)
import Data.List (List(..), (:))
import Data.List (length, singleton) as Li
import Data.Map (Map)
import Data.Map (empty, insertWith, lookup) as Map
import Data.Maybe (Maybe(..), fromMaybe, isNothing)
import Data.String (Pattern(..), split, trim)
import Data.Tree (Tree, showTree)
import Data.Tree.Zipper (Loc, findDown, fromTree, insertChild, parents, root, toTree, up)
import Effect (Effect)
import Effect.Console (log)
import Node.Encoding (Encoding(..))
import Node.FS.Sync (readTextFile)

main :: Effect Unit
main = do
  text <- readTextFile UTF8 "input.txt"
  log $ "Silver: " <> (show $ silver text)
      <> "\nGold: " <> (show $ gold text)

showTreeMaybe :: Maybe (Tree String) -> String
showTreeMaybe (Just x) = showTree x
showTreeMaybe Nothing = "Nothing"

modifyTree :: Tree String -> String -> String -> Maybe (Tree String)
modifyTree t f i = do
  nd <- findDown f $ fromTree t
  let ins = insertChild (i :< Nil) nd
  pure $ toTree $ root ins

modifyTreeList :: Maybe (Tree String) -> String -> List String -> Maybe (Tree String)
modifyTreeList t _ Nil = t
modifyTreeList (Just t) f (x:xs) = modifyTreeList (modifyTree t f x) f xs
modifyTreeList Nothing _ _ = Nothing

getTreeLength :: Tree String -> String -> Maybe Int
getTreeLength t str = do
  nd <- findDown str $ fromTree t
  pure $ Li.length $ parents nd

createMap :: String -> Map String (List String)
createMap str =
  execState (crtMap tup) Map.empty
  where
    ins = split (Pattern "\n") $ trim str
    tup = map (\v -> split(Pattern ")") v) ins
    crtMap :: Array(Array String) -> State (Map String (List String)) Unit
    crtMap = traverse_ \n -> modify \sum -> Map.insertWith
                                             (\e _ -> (fromMaybe "" $ last n):e)
                                             (fromMaybe " " $ head n)
                                             (Li.singleton $ fromMaybe " " $ last n)
                                             sum

createTreeMaybe :: Map String (List String) -> String -> Maybe (Tree String)
createTreeMaybe mp str = do
  let t = str :< Nil
  crtTreeMaybe (Just t) (Li.singleton str)
  where
    crtTreeMaybe :: Maybe (Tree String) -> List String ->  Maybe (Tree String)
    crtTreeMaybe t Nil = t
    crtTreeMaybe t (x:xs) =
      crtTreeMaybe nnt xs
      where
        lu = fromMaybe Nil $ Map.lookup x mp
        nt = modifyTreeList t x lu
        nnt = crtTreeMaybe nt lu

sumTree :: Tree String -> Int
sumTree t =
  foldl (\sum el -> sum + (Li.length $ parents $ fromMaybe (fromTree ("error" :< Nil)) $ findDown el lo)) 0 t
  where
    lo = fromTree t

silver :: String -> Int
silver text = sumTree (fromMaybe ("1" :< Nil) $ createTreeMaybe (createMap text) "COM")

gold :: String -> Int
gold text =
  let
    t = createTreeMaybe (createMap text) "COM"
    lo = fromTree (fromMaybe ("error" :< Nil) t)
    findDis :: Loc String -> String -> Int -> Maybe Int
    findDis l str sum = if isNothing (findDown str l) then
                          if isNothing (up l) then
                            Nothing
                          else
                            findDis (fromMaybe (fromTree ("error" :< Nil)) (up l)) str (sum+1)
                        else
                          (+) <$> ((-) <$> (Li.length <$> parents <$> findDown str l) <*> (Just (Li.length $ parents $ l))) <*> (Just sum)

  in
    if isNothing (findDown "YOU" lo) || isNothing (findDown "SAN" lo) then
      0
    else
      fromMaybe 0 $ (-) <$> (findDis (fromMaybe (fromTree ("error" :< Nil)) (findDown "YOU" lo)) "SAN" 0) <*> (Just 2)
