module Main where

import Prelude

import Control.Monad.Writer (Writer, execWriter, runWriter)
import Control.Monad.Writer.Class (tell)
import Data.Array (drop, dropEnd, head, index, last, length, modifyAt, reverse, takeEnd)
import Data.Int (decimal, fromString, toStringAs)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.String (Pattern(..), joinWith, split, trim)
import Data.Tuple (fst)
import Effect (Effect)
import Effect.Console (log)
import Node.Encoding (Encoding(..))
import Node.FS.Sync (readTextFile)

newtype Instruction = Instruction
  { opcode :: Int
  , modes  :: Maybe (Array Int)
  }

derive newtype instance showInstruction :: Show Instruction

main :: Effect Unit
main = do
  text <- readTextFile UTF8 "input.txt"
  log $ "Silver: " <> (show $ silver text)
      <> "\nGold: " <> (show $ gold text)

readNumbers :: String -> Array Int
readNumbers = map (fromMaybe 0 <<< fromString) <<< split (Pattern ",")

getValue :: Int -> Int -> Array Int -> Maybe Int
getValue ind m arr = do
  a <- index arr ind
  case m of
    0 -> index arr a
    1 -> pure a
    _ -> Nothing

modifyArray :: Int -> Array Int -> Int -> Instruction -> Array Int
modifyArray id arr n (Instruction ins) =
  let
    ms = fromMaybe [] ins.modes
    f1 = fromMaybe 0 $ head ms
    f2 = fromMaybe 0 $ head $ drop 1 ms
  in
  case ins.opcode of
    1 -> fromMaybe [f1, f2]
         $ modifyAt
          (fromMaybe (-1) (getValue (n+2) 1 arr))
          (\_ -> fromMaybe (-1)
          $ (+)
          <$> getValue (n) f1 arr
          <*> getValue (n+1) f2 arr)
          arr
    2 -> fromMaybe []
         $ modifyAt
          (fromMaybe (-1) (getValue (n+2) 1 arr))
          (\_ -> fromMaybe (-1)
          $ (*)
          <$> getValue (n) f1 arr
          <*> getValue (n+1) f2 arr)
          arr
    3 -> fromMaybe []
         $ modifyAt
         (fromMaybe (-1) $ index arr n)
         (\_ -> id)
         arr
    7 -> fromMaybe []
         $ modifyAt
         (fromMaybe (-1) $ index arr (n+2))
         (\_ -> if (fromMaybe 0 $ getValue n f1 arr) < (fromMaybe 0 $ getValue (n+1) f2 arr) then
                  1
                else
                  0)
         arr
    8 -> fromMaybe []
         $ modifyAt
         (fromMaybe (-1) $ index arr (n+2))
         (\_ -> if (fromMaybe 0 $ getValue n f1 arr) == (fromMaybe 0 $ getValue (n+1) f2 arr) then
                  1
                else
                  0)
         arr
    _ -> []



doComputations :: Int -> Array Int -> Int -> Maybe Instruction -> Writer (Array Int) (Array Int)
doComputations id arr n Nothing = doComputations id arr n (instruction <$> head arr)
doComputations id arr n (Just (Instruction {opcode: 4})) = do
  tell [fromMaybe 0 (getValue (n+1) 0 arr)]
  doComputations id arr (n+2) nextIns
    where
      nextIns = instruction
                <$> index arr (n+2)
doComputations id arr n (Just (Instruction ins)) = do
  if n >= length arr then
    pure arr
  else
    case ins.opcode of
    1 -> doComputations id (modifyArray id arr (n+1) (Instruction ins)) (n+4) (nextIns (n+4))
    2 -> doComputations id (modifyArray id arr (n+1) (Instruction ins)) (n+4) (nextIns (n+4))
    3 -> doComputations id (modifyArray id arr (n+1) (Instruction ins)) (n+2) (nextIns (n+2))
    5 -> doComputations id arr
                        (jumpOp arr (n+1) (fromMaybe [] $ ins.modes) true)
                        (nextIns (jumpOp arr (n+1) (fromMaybe [] $ ins.modes) true))
    6 -> doComputations id arr
                        (jumpOp arr (n+1) (fromMaybe [] $ ins.modes) false)
                        (nextIns (jumpOp arr (n+1) (fromMaybe [] $ ins.modes) false))
    7 -> doComputations id (modifyArray id arr (n+1) (Instruction ins)) (n+4) (nextIns (n+4))
    8 -> doComputations id (modifyArray id arr (n+1) (Instruction ins)) (n+4) (nextIns (n+4))
    _ -> pure arr
  where
    nextIns :: Int -> Maybe Instruction
    nextIns n' = instruction
                <$> index arr n'

jumpOp :: Array Int -> Int -> Array Int -> Boolean -> Int
jumpOp arr n m i =
  let
    f1 = fromMaybe 0 $ index m 0
    f2 = fromMaybe 0 $ index m 1
    p1 = fromMaybe 0 $ getValue n f1 arr
    p2 = fromMaybe 0 $ getValue (n+1) f2 arr
  in
    if i then
      if p1 /= 0 then
        p2
      else
        (n+2)
    else
      if p1 == 0 then
        p2
      else
        (n+2)


silver :: String -> Int
silver str =
  let
    nums = readNumbers $ trim str
    farr = fst $ runWriter $ doComputations 1 nums 0 Nothing
    sarr = execWriter $ doComputations 1 farr 0 Nothing
    res = fromMaybe 0 $ last sarr
  in
    res

gold :: String -> Int
gold str =
  let
    nums = readNumbers $ trim str
    farr = fst $ runWriter $ doComputations 5 nums 0 Nothing
    sarr = execWriter $ doComputations 5 farr 0 Nothing
    res = fromMaybe 0 $ last sarr
  in
    res

opcode :: Int -> Maybe Int
opcode ins = do
    n <- fromString
         $ joinWith ""
         $ takeEnd 2
         $ split (Pattern "")
         $ toStringAs decimal ins
    pure n

rest :: Int -> Maybe(Array Int)
rest x =
  let
    n = fromString
        $ joinWith ""
        $ dropEnd 2
        $ split (Pattern "")
        $ toStringAs decimal x
    restIn :: Int -> Array Int
    restIn num =
      let
        arr = split (Pattern "")
              $ toStringAs decimal num
      in
        reverse
        $ map (\v -> fromMaybe 2 $ fromString v) arr
  in
   if n == Nothing then
     Nothing
   else
     Just
     $ restIn
     $ fromMaybe 99 n

insert0Until :: Int -> Array Int -> Array Int
insert0Until x arr =
  if length arr >= x then
    arr
  else
    insert0Until x (arr <> [0])

instruction :: Int -> Instruction
instruction n =
  Instruction { opcode: opc
              , modes:  mod
              }
  where
    opc = fromMaybe 100
          $ opcode n
    mod = if n == 2 || n == 1 || n == 7 || n == 8 then
            Just [0,0,0]
          else if n == 3 || n == 4 then
            Just [0]
          else if n == 5 || n == 6 then
            Just [0,0]
          else case opc of
                 1 -> insert0Until 3
                 2 -> insert0Until 3
                 3 -> insert0Until 1
                 4 -> insert0Until 1
                 5 -> insert0Until 2
                 6 -> insert0Until 2
                 7 -> insert0Until 3
                 8 -> insert0Until 3
                 _ -> insert0Until 0
               <$> rest n
