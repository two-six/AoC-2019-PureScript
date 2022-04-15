module Main where

import Prelude

import Control.Monad.Writer (Writer, execWriter)
import Control.Monad.Writer.Class (tell)
import Data.Array (drop, dropEnd, head, index, length, modifyAt, reverse, takeEnd)
import Data.Int (decimal, fromString, toStringAs)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.String (Pattern(..), joinWith, split)
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
  -- log $ "Silver: " <> (show $ silver text)
  --     <> "\nGold: " <> (show $ gold text)
  log (show $ silver text)

readNumbers :: String -> Array Int
readNumbers = map (fromMaybe 0 <<< fromString) <<< split (Pattern ",")

runIns :: Array Int -> Int -> Array Int
runIns arr n = drop n arr

getValue :: Int -> Int -> Array Int -> Maybe Int
getValue ind m arr = do
  a <- index arr ind
  x <- index arr a
  case m of
    0 -> pure x
    1 -> pure a
    _ -> Nothing

modifyArray :: Array Int -> Int -> Instruction -> Array Int
modifyArray arr n (Instruction ins) =
  let
    ms = fromMaybe [] (ins.modes)
    f1 = fromMaybe 2 $ head ms
    f2 = fromMaybe 2 $ head $ drop 1 ms
    f3 = fromMaybe 2 $ head $ drop 2 ms
  in
  case ins.opcode of
    1 -> fromMaybe []
         $ modifyAt
          (fromMaybe (-1) (getValue (n+2) f3 arr))
          (\_ -> fromMaybe (-1)
          $ (+)
          <$> getValue (n) f1 arr
          <*> getValue (n+1) f2 arr)
          arr
    2 -> fromMaybe []
         $ modifyAt
          (fromMaybe (-1) (getValue (n+2) f3 arr))
          (\_ -> fromMaybe (-1)
          $ (*)
          <$> getValue (n) f1 arr
          <*> getValue (n+1) f2 arr)
          arr
    3 -> fromMaybe []
         $ modifyAt
         (fromMaybe (-1) (getValue n 0 arr))
         (\_ -> 1)
         arr
    _ -> []


doComputations :: Array Int -> Int -> Maybe Instruction -> Writer (Array Int) (Array Int)
doComputations arr n Nothing = doComputations arr n (instruction <$> head arr)
doComputations arr n (Just (Instruction {opcode: 4})) = do
  tell [fromMaybe 0 (getValue (n+1) 0 arr)]
  doComputations arr (n+2) nextIns
    where
      nextIns = instruction
                <$> index arr (n+2)
doComputations arr n (Just (Instruction ins)) = do
  case ins.opcode of
    1 -> doComputations (modifyArray arr (n+1) (Instruction ins)) (n+4) (nextIns (n+4))
    2 -> doComputations (modifyArray arr (n+1) (Instruction ins)) (n+4) (nextIns (n+4))
    3 -> doComputations (modifyArray arr (n+1) (Instruction ins)) (n+2) (nextIns (n+2))
    _ -> pure arr
  where
    nextIns :: Int -> Maybe Instruction
    nextIns n' = instruction
                <$> index arr n'


silver :: String -> Array Int
silver str =
  let
    nums = readNumbers str
    res = execWriter $ doComputations nums 0 Nothing
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
    mod = if n == 2 || n == 1 then
            Just [0,0,0]
          else if n == 3 || n == 4 then
            Just [0]
          else case opc of
                 1 -> insert0Until 3
                 2 -> insert0Until 3
                 3 -> insert0Until 1
                 4 -> insert0Until 1
                 _ -> insert0Until 0
               <$> rest n