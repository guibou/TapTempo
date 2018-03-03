{-# OPTIONS -Wall #-}
module Bf where

import Text.Megaparsec
import Text.Megaparsec.Char
import Data.Void
import System.Clock
import Data.Char (chr, ord)
import qualified System.IO as IO

import Control.Monad (foldM)
import Data.Word

type Parser = Parsec Void String

data BrainFuck = Add
               | Sub
               | MemoryLeft
               | MemoryRight
               | Put
               | Read
               | Loop [BrainFuck]
               | Comment Char
               | Debug
               deriving (Show)

data Program = Program [BrainFuck]
  deriving (Show)

parseBf :: Parser BrainFuck
parseBf = choice
  [ Add <$ char '+'
  , Sub <$ char '-'
  , MemoryLeft <$ char '<'
  , MemoryRight <$ char '>'
  , Put <$ char '.'
  , Read <$ char ','
  , Loop <$> (char '[' *> many parseBf <* char ']')
  , Debug <$ char '!'
  , Comment <$> noneOf "+-<>.,[]"
  ]

parseProgram :: Parser Program
parseProgram = Program <$> many parseBf

-- Zipper on an infinite memory band
data Memory = Memory [Word16] [Word16]
  deriving (Show)

memoryZero :: Memory
memoryZero = Memory [] []

memoryToRight :: Memory -> Memory
memoryToRight (Memory l r) = case r of
  (x:xs) -> Memory (x:l) xs
  [] -> Memory (0:l) []

memoryToLeft :: Memory -> Memory
memoryToLeft (Memory l r) = case l of
  (x:xs) -> Memory xs (x:r)
  [] -> Memory [] (0:r)

memoryModify :: (Word16 -> Word16) -> Memory -> Memory
memoryModify f (Memory l r) = case l of
  [] -> Memory [f 0] r
  (x:xs) -> Memory (f x:xs) r

memoryGet :: Memory -> Word16
memoryGet (Memory l _) = case l of
  (x:_) -> x
  [] -> 0

-- A machine contains a memory, a hardware timer and the current position of the memory
data Machine = Machine Memory Int (Maybe TimeSpec)
  deriving (Show)

startMachine :: Machine
startMachine = Machine memoryZero 0 Nothing

toLeft :: Machine -> Machine
toLeft (Machine mem idx t) = Machine (memoryToLeft mem) (idx - 1) t

toRight :: Machine -> Machine
toRight (Machine mem idx t) = Machine (memoryToRight mem) (idx + 1) t

modify :: (Word16 -> Word16) -> Machine -> IO (Machine)
modify f (Machine mem (-1) tM) = do
  t' <- getTime Monotonic
  let reg0Val = case tM of
        Nothing -> 0
        Just t -> fromIntegral ((fromIntegral precision * toNanoSecs (diffTimeSpec t t')) `div` (10 ^ (9 :: Integer)))
      -- value is bounded to 1 to avoid divid by zero
  pure (Machine (memoryModify f (memoryToLeft (memoryModify (const (max 1 reg0Val)) (memoryToRight mem)))) (-1) (Just t'))
modify f (Machine mem idx tM) = pure (Machine (memoryModify f mem) idx tM)

get :: Machine -> Word16
get (Machine mem _ _) = memoryGet mem

runBf :: Program -> IO ()
runBf (Program instrs) = do
  IO.hSetBuffering IO.stdin IO.NoBuffering
  IO.hSetBuffering IO.stdout IO.NoBuffering

  () <$ evalInstrs instrs startMachine

evalInstrs :: [BrainFuck] -> Machine -> IO Machine
evalInstrs instrs m = foldM evalInstr m instrs

evalInstr :: Machine -> BrainFuck -> IO Machine
evalInstr m Add = modify (+1) m
evalInstr m Sub = modify (subtract 1) m
evalInstr m MemoryRight = pure (toRight m)
evalInstr m MemoryLeft = pure (toLeft m)
evalInstr m Read = do
  c <- getChar
  modify (const (fromIntegral (ord c))) m
evalInstr m Put = do
  putChar (chr (fromIntegral (get m)))
  pure m
evalInstr m loop@(Loop subInstrs)
  | get m == 0 = pure m
  | otherwise = do
      m' <- evalInstrs subInstrs m
      evalInstr m' loop
evalInstr m (Comment _) = pure m
evalInstr m Debug = print m >> pure m

evalProgram :: String -> IO ()
evalProgram s = case parse parseProgram "" s of
  Left e -> print e
  Right prog -> do
    runBf prog >>= print

precision :: Int
precision = 5

