{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Maybe (mapMaybe)

data BrainfuckCommand = GoRight      -- >
                      | GoLeft       -- <
                      | Increment    -- +
                      | Decrement    -- -
                      | Print        -- .
                      | Read         -- ,
                      | LoopL        -- [
                      | LoopR        -- ]

type BrainfuckSource = [BrainfuckCommand]

data BFSource = BFSource BrainfuckSource

instance Show BFSource where
    show (BFSource x) = map bFToChar x
        where bFToChar GoRight   = '>'
              bFToChar GoLeft    = '<'
              bFToChar Increment = '+'
              bFToChar Decrement = '-'
              bFToChar Print     = '.'
              bFToChar Read      = ','
              bFToChar LoopL     = '['
              bFToChar LoopR     = ']'


checkSyntax :: BrainfuckSource -> Either String BrainfuckSource
checkSyntax xs = checkSyntax' 0 0 0 xs
    where checkSyntax' :: Int -> Int -> Int -> BrainfuckSource -> Either String BrainfuckSource
          checkSyntax' d _ e []
            | d > 0     = Left ("Bracket at pos " ++ show e ++ " is not closed.")
            | otherwise = Right xs 
          checkSyntax' d c _ (LoopL:ys) = checkSyntax' (d+1) (c+1) c ys
          checkSyntax' d c e (LoopR:ys)
            | d > 0     = checkSyntax' (d-1) (c+1) e ys
            | otherwise = Left ("Unexpected ']' at pos " ++ show c)
          checkSyntax' d c e (_:ys) = checkSyntax' d (c+1) e ys


parseBrainfuck :: String -> Either String BrainfuckSource
parseBrainfuck = checkSyntax . mapMaybe charToBF
      where charToBF '>' = Just GoRight
            charToBF '<' = Just GoLeft
            charToBF '+' = Just Increment
            charToBF '-' = Just Decrement
            charToBF '.' = Just Print
            charToBF ',' = Just Read
            charToBF '[' = Just LoopL
            charToBF ']' = Just LoopR
            charToBF  _  = Nothing


data Tape a = Tape [a] a [a]

emptyTape :: Tape Int
emptyTape = Tape zeros 0 zeros
      where zeros = repeat 0

moveRight :: Tape a -> Tape a
moveRight (Tape ls p (r:rs)) = Tape (p:ls) r rs

moveLeft :: Tape a -> Tape a
moveLeft (Tape (l:ls) p rs) = Tape ls l (p:rs)

main :: IO ()
main = putStrLn "Hello World"
