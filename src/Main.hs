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


checkSyntax :: BrainfuckSource -> Maybe BrainfuckSource
checkSyntax _ = Nothing

parseBrainfuck :: String -> BrainfuckSource
parseBrainfuck = mapMaybe charToBF
      where charToBF '>' = Just GoRight
            charToBF '<' = Just GoLeft
            charToBF '+' = Just Increment
            charToBF '-' = Just Decrement
            charToBF '.' = Just Print
            charToBF ',' = Just Read
            charToBF '[' = Just LoopL
            charToBF ']' = Just LoopR
            charToBF  _  = Nothing

main :: IO ()
main = putStrLn "Hello World"
