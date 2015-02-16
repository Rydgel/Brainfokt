module Parser where

import Data.Maybe (mapMaybe)


data BrainfuckCommand = GoRight      -- >
                      | GoLeft       -- <
                      | Increment    -- +
                      | Decrement    -- -
                      | Print        -- .
                      | Read         -- ,
                      | LoopL        -- [
                      | LoopR        -- ]
                      | Comment      -- Everything else
                      | Eof          -- End of file

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
              bFToChar _         = ' '


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

