module Main where

import           Data.Maybe (mapMaybe)
import qualified Data.Stream as S
import           Data.Stream (Stream(..))

data BrainfuckCommand = GoRight      -- >
                      | GoLeft       -- <
                      | Increment    -- +
                      | Decrement    -- -
                      | Print        -- .
                      | Read         -- ,
                      | LoopL        -- [
                      | LoopR        -- ]
                      | Comment      -- everything else

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


data Tape a = Tape (Stream a) a (Stream a)

instance Functor Tape where
  fmap f (Tape xs y zs) = Tape (fmap f xs) (f y) (fmap f zs)

emptyTape :: Tape Int
emptyTape = Tape zeros 0 zeros
      where zeros = S.repeat 0

moveRight :: Tape a -> Tape a
moveRight (Tape ls p (Cons r rs)) = Tape (Cons p ls) r rs

moveLeft :: Tape a -> Tape a
moveLeft (Tape (Cons l ls) p rs) = Tape ls l (Cons p rs)

main :: IO ()
main = putStrLn "Hello World"
