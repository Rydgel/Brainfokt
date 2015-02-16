module Main where

import           Data.Maybe (mapMaybe)
import qualified Data.Stream as S
import           Data.Stream (Stream(..), (<:>))
import           Data.Char (chr, ord)
import           System.IO (hFlush, stdout)

data BrainfuckCommand = GoRight      -- >
                      | GoLeft       -- <
                      | Increment    -- +
                      | Decrement    -- -
                      | Print        -- .
                      | Read         -- ,
                      | LoopL        -- [
                      | LoopR        -- ]
                      | Comment      -- everything else
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


data Tape a = Tape (Stream a) a (Stream a)

instance Functor Tape where
  fmap f (Tape xs y zs) = Tape (fmap f xs) (f y) (fmap f zs)

emptyTape :: Tape Int
emptyTape = Tape zeros 0 zeros
      where zeros = S.repeat 0

emptyStream :: Stream BrainfuckCommand
emptyStream = S.repeat Eof

moveRight :: Tape a -> Tape a
moveRight (Tape ls p (Cons r rs)) = Tape (p <:> ls) r rs

moveLeft :: Tape a -> Tape a
moveLeft (Tape (Cons l ls) p rs) = Tape ls l (p <:> rs)

runBrainfuck :: BrainfuckSource -> IO ()
runBrainfuck = run emptyTape . bfSource2Tape
      where bfSource2Tape (b:bs) = Tape emptyStream b (S.fromList (bs ++ (repeat Eof)))
            bfSource2Tape []     = Tape emptyStream Eof emptyStream

advance :: Tape Int -> Tape BrainfuckCommand -> IO ()
advance _ (Tape _ _ (Cons Eof _)) = return ()
advance dataTape source = run dataTape (moveRight source)

run :: Tape Int -> Tape BrainfuckCommand -> IO ()
run dataTape source@(Tape _ GoRight _) = advance (moveRight dataTape) source
run dataTape source@(Tape _ GoLeft  _) = advance (moveLeft dataTape) source
run (Tape l p r) source@(Tape _ Increment  _) = advance (Tape l (p+1) r) source
run (Tape l p r) source@(Tape _ Decrement  _) = advance (Tape l (p-1) r) source
run dataTape@(Tape _ p _) source@(Tape _ Print  _) = do
      putChar (chr p)
      hFlush stdout
      advance dataTape source
run (Tape l _ r) source@(Tape _ Read  _) = do
      p <- getChar
      advance (Tape l (ord p) r) source
run dataTape@(Tape _ p _) source@(Tape _ LoopL  _)
      | p == 0 = seekLoopR 0 dataTape source
      | otherwise = advance dataTape source
run dataTape@(Tape _ p _) source@(Tape _ LoopR  _)
      | p /= 0 = seekLoopL 0 dataTape source
      | otherwise = advance dataTape source
run dataTape source@(Tape _ Comment  _) = advance dataTape source
run dataTape source@(Tape _ Eof  _) = advance dataTape source


seekLoopR :: Int -> Tape Int -> Tape BrainfuckCommand -> IO ()
seekLoopR 1 dataTape source@(Tape _ LoopR _) = advance dataTape source
seekLoopR b dataTape source@(Tape _ LoopR _) = seekLoopR (b-1) dataTape (moveRight source)
seekLoopR b dataTape source@(Tape _ LoopL _) = seekLoopR (b+1) dataTape (moveRight source)
seekLoopR b dataTape source                  = seekLoopR b dataTape (moveRight source)

seekLoopL :: Int -> Tape Int -> Tape BrainfuckCommand -> IO ()
seekLoopL 1 dataTape source@(Tape _ LoopL _) = advance dataTape source
seekLoopL b dataTape source@(Tape _ LoopL _) = seekLoopL (b-1) dataTape (moveLeft source)
seekLoopL b dataTape source@(Tape _ LoopR _) = seekLoopL (b+1) dataTape (moveLeft source)
seekLoopL b dataTape source                  = seekLoopL b dataTape (moveLeft source)

main :: IO ()
main = readFile "helloworld.bf" >>= runBrainfuck . parse
  where parse x = case parseBrainfuck x of
                    (Left s) -> error s
                    (Right bs) -> bs 
