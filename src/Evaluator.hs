module Evaluator where

import           Parser
import           Tape
import qualified Data.Stream as S
import           Data.Stream (Stream(..))
import           Data.Char (chr, ord)
import           System.IO (hFlush, stdout)


runBrainfuck :: BrainfuckSource -> IO ()
runBrainfuck = run emptyTape . bfSource2Tape
      where bfSource2Tape (b:bs) = Tape emptyStream b (S.fromList (bs ++ repeat Eof))
            bfSource2Tape []     = Tape emptyStream Eof emptyStream

advance :: Tape Int -> Tape BrainfuckCommand -> IO ()
advance _ (Tape _ _ (Cons Eof _)) = return ()
advance dataTape source           = run dataTape (moveRight source)

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

