module Main where

import Parser
import Evaluator
import System.Environment (getArgs)


parse :: String -> BrainfuckSource
parse x = case parseBrainfuck x of
            (Left s) -> error s
            (Right bs) -> bs

main :: IO ()
main = do
    argv <- getArgs
    case argv of
        (file:_) -> readFile file >>= runBrainfuck . parse
        _        -> error "Bad input, it should be: brainfokt <filename.bf>"

