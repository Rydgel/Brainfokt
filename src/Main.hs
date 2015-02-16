module Main where

import           Parser
import           Evaluator
import           System.Environment (getArgs)


main :: IO ()
main = do
    argv <- getArgs
    case argv of
        (file:_) -> readFile file >>= runBrainfuck . parse
                        where parse x = case parseBrainfuck x of
                                            (Left s) -> error s
                                            (Right bs) -> bs
        _        -> error "Bad input, it should be: brainfokt <filename.bf>"

