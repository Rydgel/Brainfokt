module Tape where

import           Parser
import qualified Data.Stream as S
import           Data.Stream (Stream(..), (<:>))


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

