module Main where

import Lib
import System.Environment
import System.IO

main :: IO ()
main = do
  arg:_ <- getArgs
  readProbs arg >>= eval stdin
