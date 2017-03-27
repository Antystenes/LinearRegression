module Main where

import Lib
import System.IO
import System.Environment
import Text.Read (readMaybe)

main :: IO ()
main = do
  args <- getArgs
  case args of
    [arg] -> do
      dict <- withFile arg ReadMode readWords
      corp <- loadCorpus arg
      train corp dict 0
      saveProbs dict
    [arg1, arg2] -> case (readMaybe arg2 :: Maybe Int) of
      Nothing -> do
        dict <- readProbs arg1
        corp <- loadCorpus arg2
        train corp dict 0
        saveProbs dict
      Just num -> do
        dict <- withFile arg1 ReadMode readWords
        corp <- loadCorpus arg1
        train corp dict num
        saveProbs dict
    [arg1, arg2, arg3] -> do
      dict <- readProbs arg1
      corp <- loadCorpus arg2
      train corp dict . read $ arg3
      saveProbs dict
