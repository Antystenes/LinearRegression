{-# LANGUAGE Strict
           , OverloadedStrings#-}
module Lib where

import qualified Data.HashTable.IO as H
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Numeric.LinearAlgebra
import qualified Data.Vector.Storable as VS
import System.IO
import Data.List
import Control.Monad
import System.Process
import System.Random
import Text.Read (readMaybe)

someFunc :: IO ()
someFunc = putStrLn "someFunc"

nanCheck :: Double -> Double
nanCheck x = if show x == "NaN" || show x == "Infinity" then 0 else x

st :: Double
st = 10

delta :: Double
delta = 1

chars :: T.Text
chars = "qwertyuiopasdfghjklzxcvbnmęóąśłżźćń\t1234567890 "

el :: Char -> T.Text -> Bool
el = T.any . (==)

preprocess :: T.Text -> [T.Text]
preprocess = map (T.take 20) . T.words . T.map check . T.toLower
             where check x = if x `el` chars then x else ' '

type Probs = H.CuckooHashTable T.Text Double

readWords :: Handle -> IO Probs
readWords handl = do
  dict <- H.newSized 2500000
  loop handl dict
  randomIO >>= H.insert dict "<bias>"
  return dict
  where
    loop :: Handle -> Probs -> IO ()
    loop hdl dic =
        hIsEOF hdl >>= \x -> if x
            then return ()
            else do
              line <- preprocess <$> TIO.hGetLine hdl
              let ins word =
                    if (T.length word > 4)
                    then H.lookup dic word >>=
                                    \mprob -> case mprob of
                                       Just _ -> do
                                         return ()
                                       Nothing -> randomIO >>= H.insert dic word
                    else return ()
              case line of
                _:dat -> mapM_ ins dat
                _     -> hPutStrLn stderr "Can't parse line:" >> hPrint stderr line
              loop hdl dic

saveProbs :: Probs -> IO ()
saveProbs dic = do
  H.mapM_ (\(word, v) -> putStrLn . unwords $ [T.unpack . T.strip $ word, show v]) dic

readProbs :: String -> IO Probs
readProbs filename = do
  dict <- H.newSized 2500000
  withFile filename ReadMode $ readFromFile dict
  return dict
  where
    readFromFile :: Probs -> Handle -> IO ()
    readFromFile dic hdl =
      hIsEOF hdl >>= \x -> if x
                              then return ()
                              else do
                                w <- hGetLine hdl
                                case words w of
                                  [p, pr] ->
                                    H.insert dic (T.pack p) (read pr)
                                  _ -> return ()
                                readFromFile dic hdl

linear :: Vector Double -> Vector Double -> Double
linear = (<.>)

rmse :: Vector Double -> Vector Double -> Double -> Double
rmse weights inp ex = let
  prob = linear weights inp
  in (prob - ex)^2

gradRMSE :: Vector Double -> Vector Double -> Double -> Double -> Vector Double
gradRMSE weights inp ex m = VS.map (\x -> x * (linear weights inp - ex)/m) inp

counts :: Eq a => a -> [a] -> Double
counts = ((fromIntegral. length).).filter.(==)

evalLine :: Probs -> [T.Text] -> IO Double
evalLine dict line = do
  let wek@(_:words) = "<bias>" : (filter ((>4). T.length) . nub $ line)
      inp   = VS.cons 1 (foldr (\c acc -> VS.cons (log . (+1) . counts c $ line) acc) VS.empty words)
  weights <- foldM (\vec c -> do
                       mval <-  H.lookup dict c
                       case mval of
                         Just v -> return $ VS.cons v vec
                         Nothing-> return $ VS.cons 0 vec) VS.empty wek
  return ((1900+).(100*).linear weights $ inp)

updateWeights :: Probs -> [T.Text] -> Double -> Double -> Double -> IO Double
updateWeights dict line ex step m = do
  let wek@(_:words) = "<bias>":(filter ((>4).T.length) . nub $ line)
      inp   = VS.cons 1 (foldr (\c acc -> VS.cons (log . (+1) . counts c $ line) acc) VS.empty words)
  weights <- foldM (\vec c -> do
                       mval <- H.lookup dict c
                       case mval of
                         Just v -> return $ VS.snoc vec v
                         Nothing-> return $ VS.snoc vec 0) VS.empty wek
  let grad       = gradRMSE weights inp ex m
      newWeights = weights - (VS.map (nanCheck.(step *)) grad)
      newError   = rmse newWeights inp ex
  foldM_ (\acc x -> do
            let newW = VS.head acc
            unless (newW == 0) $ H.insert dict x newW
            return $ VS.tail acc) newWeights words
  return $ nanCheck newError

checkError :: Probs -> [T.Text] -> Double -> IO Double
checkError dict line ex = do
  let wek@(_:words) = "<bias>":nub line
      inp   = VS.cons 1 . foldr (\c acc -> VS.cons (log . (+1) . counts c $ line) acc) VS.empty $ words
  weights <- foldM (\vec c -> do
                       mval <- H.lookup dict c
                       case mval of
                         Just v -> return $ VS.snoc vec v
                         Nothing-> return $ VS.snoc vec 0) VS.empty wek
  return $ rmse weights inp ex

train :: [(Double,[T.Text])] -> Probs -> Int -> IO ()
train corpus dict maxEp = do
  let len     = length corpus
      testset = take (len`div`10) corpus
      trainset= drop (len`div`10) corpus
      trlen   = length trainset
      testlen = length testset
      trainHelp :: Double -> Int -> Double -> IO ()
      trainHelp err epoch step = do
          let trainOnLine (year, dat) = updateWeights dict dat year step (fromIntegral trlen)
          mapM_ trainOnLine trainset
          newErr <- sqrt <$> foldM (\er (ex, line) -> (+(er/(fromIntegral testlen))) <$> checkError dict line ex) 0 testset
          let
            lossRate   = (err - newErr)
            newSteptmp = if lossRate > 0 then step*2 else step/100
            newStep    = if newSteptmp > 20 then 20 else newSteptmp
          hPutStrLn stderr $ "***********************"
          hPutStrLn stderr $ "  Old Error: " ++ show err
          hPutStrLn stderr $ "  New Error: " ++ show newErr
          hPutStrLn stderr $ "  LossRate: " ++ show lossRate
          unless ((abs lossRate <= delta)||(epoch == maxEp)) $ trainHelp newErr (1+epoch) newStep
  startErr <- sqrt  <$> foldM (\er (ex, line) -> (+(er/fromIntegral testlen)) <$> checkError dict line ex) 0 testset
  trainHelp startErr 1 st

loadCorpus :: String -> IO [(Double,[T.Text])]
loadCorpus filename = do
  withFile filename ReadMode (loop [])
  where loop corp hdl =
          hIsEOF hdl >>= \x -> if x
            then return corp
            else do
              line <- preprocess <$> TIO.hGetLine hdl
              newEntry <- case line of
                cl:dat -> case readMaybe . T.unpack $ cl of
                  Just year -> return $ Just ((year-1900)/100, dat)
                  Nothing   -> hPutStrLn stderr "Can't read as a number:" >> hPrint stderr cl >> return Nothing
                _      -> hPutStrLn stderr "Can't parse line:" >> hPrint stderr line >> return Nothing
              case newEntry of
                Nothing -> loop corp hdl
                Just x  -> loop (x:corp) hdl

eval :: Handle -> Probs -> IO ()
eval hdl dict =
  hIsEOF hdl >>= \x ->
    if x
    then return ()
    else do
      line <- preprocess <$> TIO.hGetLine hdl
      pred <- evalLine dict line
      print pred
      eval hdl dict
