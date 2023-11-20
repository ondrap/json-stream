{-# LANGUAGE BangPatterns, OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

import Control.Exception
import Control.Monad
import Data.Aeson
import Data.Time.Clock
import System.Environment (getArgs)
import System.IO
import qualified Data.ByteString as B
import Control.DeepSeq (deepseq)

main :: IO ()
main = do
  (bs:cnt:args) <- getArgs
  let count = read cnt :: Int
  forM_ args $ \arg -> bracket (openFile arg ReadMode) hClose $ \h -> do
    putStrLn $ arg ++ ":"
    start <- getCurrentTime
    let loop !good !bad
            | good+bad >= count = return (good, bad)
            | otherwise = do
          hSeek h AbsoluteSeek 0
          content <- B.hGet h 4200000
          let result = decodeStrict' content
          case result `deepseq` result of
            Just (_ :: Value)  -> loop (good+1) bad
            Nothing -> loop good (bad+1)
    (good, _) <- loop 0 0
    delta <- flip diffUTCTime start `fmap` getCurrentTime
    putStrLn $ "  " ++ show good ++ " good, " ++ show delta
    let rate = fromIntegral count / realToFrac delta :: Double
    putStrLn $ "  " ++ show (round rate :: Int) ++ " per second"
