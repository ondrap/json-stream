{-# LANGUAGE BangPatterns, OverloadedStrings #-}

import Control.Exception
import Control.Monad
import Data.Time.Clock
import System.Environment (getArgs)
import System.IO
import qualified Data.ByteString as B
import Data.JsonStream.Parser
import Data.Aeson.Types (Value(..))
import Control.Applicative (many)
import Data.Word

parseWith :: IO B.ByteString -> Parser a -> B.ByteString -> IO [a]
parseWith refill scheme inp = do
    let pout = runParser' scheme inp
    doparse pout []
    where
      doparse (ParseDone _) acc = return acc
      doparse (ParseFailed err) _ = return []
      doparse (ParseYield v next) acc = doparse next (v:acc)
      doparse (ParseNeedData cont) acc = do
          dta <- refill
          doparse (cont dta) acc

main :: IO ()
main = do
  (bs:cnt:args) <- getArgs
  let count = read cnt :: Int
      blkSize = read bs
  forM_ args $ \arg -> bracket (openFile arg ReadMode) hClose $ \h -> do
    putStrLn $ arg ++ ":"
    start <- getCurrentTime
    let loop !good !bad
            | good+bad >= count = return (good, bad)
            | otherwise = do
          hSeek h AbsoluteSeek 0
          let refill = B.hGet h blkSize
          result <- parseWith refill (many (arrayOf integer) :: Parser [Word32]) =<< refill
          case (result) of
            []  -> loop good (bad+1)
            _   -> loop (good+1) bad
    (good, _) <- loop 0 0
    delta <- flip diffUTCTime start `fmap` getCurrentTime
    putStrLn $ "  " ++ show good ++ " good, " ++ show delta
    let rate = fromIntegral count / realToFrac delta :: Double
    putStrLn $ "  " ++ show (round rate :: Int) ++ " per second"
