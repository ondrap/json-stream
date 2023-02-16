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
import Data.JsonStream.Parser (integer, object, value)
import qualified Data.Text as T
import Data.Aeson (FromJSON, withObject)
import qualified Data.Aeson as AE

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

data TestObj = TestObj T.Text T.Text Int T.Text T.Text Int

instance FromJSON TestObj where
  parseJSON = withObject "obj" $ \o ->
      TestObj <$> (AE..:) o "guid"
              <*> (AE..:) o "picture"
              <*> (AE..:) o "age"
              <*> (AE..:) o "about"
              <*> (AE..:) o "phone"
              <*> (AE..:) o "index"

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

          let obj1 = TestObj <$> "guid" .: string <*> "picture" .: string <*> "age" .: integer <*> "about" .: string <*> "phone" .: string <*> "index" .: integer :: Parser TestObj
          let obj2 = objectOf $ TestObj <$> "guid" .: string <*> "picture" .: string <*> "age" .: integer <*> "about" .: string <*> "phone" .: string <*> "index" .: integer  :: Parser TestObj
          let obj3 = value :: Parser TestObj

          let parser3 = arrayOf obj3 :: Parser TestObj

          result <- parseWith refill parser3 =<< refill
          case result of
            []  -> loop good (bad+1)
            _   -> loop (good+1) bad
    (good, _) <- loop 0 0
    delta <- flip diffUTCTime start `fmap` getCurrentTime
    putStrLn $ "  " ++ show good ++ " good, " ++ show delta
    let rate = fromIntegral count / realToFrac delta :: Double
    putStrLn $ "  " ++ show (round rate :: Int) ++ " per second"
