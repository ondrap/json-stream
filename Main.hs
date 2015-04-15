{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections     #-}

import           Control.Applicative
import qualified Data.ByteString.Char8    as BS
import qualified Data.Text                as T

import Data.JsonStream.Parser
import Data.JsonStream.TokenParser

execIt :: Show a => [BS.ByteString] -> Parser a -> IO ()
execIt input parser = loop input $ runParser parser
  where
    loop [] (ParseNeedData _) = putStrLn "Out of data - "
    loop _ (ParseFailed err) = putStrLn $ "Failed: " ++ err
    loop _ (ParseDone bl) = putStrLn $ "Done: " ++ show bl
    loop (dta:rest) (ParseNeedData np) = loop rest $ np dta
    loop dta (ParseYield item np) = do
        putStrLn $ "Got: " ++ show item
        loop dta np

testParser = toList $ array value
-- testParser = (,) <$> array value <*> array value

main :: IO ()
main = do
  let test = ["[1,{\"ondra\":null},3,4,5,6,7] \"sdjfsdjfl\\n"]
  execIt test testParser
  return ()
