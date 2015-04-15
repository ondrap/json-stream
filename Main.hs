{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections     #-}

import           Control.Applicative
import qualified Data.ByteString.Char8    as BS
import qualified Data.Text                as T

-- import Data.JsonStream.Parser
import Data.JsonStream.TokenParser

-- execIt :: Show a => [BS.ByteString] -> Parser a -> IO ()
-- execIt input parser = loop input $ runParser parser
--   where
--     loop [] (ParseNeedData _) = putStrLn "Out of data - "
--     loop _ (ParseFailed err) = putStrLn $ "Failed: " ++ err
--     loop _ (ParseDone bl) = putStrLn $ "Done: " ++ show bl
--     loop (dta:rest) (ParseNeedData np) = loop rest $ np dta
--     loop dta (ParseYield item np) = do
--         putStrLn $ "Got: " ++ show item
--         loop dta np
--
-- testParser = (,) <$> array value <*> array value
-- testParser = (,) <$> array value <*> array value

tokenTest :: [BS.ByteString] -> [TokenResult]
tokenTest chunks = reverse $ test [] (tail chunks) (tokenParser $ head chunks)
  where
    test acc [] o@(TokMoreData {}) = o:acc
    test acc (dta:rest) o@(TokMoreData ntok _) = test (o:acc) rest (ntok dta)
    test acc lst o@(TokFailed {}) = o:acc
    test acc lst o@(PartialResult _ ntok _) = test (o:acc) lst ntok


main :: IO ()
main = do
  let test = " [ true, false, null   ,  \"abcd\" ]    "
  -- let test = "[1,2,false,true,null,6,7]"
  print $ tokenTest [test]

  -- let x = parseByteString (array value) test
  -- print x

  return ()
