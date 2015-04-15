module Data.JsonStream.IO (
  parseIOInput
) where

import Control.Monad (when)
import Control.Exception (mask_)
import Control.Concurrent.MVar
import qualified Data.ByteString as BS

import Data.JsonStream.Parser

-- | Parse input using IO monad or transformers on IO.
-- Return Left on end of input, Right on value, error on parse error.
-- MVar for synchronizing the state, it should be thread-safe.
parseIOInput :: Parser a -> IO BS.ByteString -> IO (IO (Either BS.ByteString a))
parseIOInput parser newdata = do
  nextstate <- newMVar (runParser parser)
  return $ modifyMVar nextstate generate
  where
    generate (ParseYield v np) = return (np, Right v)
    generate (ParseNeedData next) = mask_ $ do -- Mask, so that we either consume IO and change MVar or not
      dta <- newdata
      when (BS.null dta) $ error "Not enough data."
      generate (next dta)

    generate p@(ParseDone rest) = return (p, Left rest)
    generate (ParseFailed err) = error err
