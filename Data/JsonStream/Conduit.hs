-- |
-- Module : Data.JsonStream.Conduit
-- License     : BSD-style
--
-- Stability   : experimental
-- Portability : portable
--
-- Use "Data.JsonStream.Parser" parsers in "Data.Conduit".

module Data.JsonStream.Conduit (
  parserConduit
) where

import           Data.ByteString (ByteString)
import qualified Data.Conduit.Internal as C

import           Data.JsonStream.Parser

-- |Use a 'Parser' as a conduit from 'ByteString' input chunks to results, finally returning any parse error or 'Nothing' on success.
parserConduit :: Parser a -> C.ConduitT ByteString a m (Maybe String)
parserConduit ps = C.ConduitT (parsePipe $ runParser ps) where
  parsePipe (ParseYield a p) r = C.HaveOutput (parsePipe p r) a
  parsePipe (ParseNeedData f) r = C.NeedInput (\i -> parsePipe (f i) r) (\() -> r (Just "Incomplete JSON"))
  parsePipe (ParseFailed e) r = r (Just e)
  parsePipe (ParseDone l) r = C.Leftover (r Nothing) l
