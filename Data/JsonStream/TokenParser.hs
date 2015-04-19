{-# LANGUAGE OverloadedStrings #-}

module Data.JsonStream.TokenParser (
    Element(..)
  , TokenResult(..)
) where

import qualified Data.Aeson            as AE
import qualified Data.ByteString.Char8 as BS

data Element = ArrayBegin | ArrayEnd BS.ByteString | ObjectBegin | ObjectEnd BS.ByteString
               | StringContent BS.ByteString | StringEnd
               | JValue AE.Value | JInteger Int
               deriving (Show, Eq)

-- | Public interface for parsing JSON tokens.
data TokenResult =  TokMoreData (BS.ByteString -> TokenResult)
                  | PartialResult Element (TokenResult)
                  -- ^ found element, continuation, actual parsing view - so that we can report the unparsed
                  -- data when the parsing finishes.
                  | TokFailed

-- For debugging purposes
instance Show TokenResult where
  show (TokMoreData _) = "TokMoreData"
  show TokFailed = "TokFailed"
  show (PartialResult el _) = "(PartialResult' " ++ show el ++ ")"
