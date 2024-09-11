{-# LANGUAGE OverloadedStrings #-}

module Data.JsonStream.TokenParser (
    Element(..)
  , TokenResult(..)
) where

import qualified Data.Aeson            as AE
import qualified Data.ByteString.Char8 as BS
import           Foreign.C.Types

data Element = 
    ArrayBegin
  | ArrayEnd !BS.ByteString -- Rest of the source string for correct (ParseDone <rest>)
  | ObjectBegin
  | ObjectEnd !BS.ByteString -- Rest of the source string for correct (ParseDone <rest>)
  | StringContent !BS.ByteString
  | StringRaw !BS.ByteString !Bool !BS.ByteString -- Allow raw strings to go into parser as bytestring/ isAscii
  | StringEnd !BS.ByteString
  | JValue !AE.Value
  | JInteger !CLong
  deriving (Show, Eq)

-- | Public interface for parsing JSON tokens.
data TokenResult =  TokMoreData (BS.ByteString -> TokenResult)
                  | PartialResult Element TokenResult
                  -- ^ found element, continuation, actual parsing view - so that we can report the unparsed
                  -- data when the parsing finishes.
                  | TokFailed

-- For debugging purposes
instance Show TokenResult where
  show (TokMoreData _) = "TokMoreData"
  show TokFailed = "TokFailed"
  show (PartialResult el next) = "(PartialResult (" ++ show el ++ ") " ++ show next ++ ")"
