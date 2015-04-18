{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}

import Foreign
import Foreign.C.Types
import Data.ByteString.Unsafe (unsafeUseAsCString)

import CLexType
import Foreign.Marshal.Alloc
import qualified Data.ByteString as BS

data Header = Header {
    hdrCurrentState :: !CInt
  , hdrStateData :: !CInt
  , hdrStateSata2 :: !CInt

  , hdrPosition :: !CInt
  , hdrLength :: !CInt
  , hdrResultNum :: !CInt
} deriving (Show)

instance Storable Header where
  sizeOf _ = 8 * sizeOf (undefined :: CInt)
  alignment _ = sizeOf (undefined :: CInt)
  peek ptr = do
    state <- peekByteOff ptr 0
    sdata1 <- peekByteOff ptr (sizeOf state)
    sdata2 <- peekByteOff ptr (2 * sizeOf state)
    position <- peekByteOff ptr (3 * sizeOf state)
    slength <- peekByteOff ptr (4 * sizeOf state)
    sresultnum <- peekByteOff ptr (5 * sizeOf state)
    return $ Header state sdata1  sdata2  position  slength sresultnum
    -- return $ Header state sdata1 sdata2 position slength sresultnum

  poke ptr (Header {..}) = do
    pokeByteOff ptr 0 hdrCurrentState
    pokeByteOff ptr (1 * sizeOf hdrCurrentState) hdrStateData
    pokeByteOff ptr (2 * sizeOf hdrCurrentState) hdrStateSata2
    pokeByteOff ptr (3 * sizeOf hdrCurrentState) hdrPosition
    pokeByteOff ptr (4 * sizeOf hdrCurrentState) hdrLength
    pokeByteOff ptr (5 * sizeOf hdrCurrentState) hdrResultNum

data Result = Result {
    resType :: CInt
  , resStartPos :: CInt
  , resLength :: CInt
  , resAddData :: CInt
} deriving (Show)

instance Storable Result where
  sizeOf _ = 4 * sizeOf (undefined :: CInt)
  alignment _ = sizeOf (undefined :: CInt)
  peek ptr = do
    rtype <- peekByteOff ptr 0
    rpos <- peekByteOff ptr (sizeOf rtype)
    rlen <- peekByteOff ptr (2 * sizeOf rtype)
    rdata <- peekByteOff ptr (3 * sizeOf rtype)
    return $ Result rtype rpos rlen rdata
  poke ptr (Result {..}) = do
    pokeByteOff ptr 0 resType
    pokeByteOff ptr (1 * sizeOf resType) resStartPos
    pokeByteOff ptr (2 * sizeOf resType) resLength
    pokeByteOff ptr (3 * sizeOf resType) resAddData

foreign import ccall "lexit" lexit :: Ptr CChar -> Ptr Header -> Ptr Result -> IO CInt

callLex :: BS.ByteString -> Header -> IO (CInt, Header, [Result])
callLex bs hdr = do
  alloca $ \hdrptr -> do
    allocaArray (resultLimit * sizeOf (undefined :: Result)) $ \resptr -> do
      poke hdrptr hdr

      bsptr <- unsafeUseAsCString bs return
      res <- lexit bsptr hdrptr resptr

      hdrres <- peek hdrptr
      results <- peekArray (fromIntegral $ hdrResultNum hdrres) resptr
      return (res, hdrres, results)

test = "{}"

main = do
  let basehdr = Header 0 0 0 0 (fromIntegral $ BS.length test) 0
  (res, hdr, results) <- callLex test basehdr
  print (res, hdr, results)
