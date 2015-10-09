{-# LANGUAGE BangPatterns, CPP, ForeignFunctionInterface, GeneralizedNewtypeDeriving, MagicHash,
    UnliftedFFITypes #-}

module Data.JsonStream.Unescape where

import Control.Monad.ST.Unsafe (unsafeIOToST, unsafeSTToIO)
import Control.Exception (evaluate, try)
import Control.Monad.ST (runST)
import Data.ByteString as B
import Data.ByteString.Internal as B hiding (c2w)
import Data.Text.Encoding.Error (OnDecodeError, UnicodeException, strictDecode)
import Data.Text.Internal (Text(..), safe, text)
import Data.Text.Internal.Private (runText)
import Data.Text.Internal.Unsafe.Char (unsafeWrite)
import Data.Text.Internal.Unsafe.Shift (shiftR)
import Data.Text.Unsafe (unsafeDupablePerformIO)
import Data.Word (Word8, Word32)
import Foreign.C.Types (CSize(..), CInt(..))
import Foreign.ForeignPtr (withForeignPtr)
import Foreign.Marshal.Utils (with)
import Foreign.Ptr (Ptr, minusPtr, nullPtr, plusPtr)
import Foreign.Storable (Storable, peek, poke)
import GHC.Base (ByteArray#, MutableByteArray#)
import qualified Data.Text.Array as A
import qualified Data.Text.Internal.Encoding.Fusion as E
import qualified Data.Text.Internal.Fusion as F
import Data.Text.Encoding.Error (UnicodeException(..))
import Control.Exception (throw)

foreign import ccall unsafe "_js_decode_string" c_js_decode
    :: MutableByteArray# s -> Ptr CSize
    -> Ptr Word8 -> Ptr Word8 -> IO CInt

decodeJString :: ByteString -> Text
decodeJString (PS fp off len) = runText $ \done -> do
  let go dest = withForeignPtr fp $ \ptr ->
        with (0::CSize) $ \destOffPtr -> do
          let end = ptr `plusPtr` (off + len)
              loop curPtr = do
                res <- c_js_decode (A.maBA dest) destOffPtr curPtr end
                case res of
                  0 -> do
                    n <- peek destOffPtr
                    unsafeSTToIO (done dest (fromIntegral n))
                  _ ->
                    throw (DecodeError desc Nothing)
          loop (ptr `plusPtr` off)
  (unsafeIOToST . go) =<< A.new len
 where
  desc = "Data.Text.Internal.Encoding.decodeUtf8: Invalid UTF-8 stream"
