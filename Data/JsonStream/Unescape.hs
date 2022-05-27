{-# LANGUAGE CPP #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE MagicHash                #-}
{-# LANGUAGE UnliftedFFITypes         #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE MultiWayIf   #-}
{-# LANGUAGE PatternGuards   #-}

module Data.JsonStream.Unescape (
    unescapeText
  , unsafeDecodeASCII
) where

import           Data.ByteString            as B
import           Data.ByteString.Internal   as B hiding (c2w)
import           Data.Text.Encoding.Error   (UnicodeException (..))
import           Data.Text.Internal         (Text (..))
import           Data.Text.Unsafe           (unsafeDupablePerformIO)
import           Data.Word                  (Word8, Word32)
import           Foreign.ForeignPtr         (withForeignPtr)
import           Foreign.Ptr                (Ptr, plusPtr)
import           Foreign.Storable           (peek)
import qualified Data.Text as T

#if MIN_VERSION_text(2,0,0)

import qualified Data.Primitive           as P
import qualified Data.Text.Array          as TA
import qualified Data.Text.Internal       as T
import           Data.Bits                (shiftL, shiftR, (.&.), (.|.))
import           Control.Exception        (try, throwIO)
import Foreign.ForeignPtr (ForeignPtr)
import qualified Data.ByteString.Short.Internal as SBS

#else

import           Control.Exception          (evaluate, throw, try)
import           Control.Monad.ST.Unsafe    (unsafeIOToST, unsafeSTToIO)
import           Data.Text.Internal.Private (runText)
import           Foreign.Marshal.Utils      (with)
import qualified Data.Text.Array            as A
import           GHC.Base                   (MutableByteArray#)
import           Foreign.C.Types            (CInt (..), CSize (..))
import qualified Data.Text.Encoding as TE

#endif

unsafeDecodeASCII :: ByteString -> T.Text

#if MIN_VERSION_text(2,0,0)
unsafeDecodeASCII bs = withBS bs $ \_fp len -> if len == 0 then T.empty else
  let !(SBS.SBS arr) = SBS.toShort bs in T.Text (TA.ByteArray arr) 0 len

#else
unsafeDecodeASCII = TE.decodeLatin1
#endif


#if !MIN_VERSION_text(2,0,0)

foreign import ccall unsafe "_jstream_decode_string" c_js_decode
    :: MutableByteArray# s -> Ptr CSize
    -> Ptr Word8 -> Ptr Word8 -> IO CInt

unescapeText' :: ByteString -> Text
unescapeText' (PS fp off len) = runText $ \done -> do
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
  desc = "Data.JsonStream.Unescape.unescapeText': Invalid UTF-8 stream"
{-# INLINE unescapeText' #-}

unescapeText :: ByteString -> Either UnicodeException Text
unescapeText = unsafeDupablePerformIO . try . evaluate . unescapeText'
{-# INLINE unescapeText #-}

#else

withBS :: ByteString -> (ForeignPtr Word8 -> Int -> r) -> r
#if MIN_VERSION_bytestring(0,11,0)
withBS (BS !sfp !slen)       kont = kont sfp slen
#else
withBS (PS !sfp !soff !slen) kont = kont (plusForeignPtr sfp soff) slen
#endif
{-# INLINE withBS #-}

unescapeText :: ByteString -> Either UnicodeException Text
unescapeText = unsafeDupablePerformIO . try . unescapeTextIO

throwDecodeError :: IO a
throwDecodeError =
  let desc = "Data.Text.Internal.Encoding.decodeUtf8: Invalid UTF-8 stream"
   in throwIO (DecodeError desc Nothing)

-- The following is copied from aeson-2.0 

-------------------------------------------------------------------------------
-- unescapeTextIO
-------------------------------------------------------------------------------

-- This function is generated using staged-streams
-- See: https://github.com/phadej/staged/blob/master/staged-streams-unicode/src/Unicode/JSON.hs
--
-- Because @aeson@ better to not use template-haskell itself,
-- we dump the splice and prettify it by hand a bit.
--
unescapeTextIO :: ByteString -> IO Text
unescapeTextIO bs = withBS bs $ \fptr len ->
  withForeignPtr fptr $ \begin -> do
    let end :: Ptr Word8
        end = plusPtr begin len

    arr <- P.newPrimArray len

    let write3bytes :: Int -> Word8 -> Word8 -> Word8 -> Ptr Word8 -> IO Text
        write3bytes !out !b1 !b2 !b3 !inp = do
          P.writePrimArray arr out b1
          write2bytes (out + 1) b2 b3 inp

        write2bytes :: Int -> Word8 -> Word8 -> Ptr Word8 -> IO Text
        write2bytes !out !b1 !b2 !inp = do
          P.writePrimArray arr out b1
          write1byte (out + 1) b2 inp

        write1byte :: Int -> Word8 -> Ptr Word8 -> IO Text
        write1byte !out !b1 !inp = do
          P.writePrimArray arr out b1
          state_start (out + 1) inp

        writeCodePoint :: Int -> Ptr Word8 -> Word32 -> IO Text
        writeCodePoint !out !inp !acc
          | acc <= 127 = do
            P.writePrimArray arr out (fromIntegral acc :: Word8)
            state_start (out + 1) (plusPtr inp 1)

          | acc <= 2047 = do
            let b1 = fromIntegral (shiftR acc 6 .|. 192) :: Word8
            let b2 = fromIntegral ((acc .&. 63) .|. 128) :: Word8
            P.writePrimArray arr out b1
            write1byte (out + 1) b2 (plusPtr inp 1)

          | acc <= 65535 = do
            let b1 = fromIntegral (shiftR acc 12 .|. 224) :: Word8
            let b2 = fromIntegral ((shiftR acc 6 .&. 63) .|.  128) :: Word8
            let b3 = fromIntegral ((acc .&. 63) .|. 128) :: Word8
            P.writePrimArray arr out b1
            write2bytes (out + 1) b2 b3 (plusPtr inp 1)

          | otherwise = do
            let b1 = fromIntegral (shiftR acc 18 .|. 240) :: Word8
            let b2 = fromIntegral ((shiftR acc 12 .&. 63) .|. 128) :: Word8
            let b3 = fromIntegral ((shiftR acc 6 .&. 63) .|. 128) :: Word8
            let b4 = fromIntegral ((acc .&. 63) .|. 128) :: Word8
            P.writePrimArray arr out b1
            write3bytes (out + 1) b2 b3 b4 (plusPtr inp 1)

        state_sudone :: Int -> Ptr Word8 -> Word32 -> Word32 -> IO Text
        state_sudone !out !inp !hi !lo
          | 56320 <= lo, lo <= 57343
          = writeCodePoint out inp (65536 + (shiftL (hi - 55296) 10 .|.  (lo - 56320)))
          
          | otherwise
          = throwDecodeError

        state_su4 :: Int -> Ptr Word8 -> Word32 -> Word32 -> IO Text
        state_su4 !out !inp !hi !acc
          | inp == end = throwDecodeError
          | otherwise = do
            w8 <- peek inp
            if | 48 <= w8, w8 <= 57 ->
                 state_sudone out inp hi (shiftL acc 4 .|. fromIntegral (w8 - 48))
               | 65 <= w8, w8 <= 70 ->
                 state_sudone out inp hi (shiftL acc 4 .|. fromIntegral (w8 - 55))
               | 97 <= w8, w8 <= 102 ->
                 state_sudone out inp hi (shiftL acc 4 .|. fromIntegral (w8 - 87))
               | otherwise ->
                 throwDecodeError

        state_su3 :: Int -> Ptr Word8 -> Word32 -> Word32 -> IO Text
        state_su3 !out !inp !hi !acc
          | inp == end = throwDecodeError
          | otherwise = do
            w8 <- peek inp
            if | 48 <= w8, w8 <= 57 ->
                 state_su4 out (plusPtr inp 1) hi (shiftL acc 4 .|. fromIntegral (w8 - 48))
               | 65 <= w8, w8 <= 70 ->
                 state_su4 out (plusPtr inp 1) hi (shiftL acc 4 .|. fromIntegral (w8 - 55))
               | 97 <= w8, w8 <= 102 ->
                 state_su4 out (plusPtr inp 1) hi (shiftL acc 4 .|. fromIntegral (w8 - 87))
               | otherwise ->
                 throwDecodeError

        state_su2 :: Int -> Ptr Word8 -> Word32 -> Word32 -> IO Text
        state_su2 !out !inp !hi !acc
          | inp == end = throwDecodeError
          | otherwise = do
            w8 <- peek inp
            if | 48 <= w8, w8 <= 57 -> 
                 state_su3 out (plusPtr inp 1) hi (shiftL acc 4 .|. fromIntegral (w8 - 48))
               | 65 <= w8, w8 <= 70 ->
                 state_su3 out (plusPtr inp 1) hi (shiftL acc 4 .|. fromIntegral (w8 - 55))
               | 97 <= w8, w8 <= 102 ->
                 state_su3 out (plusPtr inp 1) hi (shiftL acc 4 .|. fromIntegral (w8 - 87))
               | otherwise ->
                 throwDecodeError

        state_su1 :: Int -> Ptr Word8 -> Word32 -> IO Text
        state_su1 !out !inp !hi
          | inp == end = throwDecodeError
          | otherwise = do
            w8 <- peek inp
            if | 48 <= w8, w8 <= 57 ->
                 state_su2 out (plusPtr inp 1) hi (fromIntegral (w8 - 48))
               | 65 <= w8, w8 <= 70 ->
                 state_su2 out (plusPtr inp 1) hi (fromIntegral (w8 - 55))
               | 97 <= w8, w8 <= 102 ->
                 state_su2 out (plusPtr inp 1) hi (fromIntegral (w8 - 87))
               | otherwise ->
                 throwDecodeError

        state_su :: Int -> Ptr Word8 -> Word32 -> IO Text
        state_su !out !inp !hi
          | inp == end = throwDecodeError
          | otherwise = do
            w8 <- peek inp
            case w8 of
              117 -> state_su1 out (plusPtr inp 1) hi
              _   -> throwDecodeError

        state_ss :: Int -> Ptr Word8 -> Word32 -> IO Text
        state_ss !out !inp !hi
          | inp == end = throwDecodeError
          | otherwise = do
            w8 <- peek inp
            case w8 of
              92 -> state_su out (plusPtr inp 1) hi
              _  -> throwDecodeError

        state_udone :: Int -> Ptr Word8 -> Word32 -> IO Text
        state_udone !out !inp !acc
          | acc < 55296 || acc > 57343 =
            writeCodePoint out inp acc

          | acc < 56320 =
            state_ss out (plusPtr inp 1) acc

          | otherwise =
            throwDecodeError

        state_u4 :: Int -> Ptr Word8 -> Word32 -> IO Text
        state_u4 !out !inp !acc
          | inp == end = throwDecodeError
          | otherwise = do
            w8 <- peek inp
            if | 48 <= w8, w8 <= 57 ->
                 state_udone out inp (shiftL acc 4 .|. fromIntegral (w8 - 48))
               | 65 <= w8, w8 <= 70 ->
                 state_udone out inp (shiftL acc 4 .|. fromIntegral (w8 - 55))
               | 97 <= w8, w8 <= 102 ->
                 state_udone out inp (shiftL acc 4 .|. fromIntegral (w8 - 87))
               | otherwise ->
                 throwDecodeError

        state_u3 :: Int -> Ptr Word8 -> Word32 -> IO Text
        state_u3 !out !inp !acc
          | inp == end = throwDecodeError
          | otherwise = do
            w8 <- peek inp
            if | 48 <= w8, w8 <= 57 ->
                 state_u4 out (plusPtr inp 1) (shiftL acc 4 .|. fromIntegral (w8 - 48))
               | 65 <= w8, w8 <= 70 ->
                 state_u4 out (plusPtr inp 1) (shiftL acc 4 .|. fromIntegral (w8 - 55))
               | 97 <= w8, w8 <= 102 ->
                 state_u4 out (plusPtr inp 1) (shiftL acc 4 .|. fromIntegral (w8 - 87))
               | otherwise ->
                 throwDecodeError

        state_u2 :: Int -> Ptr Word8 -> Word32 -> IO Text
        state_u2 !out !inp !acc
          | inp == end = throwDecodeError
          | otherwise = do
            w8 <- peek inp
            if | 48 <= w8, w8 <= 57 ->
                 state_u3 out (plusPtr inp 1) (shiftL acc 4 .|. fromIntegral (w8 - 48))
               | 65 <= w8, w8 <= 70 ->
                 state_u3 out (plusPtr inp 1) (shiftL acc 4 .|. fromIntegral (w8 - 55))
               | 97 <= w8, w8 <= 102 ->
                 state_u3 out (plusPtr inp 1) (shiftL acc 4 .|. fromIntegral (w8 - 87))
               | otherwise ->
                 throwDecodeError

        state_u1 :: Int -> Ptr Word8 -> IO Text
        state_u1 !out !inp
          | inp == end = throwDecodeError
          | otherwise = do
            w8 <- peek inp
            if | 48 <= w8, w8 <= 57 ->
                 state_u2 out (plusPtr inp 1) (fromIntegral (w8 - 48))
               | 65 <= w8, w8 <= 70 ->
                 state_u2 out (plusPtr inp 1) (fromIntegral (w8 - 55))
               | 97 <= w8, w8 <= 102 ->
                 state_u2 out (plusPtr inp 1) (fromIntegral (w8 - 87))
               | otherwise ->
                 throwDecodeError

        state_escape :: Int -> Ptr Word8 -> IO Text
        state_escape !out !inp
          | inp == end = throwDecodeError
          | otherwise  = do
            w8 <- peek inp
            case w8 of
              34 -> do
                P.writePrimArray arr out 34
                state_start (out + 1) (plusPtr inp 1)

              92 -> do
                P.writePrimArray arr out 92
                state_start (out + 1) (plusPtr inp 1)

              47 -> do
                P.writePrimArray arr out 47
                state_start (out + 1) (plusPtr inp 1)

              98 -> do
                P.writePrimArray arr out 8
                state_start (out + 1) (plusPtr inp 1)

              102 -> do
                P.writePrimArray arr out 12
                state_start (out + 1) (plusPtr inp 1)

              110 -> do
                P.writePrimArray arr out 10
                state_start (out + 1) (plusPtr inp 1)

              114 -> do
                P.writePrimArray arr out 13
                state_start (out + 1) (plusPtr inp 1)

              116 -> do
                P.writePrimArray arr out 9
                state_start (out + 1) (plusPtr inp 1)

              117 ->
                state_u1 out (plusPtr inp 1)

              _ -> throwDecodeError

        state_input4c :: Int -> Ptr Word8 -> Word8 -> Word8 -> Word8 -> IO Text
        state_input4c !out !inp !b1 !b2 !b3
          | inp == end = throwDecodeError
          | otherwise  = do
            w8 <- peek inp
            if | (w8 .&. 192) == 128
               , let acc    = shiftL (fromIntegral (b1 .&. 7)) 18
               , let acc'   = acc .|. shiftL (fromIntegral (b2 .&. 63)) 12
               , let acc''  = acc' .|. shiftL (fromIntegral (b3 .&. 63)) 6
               , let acc''' = acc'' .|. fromIntegral (w8 .&. 63) :: Word32
               , acc''' >= 65536 && acc''' < 1114112 -> do
                 P.writePrimArray arr out b1
                 write3bytes (out + 1) b2 b3 w8 (plusPtr inp 1)

               | otherwise ->
                 throwDecodeError

        state_input4b :: Int -> Ptr Word8 -> Word8 -> Word8 -> IO Text
        state_input4b !out !inp !b1 !b2
          | inp == end = throwDecodeError
          | otherwise  = do
            w8 <- peek inp
            if | (w8 .&. 192) == 128 ->
                 state_input4c out (plusPtr inp 1) b1 b2 w8

               | otherwise ->
                 throwDecodeError

        state_input4 :: Int -> Ptr Word8 -> Word8 -> IO Text
        state_input4 !out !inp !b1
          | inp == end = throwDecodeError
          | otherwise  = do
            w8 <- peek inp
            if | (w8 .&. 192) == 128 ->
                 state_input4b out (plusPtr inp 1) b1 w8

               | otherwise ->
                 throwDecodeError

        state_input3b :: Int -> Ptr Word8 -> Word8 -> Word8 -> IO Text
        state_input3b !out !inp !b1 !b2
          | inp == end = throwDecodeError
          | otherwise  = do
            w8 <- peek inp
            if | (w8 .&. 192) == 128
               , let acc   = shiftL (fromIntegral (b1 .&. 15)) 12
               , let acc'  = acc .|.  shiftL (fromIntegral (b2 .&. 63)) 6
               , let acc'' = acc' .|. fromIntegral (w8 .&. 63) :: Word32
               , (acc'' >= 2048 && acc'' < 55296) || acc'' > 57343 -> do
                 P.writePrimArray arr out b1
                 write2bytes (out + 1) b2 w8 (plusPtr inp 1)

               | otherwise ->
                 throwDecodeError

        state_input3 :: Int -> Ptr Word8 -> Word8 -> IO Text
        state_input3 !out !inp !b1
          | inp == end = throwDecodeError
          | otherwise  = do
            w8 <- peek inp
            if | (w8 .&. 192) == 128 ->
                 state_input3b out (plusPtr inp 1) b1 w8

               | otherwise ->
                 throwDecodeError

        state_input2 :: Int -> Ptr Word8 -> Word8 -> IO Text
        state_input2 !out !inp !b1
          | inp == end = throwDecodeError
          | otherwise  = do
            w8 <- peek inp
            if | (w8 .&. 192) == 128,
                 let acc = shiftL (fromIntegral (b1 .&. 63)) 6 :: Word32
                     acc' = acc .|. fromIntegral (w8 .&. 63) :: Word32
               , acc' >= 128 -> do
                 P.writePrimArray arr out b1
                 write1byte (out + 1) w8 (plusPtr inp 1)

               | otherwise ->
                 throwDecodeError

        state_start :: Int -> Ptr Word8 -> IO Text
        state_start !out !inp
          | inp == end = do
            P.shrinkMutablePrimArray arr out
            frozenArr <- P.unsafeFreezePrimArray arr
            return $ case frozenArr of
              P.PrimArray ba -> T.Text (TA.ByteArray ba) 0 out

          | otherwise = do
            w8 <- peek inp
            if | w8 == 92 -> state_escape out (plusPtr inp 1)
               | w8 < 128 -> do
                 P.writePrimArray arr out w8
                 state_start (out + 1) (plusPtr inp 1)

               | w8 < 192 -> throwDecodeError
               | w8 < 224 -> state_input2 out (plusPtr inp 1) w8
               | w8 < 240 -> state_input3 out (plusPtr inp 1) w8
               | w8 < 248 -> state_input4 out (plusPtr inp 1) w8

               | otherwise -> throwDecodeError

    -- start the state machine
    state_start (0 :: Int) begin

#endif