{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}

import Foreign
import Foreign.C.Types
import Data.ByteString.Unsafe (unsafeUseAsCString)
import System.IO.Unsafe (unsafePerformIO)
import qualified Data.Aeson as AE

import CLexType
import qualified Data.ByteString as BS
import Data.JsonStream.TokenParser (Element(..), TokenResult(..))

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
    resType :: LexResultType
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

{-# NOINLINE callLex #-}
callLex :: BS.ByteString -> Header -> (CInt, Header, [Result])
callLex bs hdr = unsafePerformIO $
  alloca $ \hdrptr ->
    allocaArray (resultLimit * sizeOf (undefined :: Result)) $ \resptr -> do
      poke hdrptr (hdr{hdrResultNum=0, hdrLength=(fromIntegral $ BS.length bs)})

      bsptr <- unsafeUseAsCString bs return
      res <- lexit bsptr hdrptr resptr

      hdrres <- peek hdrptr
      results <- peekArray (fromIntegral $ hdrResultNum hdrres) resptr
      return (res, hdrres, results)

data TempData = TempData {
    tmpBuffer :: BS.ByteString
  , tmpHeader :: Header
  , tmpError :: Bool
}

parseResults :: BS.ByteString -> (CInt, Header, [Result]) -> TokenResult
parseResults bs (err, hdr, results) = getNextResult (parse results) newtemp
  where
    newtemp = TempData bs hdr (err /= 0)
    parse = map toElement
    toElement (Result {..})
      -- | resType == resNumber =
      -- | resType == resString =
      | resType == resTrue = JValue (AE.Bool True)
      | resType == resFalse = JValue (AE.Bool False)
      | resType == resNull = JValue AE.Null
      | resType == resOpenBrace = ObjectBegin
      | resType == resCloseBrace = ObjectEnd
      | resType == resOpenBracket = ArrayBegin
      | resType == resCloseBracket = ArrayEnd
      | otherwise = error "Unsupported"

getNextResult :: [Element] -> TempData -> TokenResult
getNextResult [] (TempData {..})
  | tmpError = TokFailed ""
  | hdrPosition tmpHeader < hdrLength tmpHeader = parseResults tmpBuffer (callLex tmpBuffer tmpHeader)
  | otherwise = TokMoreData newdata ""
  where
    newdata dta = parseResults dta (callLex dta tmpHeader{hdrPosition=0, hdrLength=(fromIntegral $ BS.length dta)})
getNextResult (next:rest) tmpdata = PartialResult next (getNextResult rest tmpdata) ""

tokenParser :: BS.ByteString -> TokenResult
tokenParser dta = getNextResult [] (TempData dta newhdr False)
  where
    newhdr = Header 0 0 0 0 0 0

testTokens :: [BS.ByteString] -> TokenResult -> IO ()
testTokens [] (TokMoreData _ ctx) = putStrLn $ "End of data, rest: " ++ (show ctx)
testTokens _ (TokFailed ctx) = putStrLn $ "Token failed, rest: " ++ (show ctx)
testTokens (dta:rest) (TokMoreData np ctx) = do
    putStrLn $ "More data, rest: " ++ show ctx
    testTokens rest (np dta)
testTokens dta (PartialResult el np ctx) = do
    putStrLn $ "Got token " ++ show el ++ ", rest: " ++ show ctx
    testTokens dta np


main = do
  testTokens ["{[true, fa", "lse, null]}"] (tokenParser "")
