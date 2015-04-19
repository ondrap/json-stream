{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE OverloadedStrings        #-}
{-# LANGUAGE RecordWildCards          #-}
{-# LANGUAGE BangPatterns          #-}

module Data.JsonStream.CLexer (
  tokenParser
) where

import qualified Data.Aeson                  as AE
import           Data.ByteString.Unsafe      (unsafeUseAsCString)
import qualified Data.Text                   as T
import           Data.Text.Encoding          (decodeUtf8', encodeUtf8)
import           Foreign
import           Foreign.C.Types
import           System.IO.Unsafe            (unsafePerformIO)
import           Data.Scientific       (scientific, Scientific)
import Control.Monad (when)
import qualified Data.ByteString as BSW
import qualified Data.ByteString.Char8             as BS

import           Data.JsonStream.CLexType
import           Data.JsonStream.TokenParser (Element (..), TokenResult (..))

data Header = Header {
    hdrCurrentState :: !CInt
  , hdrStateData    :: !CInt
  , hdrStateSata2   :: !CInt

  , hdrPosition     :: !CInt
  , hdrLength       :: !CInt
  , hdrResultNum    :: !CInt
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


peekResult :: Int -> ForeignPtr () -> (LexResultType, CInt, CInt, CInt)
peekResult n fptr = unsafePerformIO $ do
  withForeignPtr fptr $ \ptr -> do
    rtype <- peekByteOff ptr (recsize * n)
    rpos <- peekByteOff ptr (recsize * n + isize)
    rlen <- peekByteOff ptr (recsize * n + 2 * isize)
    rdata <- peekByteOff ptr (recsize * n + 3 * isize)
    return (LexResultType rtype, rpos, rlen, rdata)
  where
    isize = sizeOf (undefined :: CInt)
    recsize = isize * 4


foreign import ccall "lexit" lexit :: Ptr CChar -> Ptr Header -> Ptr () -> IO CInt

{-# NOINLINE callLex #-}
callLex :: BS.ByteString -> Header -> (CInt, Header, [(Int, ForeignPtr ())])
callLex bs hdr = unsafePerformIO $
  alloca $ \hdrptr -> do
    poke hdrptr (hdr{hdrResultNum=0, hdrLength=(fromIntegral $ BS.length bs)})

    bsptr <- unsafeUseAsCString bs return
    -- TODO - mask
    resptr_ <- mallocBytes (resultLimit * (sizeOf (undefined :: CInt)) * 4)
    res <- lexit bsptr hdrptr resptr_
    resptr <- newForeignPtr finalizerFree resptr_

    hdrres <- peek hdrptr
    let results = zip [0..(fromIntegral $ hdrResultNum hdrres - 1)] (repeat resptr)
    return (res, hdrres, results)

substr :: Int -> Int -> BS.ByteString -> BS.ByteString
substr start len = BS.take len . BS.drop start

data TempData = TempData {
    tmpBuffer :: BS.ByteString
  , tmpHeader :: Header
  , tmpError  :: Bool
  , tmpNumbers :: [BS.ByteString]
}

parseNumber :: BS.ByteString -> Maybe Scientific
parseNumber tnumber = do
    let
      (csign, r1) = parseSign tnumber :: (Int, BS.ByteString)
      ((num, numdigits), r2) = parseDecimal r1 :: ((Integer, Int), BS.ByteString)
      ((frac, frdigits), r3) = parseFract r2 :: ((Int, Int), BS.ByteString)
      (texp, rest) = parseE r3
    when (numdigits == 0 || not (BS.null rest)) $ Nothing
    let dpart = fromIntegral csign * (num * (10 ^ frdigits) + fromIntegral frac) :: Integer
        e = texp - frdigits
    return $ scientific dpart e
  where
    parseFract txt
      | BS.null txt = ((0, 0), txt)
      | BS.head txt == '.' = parseDecimal (BS.tail txt)
      | otherwise = ((0,0), txt)

    parseE txt
      | BS.null txt = (0, txt)
      | firstc == 'e' || firstc == 'E' =
              let (sign, rest) = parseSign (BS.tail txt)
                  ((dnum, _), trest) = parseDecimal rest :: ((Int, Int), BS.ByteString)
              in (dnum * sign, trest)
      | otherwise = (0, txt)
      where
        firstc = BS.head txt

    parseSign txt
      | BS.null txt = (1, txt)
      | BS.head txt == '+' = (1, BS.tail txt)
      | BS.head txt == '-' = (-1, BS.tail txt)
      | otherwise = (1, txt)

    parseDecimal txt
      | BS.null txt = ((0, 0), txt)
      | otherwise = parseNum txt (0,0)

    -- parseNum :: BS.ByteString -> (Integer, Int) -> ((Integer, Int), BS.ByteString)
    parseNum txt (!start, !digits)
      | BS.null txt = ((start, digits), txt)
      | dchr >= 48 && dchr <= 57 = parseNum (BS.tail txt) (start * 10 + fromIntegral (dchr - 48), digits + 1)
      | otherwise = ((start, digits), txt)
      where
        dchr = BSW.head txt


parseResults :: TempData -> (CInt, Header, [(Int, ForeignPtr ())]) -> TokenResult
parseResults (TempData {tmpNumbers=tmpNumbers, tmpBuffer=bs}) (err, hdr, results) = parse results
  where
    newtemp = TempData bs hdr (err /= 0)
    parse [] = getNextResult (newtemp tmpNumbers)
    parse ((respos, ptr):rest) =
      let (resType, resStartPos, resLength, resAddData) = peekResult respos ptr
          context = BS.drop (fromIntegral (resStartPos + resLength)) bs
          textSection = substr (fromIntegral resStartPos) (fromIntegral resLength) bs
      in case () of
       _| null rest && resType == resNumberPartial && resAddData == 0 -> getNextResult (newtemp [textSection])
        | null rest && resType == resNumberPartial -> getNextResult (newtemp (textSection:tmpNumbers))
        | resType == resTrue -> PartialResult (JValue (AE.Bool True)) (parse rest)
        | resType == resFalse -> PartialResult (JValue (AE.Bool False)) (parse rest)
        | resType == resNull -> PartialResult (JValue AE.Null) (parse rest)
        | resType == resOpenBrace -> PartialResult ObjectBegin (parse rest)
        | resType == resOpenBracket -> PartialResult ArrayBegin (parse rest)
        -- ObjectEnd and ArrayEnd need pointer to data that wasn't parsed
        | resType == resCloseBrace -> PartialResult (ObjectEnd context) (parse rest)
        | resType == resCloseBracket -> PartialResult (ArrayEnd context) (parse rest)
        -- Number single
        | resType == resNumber && resAddData == 0 ->
            case parseNumber textSection of
              Just num -> PartialResult (JValue (AE.Number num)) (parse rest)
              Nothing -> TokFailed
        -- Number from parts
        | resType == resNumber ->
            case parseNumber (BS.concat $ reverse (textSection:tmpNumbers)) of
              Just num -> PartialResult (JValue (AE.Number num)) (parse rest)
              Nothing -> TokFailed
        -- Single string
        | resType == resString && resAddData == 0 ->
            case decodeUtf8' textSection of
              Right ctext -> PartialResult (JValue (AE.String ctext)) (parse rest)
              Left _ -> TokFailed
        -- Final part
        | resType == resString ->
            PartialResult (StringContent textSection) (PartialResult StringEnd (parse rest))
        -- -- Unicode
        | resType == resStringUni ->
            PartialResult (StringContent (encodeUtf8 $ T.singleton $ toEnum $ fromIntegral resAddData)) (parse rest)
        -- -- Partial string, not the end
        | resType == resStringPartial ->
            if resLength == 0
              then PartialResult (StringContent (BSW.singleton $ fromIntegral resAddData)) (parse rest)
              else PartialResult (StringContent textSection) (parse rest)
        | otherwise -> error "Unsupported"

getNextResult :: TempData -> TokenResult
getNextResult tmp@(TempData {..})
  | tmpError = TokFailed
  | hdrPosition tmpHeader < hdrLength tmpHeader = parseResults tmp (callLex tmpBuffer tmpHeader)
  | otherwise = TokMoreData newdata
  where
    newdata dta = parseResults newtmp (callLex dta newhdr)
      where
        newtmp = tmp{tmpBuffer=dta}
        newhdr = tmpHeader{hdrPosition=0, hdrLength=(fromIntegral $ BS.length dta)}


tokenParser :: BS.ByteString -> TokenResult
tokenParser dta = getNextResult (TempData dta newhdr False [])
  where
    newhdr = Header 0 0 0 0 (fromIntegral $ BS.length dta) 0

-- testTokens :: [BS.ByteString] -> TokenResult -> IO ()
-- testTokens [] (TokMoreData _ ctx) = putStrLn $ "End of data, rest: " ++ (show ctx)
-- testTokens _ (TokFailed ctx) = putStrLn $ "Token failed, rest: " ++ (show ctx)
-- testTokens (dta:rest) (TokMoreData np ctx) = do
--     putStrLn $ "More data, rest: " ++ show ctx
--     testTokens rest (np dta)
-- testTokens dta (PartialResult el np ctx) = do
--     putStrLn $ "Got token " ++ show el ++ ", rest: " ++ show ctx
--     testTokens dta np
--
