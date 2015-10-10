{-# LANGUAGE BangPatterns             #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE MultiWayIf               #-}
{-# LANGUAGE OverloadedStrings        #-}
{-# LANGUAGE RecordWildCards          #-}

module Data.JsonStream.CLexer (
    tokenParser
  , unescapeText
) where

import           Control.Applicative         ((<$>))
import           Control.Monad               (when)
import qualified Data.Aeson                  as AE
import qualified Data.ByteString             as BSW
import qualified Data.ByteString.Char8       as BS
import           Data.ByteString.Unsafe      (unsafeUseAsCString)
import           Data.Scientific             (Scientific, scientific)
import           Data.Text.Encoding          (decodeUtf8')
import           Data.Text.Internal.Unsafe   (inlinePerformIO)
import           Foreign
import           Foreign.C.Types
import           System.IO.Unsafe            (unsafeDupablePerformIO)

import           Data.JsonStream.CLexType
import           Data.JsonStream.TokenParser (Element (..), TokenResult (..))
import           Data.JsonStream.Unescape

-- | Limit for maximum size of a number; fail if larger number is found
-- this is needed to make this constant-space, otherwise we would eat
-- all memory just memoizing the number. The lexer fails if larger number
-- is encountered.
numberDigitLimit :: Int
numberDigitLimit = 200000

newtype ResultPtr = ResultPtr { unresPtr :: ForeignPtr () }

-- | Header for the C routing for batch parsing
data Header = Header {
    hdrCurrentState :: !CInt
  , hdrStateData    :: !CInt
  , hdrStateSata2   :: !CInt

  , hdrPosition     :: !CInt
  , hdrLength       :: !CInt
  , hdrResultNum    :: !CInt
  , hdrResultLimit  :: !CInt
} deriving (Show)

defHeader :: Header
defHeader = Header 0 0 0 0 0 0 0

instance Storable Header where
  sizeOf _ = 7 * sizeOf (undefined :: CInt)
  alignment _ = sizeOf (undefined :: CInt)
  peek ptr = do
    state <- peekByteOff ptr 0
    sdata1 <- peekByteOff ptr (sizeOf state)
    sdata2 <- peekByteOff ptr (2 * sizeOf state)
    position <- peekByteOff ptr (3 * sizeOf state)
    slength <- peekByteOff ptr (4 * sizeOf state)
    sresultnum <- peekByteOff ptr (5 * sizeOf state)
    sresultlimit <- peekByteOff ptr (6 * sizeOf state)
    return $ Header state sdata1  sdata2  position  slength sresultnum sresultlimit

  poke ptr (Header {..}) = do
    pokeByteOff ptr 0 hdrCurrentState
    pokeByteOff ptr (1 * sizeOf hdrCurrentState) hdrStateData
    pokeByteOff ptr (2 * sizeOf hdrCurrentState) hdrStateSata2
    pokeByteOff ptr (3 * sizeOf hdrCurrentState) hdrPosition
    pokeByteOff ptr (4 * sizeOf hdrCurrentState) hdrLength
    pokeByteOff ptr (5 * sizeOf hdrCurrentState) hdrResultNum
    pokeByteOff ptr (6 * sizeOf hdrCurrentState) hdrResultLimit

peekResultField :: Int -> Int -> ResultPtr -> Int
peekResultField n fieldno fptr = inlinePerformIO $ -- !! Using inlinePerformIO should be safe - we are just reading bytes from memory
  withForeignPtr (unresPtr fptr) $ \ptr ->
    fromIntegral <$> (peekByteOff ptr (recsize * n + fieldno * isize) :: IO CInt)
  where
    isize = sizeOf (undefined :: CInt)
    recsize = isize * 4

peekResultType :: Int -> ResultPtr -> LexResultType
peekResultType n fptr = inlinePerformIO $ -- !! Using inlinePerformIO should be safe - we are just reading bytes from memory
  withForeignPtr (unresPtr fptr) $ \ptr ->
    LexResultType <$> peekByteOff ptr (recsize * n)
  where
    isize = sizeOf (undefined :: CInt)
    recsize = isize * 4

foreign import ccall unsafe "lex_json" lexJson :: Ptr CChar -> Ptr Header -> Ptr () -> IO CInt

-- Call the C lexer. Returns (Error code, Header, (result_count, result_count, ResultPointer))
callLex :: BS.ByteString -> Header -> (CInt, Header, Int, ResultPtr)
callLex bs hdr = unsafeDupablePerformIO $ -- Using Dupable PerformIO should be safe - at the worst is is executed twice
  alloca $ \hdrptr -> do
    poke hdrptr (hdr{hdrResultNum=0, hdrLength=fromIntegral $ BS.length bs})

    bsptr <- unsafeUseAsCString bs return
    resptr <- mallocForeignPtrBytes (fromIntegral (hdrResultLimit hdr) * sizeOf (undefined :: CInt) * 4)
    res <- withForeignPtr resptr $ \resptr' ->
      lexJson bsptr hdrptr resptr'

    hdrres <- peek hdrptr
    let !rescount = fromIntegral (hdrResultNum hdrres)
    return (res, hdrres, rescount, ResultPtr resptr)

{-# INLINE substr #-}
substr :: Int -> Int -> BS.ByteString -> BS.ByteString
substr start len = BS.take len . BS.drop start

data TempData = TempData {
    tmpBuffer  :: BS.ByteString
  , tmpHeader  :: Header
  , tmpError   :: Bool
  , tmpNumbers :: [BS.ByteString]
}

-- | Parse number from bytestring to Scientific using JSON syntax rules
parseNumber :: BS.ByteString -> Maybe Scientific
parseNumber tnumber = do
    let
      (csign, r1) = parseSign tnumber :: (Int, BS.ByteString)
      ((num, numdigits), r2) = parseDecimal r1 :: ((Integer, Int), BS.ByteString)
      ((frac, frdigits), r3) = parseFract r2 :: ((Int, Int), BS.ByteString)
      (texp, rest) = parseE r3
    when (numdigits == 0 || not (BS.null rest)) Nothing
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

    parseNum txt (!start, !digits)
      | BS.null txt = ((start, digits), txt)
      | dchr >= 48 && dchr <= 57 = parseNum (BS.tail txt) (start * 10 + fromIntegral (dchr - 48), digits + 1)
      | otherwise = ((start, digits), txt)
      where
        dchr = BSW.head txt

-- | Parse particular result
parseResults :: TempData -> (CInt, Header, Int, ResultPtr) -> TokenResult
parseResults (TempData {tmpNumbers=tmpNumbers, tmpBuffer=bs}) (err, hdr, rescount, resptr) = parse 0
  where
    newtemp = TempData bs hdr (err /= 0)
    -- We iterate the items from CNT to 1, 1 is the last element, CNT is the first
    parse n
      | n >= rescount = getNextResult (newtemp tmpNumbers)
      | otherwise =
      let resType = peekResultType n resptr
          resStartPos = peekResultField n 1 resptr
          resLength = peekResultField n 2 resptr
          resAddData = peekResultField n 3 resptr
          next = parse (n + 1)
          context = BS.drop (resStartPos + resLength) bs
          textSection = substr resStartPos resLength bs
      in case () of
       _| resType == resNumberPartial ->
            if | resAddData == 0 -> getNextResult (newtemp [textSection]) -- First part of number
               | sum (map BS.length tmpNumbers) > numberDigitLimit ->  TokFailed -- Number too long
               | otherwise -> getNextResult (newtemp (textSection:tmpNumbers)) -- Middle part of number
        | resType == resTrue -> PartialResult (JValue (AE.Bool True)) next
        | resType == resFalse -> PartialResult (JValue (AE.Bool False)) next
        | resType == resNull -> PartialResult (JValue AE.Null) next
        | resType == resOpenBrace -> PartialResult ObjectBegin next
        | resType == resOpenBracket -> PartialResult ArrayBegin next
        -- ObjectEnd and ArrayEnd need pointer to data that wasn't parsed
        | resType == resCloseBrace -> PartialResult (ObjectEnd context) next
        | resType == resCloseBracket -> PartialResult (ArrayEnd context) next
        -- Number optimized - integer
        | resType == resNumberSmall ->
            if | resLength == 0 ->  PartialResult (JInteger resAddData) next
               | otherwise -> PartialResult
                               (JValue (AE.Number $ scientific (fromIntegral resAddData) ((-1) * resLength)))
                               next
        -- Number optimized - floating
        | resType == resNumber ->
            if | resAddData == 0 -> -- Single one-part number
                    case parseNumber textSection of
                      Just num -> PartialResult (JValue (AE.Number num)) next
                      Nothing -> TokFailed
               | otherwise ->  -- Concatenate number from partial parts
                     case parseNumber (BS.concat $ reverse (textSection:tmpNumbers)) of
                       Just num -> PartialResult (JValue (AE.Number num)) next
                       Nothing -> TokFailed
        | resType == resString ->
          if | resAddData == -1 -> -- One-part string without escaped characters
                case decodeUtf8' textSection  of
                  Right ctext -> PartialResult (JValue (AE.String ctext)) next
                  Left _ -> TokFailed
             | resAddData == 0 -> -- One-part string with escaped characters
                case unescapeText textSection of
                  Right ctext -> PartialResult (JValue (AE.String ctext)) next
                  _ -> TokFailed
             | otherwise -> PartialResult (StringContent textSection) -- Final part of partial strings
                            (PartialResult StringEnd next)
        | resType == resStringPartial ->
              PartialResult (StringContent textSection) next -- string section
        | otherwise -> error "Unsupported"

-- | Estimate number of elements in a chunk
estResultLimit :: BS.ByteString -> CInt
estResultLimit dta = fromIntegral $ 20 + BS.length dta `quot` 5

getNextResult :: TempData -> TokenResult
getNextResult tmp@(TempData {..})
  | tmpError = TokFailed
  | hdrPosition tmpHeader < hdrLength tmpHeader = parseResults tmp (callLex tmpBuffer tmpHeader)
  | otherwise = TokMoreData newdata
  where
    newdata dta = parseResults newtmp (callLex dta newhdr{hdrResultLimit=estResultLimit dta})
      where
        newtmp = tmp{tmpBuffer=dta}
        newhdr = tmpHeader{hdrPosition=0, hdrLength=fromIntegral $ BS.length dta}


tokenParser :: BS.ByteString -> TokenResult
tokenParser dta = getNextResult (TempData dta newhdr False [])
  where
    newhdr = defHeader{hdrLength=fromIntegral (BS.length dta), hdrResultLimit=estResultLimit dta}
