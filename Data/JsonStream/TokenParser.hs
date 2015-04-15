{-# LANGUAGE MultiWayIf        #-}
{-# LANGUAGE OverloadedStrings #-}

module Data.JsonStream.TokenParser (
    Element(..)
  , JValue(..)
  , TokenParser(..)
  , tokenParser
) where

import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy.Char8 as BL
import           Data.Char             (isDigit, isDigit, isSpace, isHexDigit, isLower)
import qualified Data.Text             as T
import           Data.Text.Encoding    (decodeUtf8With, encodeUtf8)
import Data.Text.Encoding.Error (lenientDecode)

data JValue = JString T.Text | JNumber Int | JBool Bool | JNull
              | JArray [JValue] | JObject [(T.Text, JValue)] deriving (Show, Eq)

data Element = ArrayBegin | ArrayEnd | ObjectBegin | ObjectEnd
               | ObjectKey T.Text | JValue JValue
               deriving (Show, Eq)

-- | Intermediate results for parsing data into JSON tokens.
data TokenParser =  TokMoreData (BS.ByteString -> TokenParser) BS.ByteString
                  | PartialResult Element TokenParser BS.ByteString
                  -- ^ found element, continuation, actual parsing view - so that we can report the unparsed
                  -- data when the parsing finishes.
                  | TokFailed BS.ByteString

-- For debugging purposes
instance Show TokenParser where
  show (TokMoreData _ _) = "TokMoreData"
  show (TokFailed _) = "TokFailed"
  show (PartialResult el _ rest) = "(PartialResult " ++ show el ++ " " ++ show rest ++ ")"

isBreakChar :: Char -> Bool
isBreakChar c = isSpace c || (c == '{') || (c == '[') || (c == '}') || (c == ']') || (c == ',')

-- | Parse unquoted identifier - true/false/null
parseIdent :: BS.ByteString -> BS.ByteString -> BS.ByteString -> TokenParser
parseIdent tmpid context input
  | BS.null input = moredata "" (\d -> parseIdent tmpid (BS.append context d) d) context
  | isBreakChar firstChar = toTemp tmpid input
  | BS.length tmpid > 5 = TokFailed context
  | isLower firstChar = parseIdent (tmpid `BS.append` BS.takeWhile isLower input) context (BS.dropWhile isLower input)
  | otherwise = TokFailed context
  where
    firstChar = BS.head input
    toTemp "true" dta = PartialResult (JValue $ JBool True) (tokenParser input) context
    toTemp "false" dta = PartialResult (JValue $ JBool False) (tokenParser input) context
    toTemp "null" dta = PartialResult (JValue JNull) (tokenParser input) context
    toTemp _ _ = TokFailed context

moredata :: BS.ByteString -> (BS.ByteString -> TokenParser) -> BS.ByteString -> TokenParser
moredata bl p context = TokMoreData (p . BS.append bl) context

parseUnicode :: [BS.ByteString] -> BS.ByteString -> BS.ByteString -> BS.ByteString -> TokenParser
parseUnicode chunks uni context bl
  | BS.null bl =
      moredata "" (\t -> parseUnicode chunks uni t t) context
  | remaining == 0 = parseString (encodeUtf8 (T.singleton $ hexToChar uni) : chunks) bl bl
  | BS.all isHexDigit newbytes =
      parseUnicode chunks (BS.append uni $ BS.take remaining bl)
                           context (BS.drop remaining bl)
  | otherwise = TokFailed context
  where
    hexToChar = toEnum . foldl1 (\a b -> 16 * a + b) . map hexCharToInt . BS.unpack
    newbytes = BS.take remaining bl
    remaining = 4 - BS.length uni
    hexCharToInt :: Char -> Int
    hexCharToInt c
      | c >= 'A' && c <= 'F' = 10 + (fromEnum c - fromEnum 'A')
      | c >= 'a' && c <= 'f' = 10 + (fromEnum c - fromEnum 'a')
      | isDigit c = fromEnum c - fromEnum '0'
      | otherwise = error "Incorrect hex input, internal error."


parseSpecChar :: [BS.ByteString] -> BS.ByteString -> BS.ByteString -> TokenParser
parseSpecChar chunks context bl
  | BS.null bl = moredata "" (parseSpecChar chunks context) ""
  | chr == '"' = slashchr '"'
  | chr == '\\' = slashchr '\\'
  | chr == '/' = slashchr '\\'
  | chr == 'b' = slashchr '\b'
  | chr == 'f' = slashchr '\f'
  | chr == 'n' = slashchr '\n'
  | chr == 'r' = slashchr '\r'
  | chr == 't' = slashchr '\t'
  | chr == 'u' = parseUnicode chunks "" context (BS.tail bl)
  | otherwise = TokFailed bl
  where
    chr = BS.head bl
    slashchr c = parseString (BS.singleton c:chunks) context (BS.tail bl)

chooseKeyOrValue :: T.Text -> BS.ByteString -> BS.ByteString -> TokenParser
chooseKeyOrValue text context bl
  | BS.null bl = moredata "" (chooseKeyOrValue text context) context
  | chr == ':' = PartialResult (ObjectKey text) (tokenParser $ BS.tail bl) context
  | otherwise = PartialResult (JValue $ JString text) (tokenParser bl) context
  where
    chr = BS.head bl


-- | Naparsuje string, ale po ukonceni stringu pocka, jestli neni ':', pak by to byl klic
parseString :: [BS.ByteString] -> BS.ByteString -> BS.ByteString -> TokenParser
parseString start context bl
  | BS.null bl = moredata "" (parseString start context) $ BS.cons '"' (BS.concat (reverse start))
  | chr == '"' = chooseKeyOrValue (decodeUtf8With lenientDecode $ BS.concat $ reverse start) context (BS.tail bl)
  | chr == '\\' = parseSpecChar start context (BS.tail bl)
  | otherwise = parseString (cont:start) context rest
  where
    chr = BS.head bl
    (cont, rest) = BS.break (\c -> c == '"' || c == '\\' ) bl

tokenParser :: BS.ByteString -> TokenParser
tokenParser bl
  | BS.null bl = moredata "" tokenParser ""
  | isSpace chr = tokenParser (BS.dropWhile isSpace bl)
  | chr == '[' = partres ArrayBegin
  | chr == ']' = partres ArrayEnd
  | chr == '{' = partres ObjectBegin
  | chr == '}' = partres ObjectEnd
  | chr == ',' = tokenParser (BS.tail bl)
  | isDigit chr =
        let (start, rest) = BS.span isDigit bl
        in if | BS.null rest -> moredata bl tokenParser bl
              | otherwise -> PartialResult (JValue $ JNumber $ read (BS.unpack start)) (tokenParser rest) bl
  | chr == '"' = parseString [] bl (BS.tail bl)
  | chr == 't' || chr == 'f' || chr == 'n' = parseIdent "" bl bl
  | otherwise = TokFailed bl
  where
    chr = BS.head bl
    partres elm = PartialResult elm (tokenParser $ BS.tail bl) bl
