{-# LANGUAGE MultiWayIf        #-}
{-# LANGUAGE OverloadedStrings #-}

module Data.JsonStream.TokenParser (
    Element(..)
  , JValue(..)
  , TokenParser(..)
  , tokenParser
) where

import qualified Data.ByteString.Char8 as BS
import           Data.Char             (isDigit, isDigit, isSpace)
import qualified Data.Text             as T
import           Data.Text.Encoding    (decodeUtf8)

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

isBreakChar :: Char -> Bool
isBreakChar c = isSpace c || (c == '{') || (c == '[') || (c == '}') || (c == ']') || (c == ',')

-- | Parse unquoted identifier - true/false/null
ident :: BS.ByteString -> Element -> BS.ByteString -> Maybe TokenParser
ident name el input
  | BS.length name < BS.length input =
      if name `BS.isPrefixOf` input && isBreakChar (BS.index input (BS.length name))
        then Just $ PartialResult el (tokenParser rest) rest
        else Nothing
  | otherwise = Just $ moredata input tokenParser input
  where
    rest = BS.drop (BS.length name) input

moredata :: BS.ByteString -> (BS.ByteString -> TokenParser) -> BS.ByteString -> TokenParser
moredata bl p context = TokMoreData (p . BS.append bl) context

parseSpecChar :: [BS.ByteString] -> BS.ByteString -> BS.ByteString -> TokenParser
parseSpecChar start context bl
  | BS.null bl = moredata "" (parseSpecChar start context) ""
  | chr == '"' = slashchr '"'
  | chr == '\\' = slashchr '\\'
  | chr == '/' = slashchr '\\'
  | chr == 'b' = slashchr '\b'
  | chr == 'f' = slashchr '\f'
  | chr == 'n' = slashchr '\n'
  | chr == 'r' = slashchr '\r'
  | chr == 't' = slashchr '\t'
  -- TODO - unicode
  | otherwise = TokFailed bl
  where
    chr = BS.head bl
    slashchr c = parseString (BS.singleton c:start) context (BS.tail bl)

chooseKeyOrValue :: T.Text -> BS.ByteString -> BS.ByteString -> TokenParser
chooseKeyOrValue text context bl
  | BS.null bl = moredata "" (chooseKeyOrValue text context) context
  | chr == ':' = PartialResult (ObjectKey text) (tokenParser $ BS.tail bl) bl
  | otherwise = PartialResult (JValue $ JString text) (tokenParser bl) bl
  where
    chr = BS.head bl


-- | Naparsuje string, ale po ukonceni stringu pocka, jestli neni ':', pak by to byl klic
parseString :: [BS.ByteString] -> BS.ByteString -> BS.ByteString -> TokenParser
parseString start context bl
  | BS.null bl = moredata "" (parseString start context) $ BS.cons '"' (BS.concat (reverse start))
  | chr == '"' = chooseKeyOrValue (decodeUtf8 $ BS.concat $ reverse start) context (BS.tail bl)
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
  | Just res <- ident "true" (JValue $ JBool True) bl = res
  | Just res <- ident "false" (JValue $ JBool False) bl = res
  | Just res <- ident "null" (JValue JNull) bl = res
  | otherwise = TokFailed bl
  where
    chr = BS.head bl
    partres elm = PartialResult elm (tokenParser $ BS.tail bl) bl
