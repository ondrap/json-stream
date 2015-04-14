{-# LANGUAGE MultiWayIf        #-}
{-# LANGUAGE OverloadedStrings #-}

module Data.JStream.TokenParser (
    Element(..)
  , JValue(..)
  , TokenParser(..)
  , tokenParser
) where

import qualified Data.ByteString.Char8 as BS
import           Data.Char             (isDigit, isDigit, isSpace)
import qualified Data.Text             as T
import           Data.Text.Encoding    (decodeUtf8)

data JValue = JString T.Text | JNumber Int | JBool Bool | JNull deriving (Show)

data Element = ArrayBegin | ArrayEnd | ObjectBegin | ObjectEnd
               | ArrayKey T.Text | JValue JValue
               deriving (Show)

data TokenParser =  TokMoreData (BS.ByteString -> TokenParser)
                      | PartialResult Element TokenParser BS.ByteString -- ^ found element, continuation, unparsed data
                      | TokFailed

isBreakChar :: Char -> Bool
isBreakChar c = isSpace c || (c == '{') || (c == '[') || (c == '}') || (c == ']') || (c == ',')

ident :: BS.ByteString -> Element -> BS.ByteString -> Maybe TokenParser
ident name el input
  | BS.length name < BS.length input =
      if name `BS.isPrefixOf` input && isBreakChar (BS.index input (BS.length name))
        then Just $ PartialResult el (tokenParser rest) rest
        else Nothing
  | otherwise = Just $ moredata input tokenParser
  where
    rest = BS.drop (BS.length name) input

moredata :: BS.ByteString -> (BS.ByteString -> TokenParser) -> TokenParser
moredata bl p = TokMoreData (p . BS.append bl)

parseSpecChar :: [BS.ByteString] -> BS.ByteString -> TokenParser
parseSpecChar start bl
  | BS.null bl = moredata "" (parseSpecChar start)
  | chr == '"' = slashchr '"'
  | chr == '\\' = slashchr '\\'
  | chr == '/' = slashchr '\\'
  | chr == 'b' = slashchr '\b'
  | chr == 'f' = slashchr '\f'
  | chr == 'n' = slashchr '\n'
  | chr == 'r' = slashchr '\r'
  | chr == 't' = slashchr '\t'
  -- TODO - unicode
  | otherwise = TokFailed
  where
    chr = BS.head bl
    slashchr c = parseString (BS.singleton c:start) (BS.tail bl)

chooseKeyOrValue :: T.Text -> BS.ByteString -> TokenParser
chooseKeyOrValue text bl
  | BS.null bl = moredata "" (chooseKeyOrValue text)
  | chr == ':' = PartialResult (ArrayKey text) (tokenParser $ BS.tail bl) (BS.tail bl)
  | otherwise = PartialResult (JValue $ JString text) (tokenParser bl) bl
  where
    chr = BS.head bl


-- | Naparsuje string, ale po ukonceni stringu pocka, jestli neni ':', pak by to byl klic
parseString :: [BS.ByteString] -> BS.ByteString -> TokenParser
parseString start bl
  | BS.null bl = moredata "" (parseString start)
  | chr == '"' = chooseKeyOrValue (decodeUtf8 $ BS.concat $ reverse start) (BS.tail bl)
  | chr == '\\' = parseSpecChar start (BS.tail bl)
  | otherwise = parseString (cont:start) rest
  where
    chr = BS.head bl
    (cont, rest) = BS.break (\c -> c == '"' || c == '\\' ) bl

tokenParser :: BS.ByteString -> TokenParser
tokenParser bl
  | BS.null bl = moredata "" tokenParser
  | isSpace chr = tokenParser (BS.dropWhile isSpace bl)
  | chr == '[' = partres ArrayBegin
  | chr == ']' = partres ArrayEnd
  | chr == '{' = partres ObjectBegin
  | chr == '}' = partres ObjectEnd
  | chr == ',' = tokenParser (BS.tail bl)
  | isDigit chr =
        let (start, rest) = BS.span isDigit bl
        in if | BS.null rest -> moredata bl tokenParser
              | otherwise -> PartialResult (JValue $ JNumber $ read (BS.unpack start)) (tokenParser rest) rest
  | chr == '"' = parseString [] (BS.tail bl)
  | Just res <- ident "true" (JValue $ JBool True) bl = res
  | Just res <- ident "false" (JValue $ JBool False) bl = res
  | Just res <- ident "null" (JValue JNull) bl = res
  | otherwise = TokFailed
  where
    chr = BS.head bl
    partres elm = PartialResult elm (tokenParser $ BS.tail bl) (BS.tail bl)
