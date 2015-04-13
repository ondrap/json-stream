{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}

import qualified Data.Text as T
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy.Char8 as BL
import Data.Char (isSpace, isDigit, isDigit)
import Data.Text.Encoding (decodeUtf8)

import Debug.Trace

data Element = ArrayBegin | ArrayEnd | ObjectBegin | ObjectEnd
               | ArrayKey T.Text
               | JString T.Text | JNumber Int | JBool Bool | JNull
               deriving (Show)

data ParseResult =  MoreData (BS.ByteString -> ParseResult)
                  | PartialResult Element ParseResult
                  | Failed

isBreakChar :: Char -> Bool
isBreakChar c = isSpace c || (c == '{') || (c == '[') || (c == '}') || (c == ']') || (c == ',')

ident :: BL.ByteString -> Element -> BL.ByteString -> Maybe ParseResult
ident name el input
  | BL.length name < BL.length input =
      if name `BL.isPrefixOf` input && isBreakChar (BL.index input (BL.length name))
        then Just $ PartialResult el (parser (BL.drop (BL.length name) input))
        else Nothing
  | otherwise = Just $ moredata input parser

moredata :: BL.ByteString -> (BL.ByteString -> ParseResult) -> ParseResult
moredata bl p = MoreData (\d -> p (BL.append bl (BL.fromChunks [d])))

parseSpecChar :: BL.ByteString -> BL.ByteString -> ParseResult
parseSpecChar start bl
  | BL.null bl = moredata "" (parseSpecChar start)
  | chr == '"' = slashchr '"'
  | chr == '\\' = slashchr '\\'
  | chr == '/' = slashchr '\\'
  | chr == 'b' = slashchr '\b'
  | chr == 'f' = slashchr '\f'
  | chr == 'n' = slashchr '\n'
  | chr == 'r' = slashchr '\r'
  | chr == 't' = slashchr '\t'
  -- TODO - unicode
  | otherwise = Failed
  where
    chr = BL.head bl
    slashchr chr = parseString (start `BL.append` BL.singleton chr) (BL.tail bl)


chooseKeyOrValue text bl
  | BL.null bl = moredata "" (chooseKeyOrValue text)
  | chr == ':' = PartialResult (ArrayKey text) (parser $ BL.tail bl)
  | otherwise = PartialResult (JString text) (parser bl)
  where
    chr = BL.head bl


-- | Naparsuje string, ale po ukonceni stringu pocka, jestli neni ':', pak by to byl klic
parseString :: BL.ByteString -> BL.ByteString -> ParseResult
parseString start bl
  | BL.null bl = moredata "" (parseString start)
  | chr == '"' = chooseKeyOrValue (decodeUtf8 $ BS.concat $ BL.toChunks start) (BL.tail bl)
  | chr == '\\' && BL.length bl == 1 = MoreData (\d -> parseString start $ bl `BL.append` BL.fromChunks [d])
  | chr == '\\' = parseSpecChar start (BL.tail bl)
  | otherwise = parseString (BL.append start cont) rest
  where
    chr = BL.head bl
    sndchr = BL.index bl 1
    (cont, rest) = BL.break (\c -> c == '"' || c == '\\' ) bl


parser :: BL.ByteString -> ParseResult
parser bl
  | BL.null bl = moredata "" parser
  | isSpace chr = parser (BL.dropWhile isSpace bl)
  | chr == '[' = PartialResult ArrayBegin (parser (BL.tail bl))
  | chr == ']' = PartialResult ArrayEnd (parser (BL.tail bl))
  | chr == '{' = PartialResult ObjectBegin (parser (BL.tail bl))
  | chr == '}' = PartialResult ObjectEnd (parser (BL.tail bl))
  | chr == ',' = parser (BL.tail bl)
  | isDigit chr =
        let (start, rest) = BL.span isDigit bl
        in if | BL.null rest -> moredata bl parser
              | otherwise -> PartialResult (JNumber $ read (BL.unpack start)) (parser rest)
  | chr == '"' = parseString BL.empty (BL.tail bl)
  | Just res <- ident "true" (JBool True) bl = res
  | Just res <- ident "false" (JBool False) bl = res
  | Just res <- ident "null" JNull bl = res
  | otherwise = Failed
  where
    chr = BL.head bl




iterParser _ Failed = putStrLn "Failed"
iterParser [] (MoreData _) = putStrLn "No more data"
iterParser (dta:rest) (MoreData p) = iterParser rest (p dta)
iterParser dta (PartialResult e p) = do
  print e
  iterParser dta p

main :: IO ()
main = do
  let test = ["[1,2", "2,3,\"", "ond\\\"ra\"","t", "rue,fal", "se,[null]", "{\"ondra\":\"martin\", \"x\":5}", "]"]
  iterParser (tail test) (parser $ BL.fromChunks [head test])
