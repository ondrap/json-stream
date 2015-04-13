{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}

import qualified Data.Text as T
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy.Char8 as BL
import Data.Char (isSpace, isDigit, isAlpha, isDigit)
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
  | otherwise = Just $ moredata input

moredata :: BL.ByteString -> ParseResult
moredata bl = MoreData (\d -> parser (BL.append bl (BL.fromChunks [d])))

parseString :: BL.ByteString -> BL.ByteString -> ParseResult
parseString start bl
  | BL.null bl = MoreData (\d -> parseString start $ BL.fromChunks [d])
  | chr == '"' = PartialResult (JString (decodeUtf8 $ BS.concat $ BL.toChunks start)) (parser $ BL.tail bl)
  | chr == '\\' && BL.length bl == 1 = MoreData (\d -> parseString start $ bl `BL.append` BL.fromChunks [d])
  | chr == '\\' && sndchr == '"' = parseString (start `BL.append` "\"") (BL.drop 2 bl)
  | chr == '\\' && sndchr == '\\' = parseString (start `BL.append` "\\") (BL.drop 2 bl)
  | otherwise = parseString (BL.append start cont) rest
  where
    chr = BL.head bl
    sndchr = BL.index bl 1
    (cont, rest) = BL.break (\c -> c == '"' || c == '\\' ) bl

parser :: BL.ByteString -> ParseResult
parser bl
  | BL.null bl = MoreData (\d -> parser $ BL.fromChunks [d])
  | isSpace chr = parser (BL.dropWhile isSpace bl)
  | chr == '[' = PartialResult ArrayBegin (parser (BL.tail bl))
  | chr == ']' = PartialResult ArrayEnd (parser (BL.tail bl))
  | chr == '{' = PartialResult ObjectBegin (parser (BL.tail bl))
  | chr == '}' = PartialResult ObjectEnd (parser (BL.tail bl))
  | chr == ',' = parser (BL.tail bl)
  | isDigit chr =
        let (start, rest) = BL.span isDigit bl
        in if | BL.null rest -> moredata bl
              | otherwise -> PartialResult (JNumber $ read (BL.unpack start)) (parser rest)
  | chr == '"' = parseString BL.empty (BL.tail bl)
  | Just res <- ident "true" (JBool True) bl = res
  | Just res <- ident "false" (JBool False) bl = res
  | Just res <- ident "null" JNull bl = res
  | otherwise = Failed
  where
    chr = BL.head bl




iterParser _ Failed = putStrLn "Failed"
iterParser [] (MoreData p) = putStrLn "No more data"
iterParser (dta:rest) (MoreData p) = iterParser rest (p dta)
iterParser dta (PartialResult e p) = do
  print e
  iterParser dta p


main = do
  let test = ["[1,2", "2,3,\"", "ond\\\"ra\"","t", "rue,fal", "se,[null]", "]"]
  iterParser (tail test) (parser $ BL.fromChunks [head test])
