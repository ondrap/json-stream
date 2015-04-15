{-# LANGUAGE MultiWayIf        #-}
{-# LANGUAGE OverloadedStrings #-}

module Data.JsonStream.TokenParser (
    Element(..)
  , JValue(..)
  , TokenResult(..)
  , tokenParser
) where

import           Control.Monad            (replicateM, when)
import qualified Data.ByteString.Char8    as BS
import           Data.Char                (isDigit, isDigit, isHexDigit,
                                           isLower, isSpace)
import qualified Data.Text                as T
import           Data.Text.Encoding       (decodeUtf8With, encodeUtf8)
import           Data.Text.Encoding.Error (lenientDecode)

data JValue = JString T.Text | JNumber Int | JBool Bool | JNull
              | JArray [JValue] | JObject [(T.Text, JValue)] deriving (Show, Eq)

data Element = ArrayBegin | ArrayEnd | ObjectBegin | ObjectEnd
               | ObjectKey T.Text | JValue JValue
               deriving (Show, Eq)

data TokenResult' a =  TokMoreData' (BS.ByteString -> TokenParser a) BS.ByteString
                 | PartialResult' Element (TokenParser a) BS.ByteString
                 -- ^ found element, continuation, actual parsing view - so that we can report the unparsed
                 -- data when the parsing finishes.
                 | TokFailed' BS.ByteString
                 | Intermediate' a


-- | Public interface for parsing JSON tokens.
data TokenResult =  TokMoreData (BS.ByteString -> TokenResult) BS.ByteString
                  | PartialResult Element (TokenResult) BS.ByteString
                  -- ^ found element, continuation, actual parsing view - so that we can report the unparsed
                  -- data when the parsing finishes.
                  | TokFailed BS.ByteString

-- For debugging purposes
instance Show TokenResult where
  show (TokMoreData _ ctx) = "(TokMoreData' + " ++ show ctx ++ ")"
  show (TokFailed _) = "TokFailed'"
  show (PartialResult el _ rest) = "(PartialResult' " ++ show el ++ " " ++ show rest ++ ")"

data State = State {
    stData    :: BS.ByteString
  , stContext :: BS.ByteString
}

newtype TokenParser a = TokenParser {
    runTokParser :: State -> (TokenResult' a, State)
}

instance Monad TokenParser where
  return x = TokenParser $ \s -> (Intermediate' x, s)
  m >>= mpost = TokenParser $ \s ->
                let (res, newstate) = runTokParser m s
                in case res of
                    TokMoreData' cont context -> (TokMoreData' (\dta -> (cont dta >>= mpost)) context, newstate)
                    PartialResult' el tokp context -> (PartialResult' el (tokp >>= mpost) context, newstate)
                    TokFailed' context -> (TokFailed' context, newstate)
                    Intermediate' result -> runTokParser (mpost result) newstate

failTok :: TokenParser a
failTok = TokenParser $ \s -> (TokFailed' (stContext s), s)

isBreakChar :: Char -> Bool
isBreakChar c = isSpace c || (c == '{') || (c == '[') || (c == '}') || (c == ']') || (c == ',')

peekChar :: TokenParser Char
peekChar = TokenParser handle
  where
    handle st@(State dta context)
      | BS.null dta = (TokMoreData' (\newdta -> TokenParser $ \_ -> runTokParser peekChar (State newdta (BS.append context newdta))) context, st)
      | otherwise   = (Intermediate' (BS.head dta), st)

pickChar :: TokenParser Char
pickChar = do
    chr <- peekChar
    dropchar
    return chr
  where
    dropchar = TokenParser $ \s -> (Intermediate' (), s{stData=BS.tail (stData s)})

yield :: Element -> TokenParser ()
yield el = TokenParser $ \state@(State dta ctx) -> (PartialResult' el (contparse dta) ctx, state)
  where
    -- Use data as new context
    contparse dta = TokenParser $ const (Intermediate' (), State dta dta )

-- | Return some elements satisfying predicate or none, if the next element does not satisfy
getWhile :: (Char -> Bool) -> TokenParser BS.ByteString
getWhile predicate = do
  char <- peekChar
  if predicate char then getWhile'
                    else return ""
  where
    getWhile' = TokenParser $ \(State dta ctx) ->
        let (st,rest) = BS.span predicate dta
        in (Intermediate' st, State rest ctx)

-- | Parse unquoted identifier - true/false/null
parseIdent :: TokenParser ()
parseIdent = loop ""
  where
    loop ident = do
      when (BS.length ident > 5) $ failTok
      dta <- getWhile isLower
      let newident = ident `BS.append` dta
      nextchar <- peekChar
      if | isBreakChar nextchar -> toTemp newident -- We found a barrier -> parse
         | BS.null dta -> failTok -- No new data? obviously next char is not lower/break
         | otherwise -> loop newident

    toTemp "true" = yield $ JValue $ JBool True
    toTemp "false" = yield $ JValue $ JBool False
    toTemp "null" = yield $ JValue JNull
    toTemp _ = failTok

parseUnicode :: TokenParser Char
parseUnicode = do
    lst <- replicateM 4 pickChar
    return $ toEnum $ foldl1 (\a b -> 16 * a + b) $ map hexCharToInt lst
  where
    hexCharToInt :: Char -> Int
    hexCharToInt c
      | c >= 'A' && c <= 'F' = 10 + (fromEnum c - fromEnum 'A')
      | c >= 'a' && c <= 'f' = 10 + (fromEnum c - fromEnum 'a')
      | isDigit c = fromEnum c - fromEnum '0'
      | otherwise = error "Incorrect hex input, internal error."

--
-- Choose if this is object key based on next character
chooseKeyOrValue :: T.Text -> TokenParser ()
chooseKeyOrValue text = do
  _ <- getWhile isSpace
  chr <- peekChar
  if | isSpace chr -> chooseKeyOrValue text
     | chr == ':' -> yield $ ObjectKey text
     | otherwise -> yield $ JValue $ JString text

-- | Parse string, when finished check if we are object in dict (followed by :) or just a string
parseString :: TokenParser ()
parseString = do
    _ <- pickChar
    handleString []
  where
    handleString acc = do
      chr <- peekChar
      case chr of
        '"' -> do
            _ <- pickChar
            chooseKeyOrValue $ decodeUtf8With lenientDecode $ BS.concat $ reverse acc
        '\\' -> do
            _ <- pickChar
            specchr <- pickChar
            nchr <- parseSpecChar specchr
            handleString (encodeUtf8 (T.singleton nchr):acc)
        _ -> do
          dstr <- getWhile (\c -> c /= '"' && c /= '\\' )
          handleString (dstr:acc)

    parseSpecChar '"' = return '"'
    parseSpecChar '\\' = return '\\'
    parseSpecChar '/' = return '/'
    parseSpecChar 'b' = return '\b'
    parseSpecChar 'f' = return '\f'
    parseSpecChar 'n' = return '\n'
    parseSpecChar 'r' = return '\r'
    parseSpecChar 't' = return '\t'
    parseSpecChar 'u' = parseUnicode
    parseSpecChar c = return c


mainParser :: TokenParser ()
mainParser = do
  _ <- getWhile isSpace
  chr <- peekChar
  if | chr == '[' -> pickChar >> yield ArrayBegin
     | chr == ']' -> pickChar >> yield ArrayEnd
     | chr == '{' -> pickChar >> yield ObjectBegin
     | chr == '}' -> pickChar >> yield ObjectEnd
     | chr == ',' -> pickChar >> mainParser
--  | isDigit chr || chr == '-' = parseNumber bl
     | chr == '"' -> parseString
     | chr == 't' || chr == 'f' || chr == 'n'-> parseIdent
     | otherwise -> failTok

tokenParser :: BS.ByteString -> TokenResult
tokenParser dta = handle $ runTokParser mainParser (State dta dta)
  where
    handle (TokMoreData' ntp ctx, st) = TokMoreData (\ndta -> handle $ runTokParser (ntp ndta) st) ctx
    handle (PartialResult' el ntp ctx, st) = PartialResult el (handle $ runTokParser ntp st) ctx
    handle (TokFailed' ctx, _) = TokFailed ctx
    handle (Intermediate' _, st) = handle $ runTokParser mainParser st
