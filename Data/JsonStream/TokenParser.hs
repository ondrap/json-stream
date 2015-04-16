{-# LANGUAGE BangPatterns      #-}
{-# LANGUAGE MultiWayIf        #-}
{-# LANGUAGE OverloadedStrings #-}

module Data.JsonStream.TokenParser (
    Element(..)
  , TokenResult(..)
  , tokenParser
) where

import           Control.Applicative
import           Control.Monad         (replicateM, when, (>=>), void)
import qualified Data.Aeson            as AE
import qualified Data.ByteString       as BSW
import qualified Data.ByteString.Char8 as BS
import           Data.Char             (isDigit, isDigit, isLower, isSpace)
import           Data.Scientific       (scientific)
import qualified Data.Text             as T
import           Data.Text.Encoding    (decodeUtf8', encodeUtf8)

data Element = ArrayBegin | ArrayEnd | ObjectBegin | ObjectEnd
               | ObjectKey T.Text | JValue AE.Value
               deriving (Show, Eq)

-- Internal Interface for parsing monad
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
                    TokMoreData' cont context -> (TokMoreData' (cont >=> mpost) context, newstate)
                    PartialResult' el tokp context -> (PartialResult' el (tokp >>= mpost) context, newstate)
                    TokFailed' context -> (TokFailed' context, newstate)
                    Intermediate' result -> runTokParser (mpost result) newstate

instance Functor TokenResult' where
  fmap f (TokMoreData' newp ctx) = TokMoreData' (fmap f . newp) ctx
  fmap f (PartialResult' el tok ctx) = PartialResult' el (fmap f tok) ctx
  fmap _ (TokFailed' ctx) = TokFailed' ctx
  fmap f (Intermediate' a) = Intermediate' (f a)

instance Applicative TokenParser where
  pure = return
  f <*> param = do
    mf <- f
    mparam <- param
    return (mf mparam)

instance Functor TokenParser where
  fmap f tokp = TokenParser $ \s ->
              let (res, newstate) = runTokParser tokp s
              in (fmap f res, newstate)

failTok :: TokenParser a
failTok = TokenParser $ \s -> (TokFailed' (stContext s), s)

{-# INLINE isBreakChar #-}
isBreakChar :: Char -> Bool
isBreakChar c = isSpace c || (c == '{') || (c == '[') || (c == '}') || (c == ']') || (c == ',')

{-# INLINE peekChar #-}
peekChar :: TokenParser Char
peekChar = TokenParser handle
  where
    -- handle :: State -> (TokenResult' a, State)
    handle st@(State dta context)
      | BS.null dta = (TokMoreData' (\newdta -> TokenParser $ \_ -> handle (State newdta (BS.append context newdta)))
                                     context
                        , st)
      | otherwise   = (Intermediate' (BS.head dta), st)

{-# INLINE pickChar #-}
pickChar :: TokenParser Char
pickChar = TokenParser handle
  where
    handle st@(State dta context)
      | BS.null dta = (TokMoreData' (\newdta -> TokenParser $ \_ -> handle (State newdta (BS.append context newdta)))
                                     context
                        , st)
      | otherwise   = (Intermediate' (BS.head dta), State (BS.tail dta) context)

{-# INLINE yield #-}
yield :: Element -> TokenParser ()
yield el = TokenParser $ \state@(State dta ctx) -> (PartialResult' el (contparse dta) ctx, state)
  where
    -- Use data as new context
    contparse dta = TokenParser $ const (Intermediate' (), State dta dta )

-- | Return SOME input satisfying predicate or none, if the next element does not satisfy
{-# INLINE getWhile' #-}
getWhile' :: (Char -> Bool) -> TokenParser BS.ByteString
getWhile' predicate = do
  char <- peekChar
  if predicate char then getBuf
                    else return ""
  where
    getBuf = TokenParser $ \(State dta ctx) ->
        let (st,rest) = BS.span predicate dta
        in (Intermediate' st, State rest ctx)

-- | Read ALL input satisfying predicate
{-# INLINE getWhile #-}
getWhile :: (Char -> Bool) -> TokenParser BS.ByteString
getWhile predicate = loop []
  where
    loop acc = do
      dta <- getWhile' predicate
      if BS.null dta
        then return $ BS.concat $ reverse acc
        else loop (dta:acc)

-- | Parse unquoted identifier - true/false/null
parseIdent :: TokenParser ()
parseIdent = do
    ident <- getWhile isLower
    nextchar <- peekChar
    if | isBreakChar nextchar -> toTemp ident -- We found a barrier -> parse
       | otherwise -> failTok
  where
    toTemp "true" = yield $ JValue $ AE.Bool True
    toTemp "false" = yield $ JValue $ AE.Bool False
    toTemp "null" = yield $ JValue AE.Null
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
{-# INLINE chooseKeyOrValue #-}
chooseKeyOrValue :: T.Text -> TokenParser ()
chooseKeyOrValue text = do
  chr <- peekChar
  if | chr == ':' -> pickChar >> yield (ObjectKey text)
     | isSpace chr -> getWhile' isSpace >> chooseKeyOrValue text
     | otherwise -> yield $ JValue $ AE.String text

-- | Parse string, when finished check if we are object in dict (followed by :) or just a string
parseString :: TokenParser ()
parseString = do
    _ <- pickChar -- remove leading '"'
    firstpart <- getWhile' (\c -> c /= '"' && c /= '\\' )
    handleString [firstpart]
  where
    handleString acc = do
      chr <- peekChar
      case chr of
        '"' -> do
            _ <- pickChar
            case decodeUtf8' (BS.concat $ reverse acc) of
              Left _ -> failTok
              Right val -> chooseKeyOrValue val
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

parseNumber :: TokenParser ()
parseNumber = do
    tnumber <- getWhile (\c -> isDigit c || c == '.' || c == '+' || c == '-' || c == 'e' || c == 'E')
    let
      ([(texp, _), (frac, frdigits), (num, numdigits), (csign, _)], rest) =
              foldl parseStep ([], tnumber) [parseSign, parseDecimal, parseFract, parseE]
    when (numdigits == 0 || not (BS.null rest)) failTok

    let dpart = fromIntegral csign * (fromIntegral num * (10 ^ frdigits) + fromIntegral frac) :: Integer
        e = texp - frdigits
    yield $ JValue $ AE.Number $ scientific dpart e
  where
    parseStep :: ([(Int, Int)], BS.ByteString) -> (BS.ByteString -> ((Int, Int), BS.ByteString)) -> ([(Int, Int)], BS.ByteString)
    parseStep (lst, txt) f =
      let (newi, rest) = f txt
      in (newi:lst, rest)

    parseFract txt
      | BS.null txt = ((0, 0), txt)
      | BS.head txt == '.' = parseDecimal (BS.tail txt)
      | otherwise = ((0,0), txt)

    parseE txt
      | BS.null txt = ((0, 0), txt)
      | firstc == 'e' || firstc == 'E' =
              let ((sign, d1), rest) = parseSign (BS.tail txt)
                  ((dnum, d2), trest) = parseDecimal rest
              in ((dnum * sign, d1 + d2), trest)
      | otherwise = ((0,0), txt)
      where
        firstc = BS.head txt

    parseSign txt
      | BS.null txt = ((1, 0), txt)
      | BS.head txt == '+' = ((1, 1), BS.tail txt)
      | BS.head txt == '-' = ((-1, 1), BS.tail txt)
      | otherwise = ((1, 0), txt)

    parseDecimal txt
      | BS.null txt = ((0, 0), txt)
      | otherwise = parseNum txt (0,0)

    parseNum txt (!start, !digits)
      | BS.null txt = ((start, digits), txt)
      | dchr >= 48 && dchr <= 57 = parseNum (BS.tail txt) (start * 10 + fromIntegral (dchr - 48), digits + 1)
      | otherwise = ((start, digits), txt)
      where
        dchr = BSW.head txt

{-# INLINE peekCharInMain #-}
-- Specialized version of peek char for main function so that we get faster performance
peekCharInMain :: TokenParser Char
peekCharInMain = TokenParser handle
  where
    handle st@(State dta ctx)
      | BS.null dta = (TokMoreData' (\newdta -> TokenParser $ \_ -> handle (State newdta (BS.append ctx newdta)))
                                     ctx
                        , st)
      | chr == '[' = (PartialResult' ArrayBegin contparse ctx, st)
      | chr == ']' = (PartialResult' ArrayEnd contparse ctx, st)
      | chr == '{' = (PartialResult' ObjectBegin contparse ctx, st)
      | chr == '}' = (PartialResult' ObjectEnd contparse ctx, st)
      | chr == ',' || isSpace chr = handle (State (BS.dropWhile (\c -> c == ',' || isSpace c) ctx) ctx)
      | otherwise   = (Intermediate' (BS.head dta), st)
      where
        chr = BS.head dta
        rest = BS.tail dta
        -- Use data as new context
        contparse = TokenParser $ const $ handle (State rest rest)

{-# INLINE mainParser #-}
mainParser :: TokenParser ()
mainParser = do
  chr <- peekCharInMain
  case chr of
    '"' -> parseString
    't' -> parseIdent
    'f' -> parseIdent
    'n' -> parseIdent
    '-' -> parseNumber
    _| isDigit chr -> parseNumber
     | otherwise -> failTok

tokenParser :: BS.ByteString -> TokenResult
tokenParser dta = handle $ runTokParser mainParser (State dta dta)
  where
    handle (TokMoreData' ntp ctx, st) = TokMoreData (\ndta -> handle $ runTokParser (ntp ndta) st) ctx
    handle (PartialResult' el ntp ctx, st) = PartialResult el (handle $ runTokParser ntp st) ctx
    handle (TokFailed' ctx, _) = TokFailed ctx
    handle (Intermediate' _, st) = handle $ runTokParser mainParser st
