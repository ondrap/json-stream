{-# LANGUAGE TupleSections #-}
{-# LANGUAGE BangPatterns #-}

module Data.JsonStream.Parser (
    Parser
  , ParseOutput(..)
  , runParser
  , runParser'
  , parseByteString
  , parseLazyByteString

  , value
  , objectWithKey
  , objectItems
  , objectValues
  , array
  , arrayWithIndex
  , indexedArray

  , filterI
  , toList
  , defaultValue
  , catchFail
) where

import           Control.Applicative
import qualified Data.Aeson                  as AE
import qualified Data.ByteString             as BS
import qualified Data.ByteString.Lazy        as BL
import qualified Data.HashMap.Strict         as HMap
import qualified Data.Text                   as T
import qualified Data.Vector                 as Vec

import           Data.JsonStream.TokenParser

data ParseResult v =  MoreData (Parser v, BS.ByteString -> TokenResult)
                    | Failed String
                    | Done TokenResult
                    | Yield !v (ParseResult v)
                    | UnexpectedEnd Element TokenResult -- Thrown on ArrayEnd and ObjectEnd


instance Functor ParseResult where
  fmap f (MoreData (np, ntok)) = MoreData (fmap f np, ntok)
  fmap _ (Failed err) = Failed err
  fmap _ (Done tok) = Done tok
  fmap f (Yield v np) = Yield (f v) (fmap f np)
  fmap _ (UnexpectedEnd el tok) = UnexpectedEnd el tok

instance Functor Parser where
  fmap f (Parser p) = Parser $ \d -> fmap f (p d)

instance Applicative Parser where
  pure x = Parser $ \tok -> Yield x (callParse ignoreVal tok)
  -- | Run both parsers in parallel using a shared token parser, combine results
  (<*>) m1 m2 = Parser $ \tok -> process ([], []) (callParse m1 tok) (callParse m2 tok)
    where
      process (lst1, lst2) (Yield v np1) p2 = process (v:lst1, lst2) np1 p2
      process (lst1, lst2) p1 (Yield v np2) = process (lst1, v:lst2) p1 np2
      process _ (Failed err) _ = Failed err
      process _ _ (Failed err) = Failed err
      process (lst1, lst2) (Done ntok) (Done _) =
        yieldResults [ mx my | mx <- lst1, my <- lst2 ] (Done ntok)
      process (lst1, lst2) (UnexpectedEnd el ntok) (UnexpectedEnd _ _) =
        yieldResults [ mx my | mx <- lst1, my <- lst2 ] (UnexpectedEnd el ntok)
      process lsts (MoreData (np1, ntok1)) (MoreData (np2, _)) =
        MoreData (Parser (\tok -> process lsts (callParse np1 tok) (callParse np2 tok)), ntok1)
      process _ _ _ = Failed "Unexpected error in parallel processing <*>."

      yieldResults values end = foldr Yield end values


instance Alternative Parser where
  empty = ignoreVal
  -- | Run both parsers in parallel using a shared token parser, yielding from both as the data comes
  (<|>) m1 m2 = Parser $ \tok -> process (callParse m1 tok) (callParse m2 tok)
    where
      process (Done ntok) (Done _) = Done ntok
      process (Failed err) _ = Failed err
      process _ (Failed err) = Failed err
      process (Yield v np1) p2 = Yield v (process np1 p2)
      process p1 (Yield v np2) = Yield v (process p1 np2)
      process (MoreData (np1, ntok)) (MoreData (np2, _)) =
          MoreData (Parser $ \tok -> process (callParse np1 tok) (callParse np2 tok), ntok)
      process (UnexpectedEnd el ntok) (UnexpectedEnd _ _) = UnexpectedEnd el ntok
      process _ _ = error "Unexpected error in parallel processing <|>"

newtype Parser a = Parser {
    callParse :: TokenResult -> ParseResult a
}

array' :: (Int -> Parser a) -> Parser a
array' valparse = Parser $ \tp ->
  case tp of
    (PartialResult ArrayBegin ntp _) -> arrcontent 0 (callParse (valparse 0) ntp)
    (PartialResult el ntp _)
      | el == ArrayEnd || el == ObjectEnd -> UnexpectedEnd el ntp
      | otherwise -> Failed ("Array - unexpected token: " ++ show el)
    (TokMoreData ntok _) -> MoreData (array' valparse, ntok)
    (TokFailed _) -> Failed "Array - token failed"
  where
    arrcontent i (Done ntp) = arrcontent (i+1) (callParse (valparse (i + 1)) ntp) -- Reset to next value
    arrcontent i (MoreData (Parser np, ntp)) = MoreData (Parser (arrcontent i . np), ntp)
    arrcontent i (Yield v np) = Yield v (arrcontent i np)
    arrcontent _ (Failed err) = Failed err
    arrcontent _ (UnexpectedEnd ArrayEnd ntp) = Done ntp
    arrcontent _ (UnexpectedEnd el _) = Failed ("Array - UnexpectedEnd: " ++ show el)

-- | Match all items of an array
array :: Parser a -> Parser a
array valparse = array' (const valparse)

-- | Match n'th item of an array
arrayWithIndex :: Int -> Parser a -> Parser a
arrayWithIndex idx valparse = array' itemFn
  where
    itemFn aidx
      | aidx == idx = valparse
      | otherwise = ignoreVal

-- | Match all items of an array, add index to output
indexedArray :: Parser a -> Parser (Int, a)
indexedArray valparse = array' (\(!key) -> (key,) <$> valparse)

object' :: (T.Text -> Parser a) -> Parser a
object' valparse = Parser $ \tp ->
  case tp of
    (PartialResult ObjectBegin ntp _) -> objcontent (keyValue ntp)
    (PartialResult el ntp _)
      | el == ArrayEnd || el == ObjectEnd -> UnexpectedEnd el ntp
      | otherwise -> Failed ("Object - unexpected token: " ++ show el)
    (TokMoreData ntok _) -> MoreData (object' valparse, ntok)
    (TokFailed _) -> Failed "Object - token failed"
  where
    objcontent (Done ntp) = objcontent (keyValue ntp) -- Reset to next value
    objcontent (MoreData (Parser np, ntok)) = MoreData (Parser (objcontent . np), ntok)
    objcontent (Yield v np) = Yield v (objcontent np)
    objcontent (Failed err) = Failed err
    objcontent (UnexpectedEnd ObjectEnd ntp) = Done ntp
    objcontent (UnexpectedEnd el _) = Failed ("Object - UnexpectedEnd: " ++ show el)

    keyValue (TokFailed _) = Failed "KeyValue - token failed"
    keyValue (TokMoreData ntok _) = MoreData (Parser keyValue, ntok)
    keyValue (PartialResult (ObjectKey key) ntok _) = callParse (valparse key) ntok
    keyValue (PartialResult el ntok _)
      | el == ArrayEnd || el == ObjectEnd = UnexpectedEnd el ntok
      | otherwise = Failed ("Array - unexpected token: " ++ show el)


-- | Match all key-value pairs of an object, return them as a tuple
objectItems :: Parser a -> Parser (T.Text, a)
objectItems valparse = object' $ \(!key) -> (key,) <$> valparse

-- | Match all key-value pairs of an object, return only values
objectValues :: Parser a -> Parser a
objectValues valparse = object' (const valparse)

-- | Match only specific key of an object
objectWithKey :: T.Text -> Parser a -> Parser a
objectWithKey name valparse = object' itemFn
  where
    itemFn key
      | key == name = valparse
      | otherwise = ignoreVal

-- | Parses underlying values and generates a AE.Value
aeValue :: Parser AE.Value
aeValue = Parser value'
  where
    value' (TokFailed _) = Failed "Value - token failed"
    value' (TokMoreData ntok _) = MoreData (Parser value', ntok)
    value' (PartialResult (JValue val) ntok _) = Yield val (Done ntok)
    value' tok@(PartialResult ArrayBegin _ _) =
        AE.Array . Vec.fromList <$> callParse (toList (array value)) tok
    value' tok@(PartialResult ObjectBegin _ _) =
        AE.Object . HMap.fromList <$> callParse (toList (objectItems value)) tok
    value' (PartialResult el ntok _)
      | el == ArrayEnd || el == ObjectEnd = UnexpectedEnd el ntok
      | otherwise = Failed ("aeValue - unexpected token: " ++ show el)

-- | Convert a value fromjson, fail with Failed if it doesn't work
value :: AE.FromJSON a => Parser a
value = Parser $ \ntok -> loop (callParse aeValue ntok)
  where
    loop (Done ntp) = Done ntp
    loop (Failed err) = Failed err
    loop (UnexpectedEnd el b) = UnexpectedEnd el b
    loop (MoreData (Parser np, ntok)) = MoreData (Parser (loop . np), ntok)
    loop (Yield v np) =
      case AE.fromJSON v of
        AE.Error err -> Failed err
        AE.Success res -> Yield res (loop np)

-- | Skip value; cheat to avoid parsing and make it faster
ignoreVal :: Parser a
ignoreVal = Parser $ handleTok 0
  where
    handleTok :: Int -> TokenResult -> ParseResult a
    handleTok _ (TokFailed _) = Failed "Token error"
    handleTok level (TokMoreData ntok _) = MoreData (Parser (handleTok level), ntok)

    handleTok 0 (PartialResult (JValue _) ntok _) = Done ntok
    handleTok 0 (PartialResult (ObjectKey _) ntok _) = Done ntok
    handleTok 0 (PartialResult elm ntok _)
      | elm == ArrayEnd || elm == ObjectEnd = UnexpectedEnd elm ntok
    handleTok level (PartialResult (JValue _) ntok _) = handleTok level ntok
    handleTok level (PartialResult (ObjectKey _) ntok _) = handleTok level ntok

    handleTok 1 (PartialResult elm ntok _)
      | elm == ArrayEnd || elm == ObjectEnd = Done ntok
    handleTok level (PartialResult elm ntok _)
      | elm == ArrayBegin || elm == ObjectBegin = handleTok (level + 1) ntok
      | elm == ArrayEnd || elm == ObjectEnd = handleTok (level - 1) ntok
    handleTok _ _ = Failed "UnexpectedEnd "

-- | Fetch yields of a function and return them as list
toList :: Parser a -> Parser [a]
toList f = Parser $ \ntok -> loop [] (callParse f ntok)
  where
    loop acc (Done ntp) = Yield (reverse acc) (Done ntp)
    loop acc (MoreData (Parser np, ntok)) = MoreData (Parser (loop acc . np), ntok)
    loop acc (Yield v np) = loop (v:acc) np
    loop _ (Failed err) = Failed err
    loop _ (UnexpectedEnd el _) = Failed ("getYields - UnexpectedEnd: " ++ show el)

-- | Let only items matching a condition pass
filterI :: (a -> Bool) -> Parser a -> Parser a
filterI cond valparse = Parser $ \ntok -> loop (callParse valparse ntok)
  where
    loop (Done ntp) = Done ntp
    loop (Failed err) = Failed err
    loop (UnexpectedEnd el b) = UnexpectedEnd el b
    loop (MoreData (Parser np, ntok)) = MoreData (Parser (loop . np), ntok)
    loop (Yield v np)
      | cond v = Yield v (loop np)
      | otherwise = loop np

-- | Returns a value if none is found upstream
defaultValue :: a -> Parser a -> Parser a
defaultValue defvalue valparse = Parser $ \ntok -> loop False (callParse valparse ntok)
  where
    loop True (Done ntp) = Done ntp
    loop False (Done ntp) = Yield defvalue (Done ntp)
    loop _ (Failed err) = Failed err
    loop _ (UnexpectedEnd el b) = UnexpectedEnd el b
    loop found (MoreData (Parser np, ntok)) = MoreData (Parser (loop found . np), ntok)
    loop _ (Yield v np) = Yield v (loop True np)

-- | Tries to catch an error in underlying parser
catchFail :: Parser a -> Parser a
catchFail valparse = Parser $ \tok -> process (callParse valparse tok) (callParse ignoreVal tok)
  where -- Call ignoreVal in parallel, switch to it if the first parser fails
    process (Yield v np1) p2 = Yield v (process np1 p2)
    process _ (Failed err) = Failed err
    process (Done ntok) _ = Done ntok
    process _ (Done ntok) = Done ntok
    process (UnexpectedEnd el ntok) _ = UnexpectedEnd el ntok
    process _ (UnexpectedEnd el ntok) = UnexpectedEnd el ntok
    process (MoreData (np1, ntok1)) (MoreData (np2, _)) =
        MoreData (Parser (\tok -> process (callParse np1 tok) (callParse np2 tok)), ntok1)
    process p1@(Failed _) (MoreData (np2, ntok2)) =
        MoreData (Parser (\tok -> process p1 (callParse np2 tok)), ntok2)
    process _ _ = Failed "Unexpected error in parallel processing catchFail."

data ParseOutput a = ParseYield a (ParseOutput a)
                    | ParseNeedData (BS.ByteString -> ParseOutput a)
                    | ParseFailed String
                    | ParseDone BS.ByteString

-- | Run streaming parser with initial input
runParser' :: Parser a -> BS.ByteString -> ParseOutput a
runParser' parser startdata = parse $ callParse parser (tokenParser startdata)
  where
    parse (MoreData (np, ntok)) = ParseNeedData (parse . callParse np .ntok)
    parse (Failed err) = ParseFailed err
    parse (UnexpectedEnd el _) = ParseFailed $ "UnexpectedEnd item: " ++ show el
    parse (Yield v np) = ParseYield v (parse np)
    parse (Done (PartialResult _ _ rest)) = ParseDone rest
    parse (Done (TokFailed rest)) = ParseDone rest
    parse (Done (TokMoreData _ rest)) = ParseDone rest

-- | Run streaming parser, immediately returns ParseMoreData
runParser :: Parser a -> ParseOutput a
runParser parser = runParser' parser BS.empty

-- | Parse a bytestring, generate lazy list of parsed values. If an error occurs, throws an exception.
parseByteString :: Parser a -> BS.ByteString -> [a]
parseByteString parser startdata = loop (runParser' parser startdata)
  where
    loop (ParseNeedData _) = error "Not enough data."
    loop (ParseDone _) = []
    loop (ParseFailed err) = error err
    loop (ParseYield v np) = v : loop np

-- | Parse a lazy bytestring, generate lazy list of parsed values. If an error occurs, throws an exception.
parseLazyByteString :: Parser a -> BL.ByteString -> [a]
parseLazyByteString parser input = loop chunks (runParser parser)
  where
    chunks = BL.toChunks input
    loop [] (ParseNeedData _) = error "Not enough data."
    loop (dta:rest) (ParseNeedData np) = loop rest (np dta)
    loop _ (ParseDone _) = []
    loop _ (ParseFailed err) = error err
    loop rest (ParseYield v np) = v : loop rest np
