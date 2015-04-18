{-# LANGUAGE BangPatterns  #-}
{-# LANGUAGE TupleSections #-}

-- |
-- Module : Data.JsonStream.Parser
-- License     : BSD-style
--
-- Maintainer  : palkovsky.ondrej@gmail.com
-- Stability   : experimental
-- Portability : portable
--
-- An incremental applicative-style JSON parser, suitable for high performance
-- memory efficient stream parsing.
--
-- The parser is using "Data.Aeson" types and 'FromJSON' instance, it can be
-- easily combined with aeson monadic parsing instances when appropriate.

module Data.JsonStream.Parser (
    -- * How to use this library
    -- $use

    -- * Constant space decoding
    -- $constant

    -- * Aeson compatibility
    -- $aeson

    -- * The @Parser@ type
    Parser
  , ParseOutput(..)
    -- * Parsing functions
  , runParser
  , runParser'
  , parseByteString
  , parseLazyByteString
    -- * FromJSON parser
  , value
  , string
    -- * Constant space parsers
  , boundedString
  , number
  , integer
  , real
  , bool
  , jNull
    -- * Convenience aeson-like operators
  , (.:)
  , (.:?)
  , (.!=)
    -- * Structure parsers
  , objectWithKey
  , objectItems
  , objectValues
  , arrayOf
  , arrayWithIndex
  , indexedArray
  , nullable
    -- * Parsing modifiers
  , defaultValue
  , filterI
  , takeI
  , toList
) where

import           Control.Applicative
import qualified Data.Aeson                  as AE
import qualified Data.ByteString             as BS
import qualified Data.ByteString.Lazy        as BL
import qualified Data.HashMap.Strict         as HMap
import           Data.Scientific             (Scientific, isInteger,
                                              toBoundedInteger, toRealFloat)
import qualified Data.Text                   as T
import Data.Text.Encoding (decodeUtf8')
import qualified Data.Vector                 as Vec
import Data.Maybe (fromMaybe)

import           Data.JsonStream.TokenParser

-- | Private parsing result
data ParseResult v =  MoreData (Parser v, BS.ByteString -> TokenResult)
                    | Failed String
                    | Done TokenResult
                    | Yield v (ParseResult v)
                    | UnexpectedEnd Element TokenResult -- Thrown on ArrayEnd and ObjectEnd


instance Functor ParseResult where
  fmap f (MoreData (np, ntok)) = MoreData (fmap f np, ntok)
  fmap _ (Failed err) = Failed err
  fmap _ (Done tok) = Done tok
  fmap f (Yield v np) = Yield (f v) (fmap f np)
  fmap _ (UnexpectedEnd el tok) = UnexpectedEnd el tok

-- | A representation of the parser.
newtype Parser a = Parser {
    callParse :: TokenResult -> ParseResult a
}

instance Functor Parser where
  fmap f (Parser p) = Parser $ \d -> fmap f (p d)

instance Applicative Parser where
  pure x = Parser $ \tok -> process (callParse ignoreVal tok)
    where
      process (Failed err) = Failed err
      process (Done tok) = Yield x (Done tok)
      process (UnexpectedEnd el tok) = UnexpectedEnd el tok
      process (MoreData (np, ntok)) = MoreData (Parser (process . callParse np), ntok)
      process _ = Failed "Internal error in pure, ignoreVal doesn't yield"

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

array' :: (Int -> Parser a) -> Parser a
array' valparse = Parser $ \tp ->
  case tp of
    (PartialResult ArrayBegin ntp _) -> arrcontent 0 (callParse (valparse 0) ntp)
    (PartialResult el ntp _)
      | el == ArrayEnd || el == ObjectEnd -> UnexpectedEnd el ntp
      | otherwise -> callParse ignoreVal tp -- Run ignoreval parser on the same output we got
    (TokMoreData ntok _) -> MoreData (array' valparse, ntok)
    (TokFailed _) -> Failed "Array - token failed"
  where
    arrcontent i (Done ntp) = arrcontent (i+1) (callParse (valparse (i + 1)) ntp) -- Reset to next value
    arrcontent i (MoreData (Parser np, ntp)) = MoreData (Parser (arrcontent i . np), ntp)
    arrcontent i (Yield v np) = Yield v (arrcontent i np)
    arrcontent _ (Failed err) = Failed err
    arrcontent _ (UnexpectedEnd ArrayEnd ntp) = Done ntp
    arrcontent _ (UnexpectedEnd el _) = Failed ("Array - UnexpectedEnd: " ++ show el)

-- | Match all items of an array.
arrayOf :: Parser a -> Parser a
arrayOf valparse = array' (const valparse)

-- | Match n'th item of an array.
arrayWithIndex :: Int -> Parser a -> Parser a
arrayWithIndex idx valparse = array' itemFn
  where
    itemFn aidx
      | aidx == idx = valparse
      | otherwise = ignoreVal

-- | Match all items of an array, add index to output.
indexedArray :: Parser a -> Parser (Int, a)
indexedArray valparse = array' (\(!key) -> (key,) <$> valparse)


-- | Go through an object; if once is True, yield only first success, then ignore the rest
object' :: Bool -> (T.Text -> Parser a) -> Parser a
object' once valparse = Parser $ \tp ->
  case tp of
    PartialResult ObjectBegin ntp _ -> objcontent False (keyValue ntp)
    PartialResult el ntp _
      | el == ArrayEnd || el == ObjectEnd -> UnexpectedEnd el ntp
      | otherwise -> callParse ignoreVal tp -- Run ignoreval parser on the same output we got
    TokMoreData ntok _ -> MoreData (object' once valparse, ntok)
    TokFailed _ -> Failed "Object - token failed"
  where
    -- If we already yielded and should yield once, ignore the rest
    objcontent yielded (Done ntp)
      | once && yielded = callParse (ignoreVal' 1) ntp
      | otherwise = objcontent yielded (keyValue ntp) -- Reset to next value
    objcontent yielded (MoreData (Parser np, ntok)) = MoreData (Parser (objcontent yielded. np), ntok)
    objcontent _ (Yield v np) = Yield v (objcontent True np)
    objcontent _ (Failed err) = Failed err
    objcontent _ (UnexpectedEnd ObjectEnd ntp) = Done ntp
    objcontent _ (UnexpectedEnd el _) = Failed ("Object - UnexpectedEnd: " ++ show el)

    keyValue (TokFailed _) = Failed "KeyValue - token failed"
    keyValue (TokMoreData ntok _) = MoreData (Parser keyValue, ntok)
    keyValue (PartialResult (JValue (AE.String key)) ntok _) = callParse (valparse key) ntok
    keyValue (PartialResult (StringBegin str) ntok _) = getLongString [str] (BS.length str) ntok
    keyValue (PartialResult el ntok _)
      | el == ArrayEnd || el == ObjectEnd = UnexpectedEnd el ntok
      | otherwise = Failed ("Object - unexpected token: " ++ show el)

    getLongString acc !len tok =
      case tok of
        PartialResult StringEnd ntok _
          | Right key <- decodeUtf8' (BS.concat $ reverse acc) -> callParse (valparse key) ntok
          | otherwise -> Failed "Error decoding UTF8"
        PartialResult (StringContent str) ntok _ -> getLongString (str:acc) (len + BS.length str) ntok
        TokMoreData ntok _ -> MoreData (Parser (getLongString acc len), ntok)
        _ -> Failed "Object longstr - unexpected token."


-- | Match all key-value pairs of an object, return them as a tuple.
-- If the source object defines same key multiple times, all values
-- are matched.
objectItems :: Parser a -> Parser (T.Text, a)
objectItems valparse = object' False $ \(!key) -> (key,) <$> valparse

-- | Match all key-value pairs of an object, return only values.
-- If the source object defines same key multiple times, all values
-- are matched.
objectValues :: Parser a -> Parser a
objectValues valparse = object' False (const valparse)

-- | Match only specific key of an object.
-- This function will return only the first matched value in an object even
-- if the source JSON defines the key multiple times (in violation of the specification).
objectWithKey :: T.Text -> Parser a -> Parser a
objectWithKey name valparse = object' True itemFn
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
    value' tok@(PartialResult (StringBegin _) _ _) = callParse (AE.String <$> longString Nothing) tok
    value' tok@(PartialResult ArrayBegin _ _) =
        AE.Array . Vec.fromList <$> callParse (toList (arrayOf aeValue)) tok
    value' tok@(PartialResult ObjectBegin _ _) =
        AE.Object . HMap.fromList <$> callParse (toList (objectItems aeValue)) tok
    value' (PartialResult el ntok _)
      | el == ArrayEnd || el == ObjectEnd = UnexpectedEnd el ntok
      | otherwise = Failed ("aeValue - unexpected token: " ++ show el)

-- | Convert a strict aeson value (no object/array) to a value.
-- Non-matching type is ignored and not parsed (unlike 'value')
jvalue :: (AE.Value -> Maybe a) -> Parser a
jvalue convert = Parser value'
  where
    value' (TokFailed _) = Failed "Value - token failed"
    value' (TokMoreData ntok _) = MoreData (Parser value', ntok)
    value' (PartialResult (JValue val) ntok _) =
          case convert val of
            Just convValue -> Yield convValue (Done ntok)
            Nothing -> Done ntok
    value' tp@(PartialResult el ntok _)
      | el == ArrayEnd || el == ObjectEnd = UnexpectedEnd el ntok
      | otherwise = callParse ignoreVal tp


-- | Match a possibly bounded string roughly limited by a limit
longString :: Maybe Int -> Parser T.Text
longString mbounds = Parser $ handle [] 0
  where
    handle acc !len tp =
      case tp of
        PartialResult (JValue (AE.String str)) ntok _ -> Yield str (Done ntok)
        PartialResult (StringBegin str) ntp _ -> handle [str] (BS.length str) ntp
        PartialResult (StringContent str) ntp _
          | (Just bounds) <- mbounds,
            len > bounds             -> handle acc len ntp
          | otherwise -> handle (str:acc) (len + BS.length str) ntp
        PartialResult StringEnd ntp _
          | Right val <- decodeUtf8' (BS.concat $ reverse acc) -> Yield val (Done ntp)
          | otherwise -> Failed "Error decoding UTF8"
        tok@(PartialResult {}) -> callParse ignoreVal tok
        TokMoreData ntok _ -> MoreData (Parser (handle acc len), ntok)
        TokFailed _ -> Failed "String - token failed"

-- | Parse string value, skip parsing otherwise.
string :: Parser T.Text
string = longString Nothing

-- | Stops parsing string after the limit is reached. The returned
-- string will be longer  than specified limit, but new chunks will
-- not be added to the string.
boundedString :: Int -> Parser T.Text
boundedString limit = longString (Just limit)

-- | Parse number, return in scientific format.
number :: Parser Scientific
number = jvalue cvt
  where
    cvt (AE.Number num) = Just num
    cvt _ = Nothing

-- | Parse to integer type.
integer :: (Integral i, Bounded i) => Parser i
integer = jvalue cvt
  where
    cvt (AE.Number num)
      | isInteger num = toBoundedInteger num
    cvt _ = Nothing

-- | Parse to float/double.
real :: RealFloat a => Parser a
real = jvalue cvt
  where
    cvt (AE.Number num) = Just $ toRealFloat num
    cvt _ = Nothing

-- | Parse bool, skip if the type is not bool.
bool :: Parser Bool
bool = jvalue cvt
  where
    cvt (AE.Bool b) = Just b
    cvt _ = Nothing

-- | Match a null value.
jNull :: Parser ()
jNull = jvalue cvt
  where
    cvt (AE.Null) = Just ()
    cvt _ = Nothing

-- | Parses a field with a possible null value. Use 'defaultValue' for missing values.
nullable :: Parser a -> Parser (Maybe a)
nullable valparse = Parser value'
  where
    value' (TokFailed _) = Failed "Nullable - token failed"
    value' (TokMoreData ntok _) = MoreData (Parser value', ntok)
    value' (PartialResult (JValue AE.Null) ntok _) = Yield Nothing (Done ntok)
    value' tok@(PartialResult {}) = callParse (Just <$> valparse) tok


-- | Match 'FromJSON' value.
value :: AE.FromJSON a => Parser a
value = Parser $ \ntok -> loop (callParse aeValue ntok)
  where
    loop (Done ntp) = Done ntp
    loop (Failed err) = Failed err
    loop (UnexpectedEnd el b) = UnexpectedEnd el b
    loop (MoreData (Parser np, ntok)) = MoreData (Parser (loop . np), ntok)
    loop (Yield v np) =
      case AE.fromJSON v of
        AE.Error _ -> loop np
        AE.Success res -> Yield res (loop np)

-- | Take maximum n matching items.
takeI :: Int -> Parser a -> Parser a
takeI num valparse = Parser $ \tok -> loop num (callParse valparse tok)
  where
    loop _ (Done ntp) = Done ntp
    loop _ (Failed err) = Failed err
    loop _ (UnexpectedEnd el b) = UnexpectedEnd el b
    loop n (MoreData (Parser np, ntok)) = MoreData (Parser (loop n . np), ntok)
    loop 0 (Yield _ np) = loop 0 np
    loop n (Yield v np) = Yield v (loop (n-1) np)

-- | Skip value; cheat to avoid parsing and make it faster
ignoreVal :: Parser a
ignoreVal = ignoreVal' 0

ignoreVal' :: Int -> Parser a
ignoreVal' stval = Parser $ handleTok stval
  where
    handleTok :: Int -> TokenResult -> ParseResult a
    handleTok _ (TokFailed _) = Failed "Token error"
    handleTok level (TokMoreData ntok _) = MoreData (Parser (handleTok level), ntok)

    handleTok 0 (PartialResult (JValue _) ntok _) = Done ntok
    handleTok 0 (PartialResult elm ntok _)
      | elm == ArrayEnd || elm == ObjectEnd = UnexpectedEnd elm ntok
    handleTok level (PartialResult (JValue _) ntok _) = handleTok level ntok

    handleTok 1 (PartialResult elm ntok _)
      | elm == ArrayEnd || elm == ObjectEnd || elm == StringEnd = Done ntok
    handleTok level (PartialResult elm ntok _)
      | elm == ArrayBegin || elm == ObjectBegin = handleTok (level + 1) ntok
      | elm == ArrayEnd || elm == ObjectEnd = handleTok (level - 1) ntok
    handleTok level (PartialResult (StringBegin _) ntok _) = handleTok (level + 1) ntok
    handleTok level (PartialResult StringEnd ntok _) = handleTok (level - 1) ntok
    handleTok level (PartialResult (StringContent _) ntok _) = handleTok level ntok
    handleTok _ _ = Failed "UnexpectedEnd "

-- | Fetch yields of a function and return them as list.
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

-- | Returns a value if none is found upstream.
defaultValue :: a -> Parser a -> Parser a
defaultValue defvalue valparse = Parser $ \ntok -> loop False (callParse valparse ntok)
  where
    loop True (Done ntp) = Done ntp
    loop False (Done ntp) = Yield defvalue (Done ntp)
    loop _ (Failed err) = Failed err
    loop _ (UnexpectedEnd el b) = UnexpectedEnd el b
    loop found (MoreData (Parser np, ntok)) = MoreData (Parser (loop found . np), ntok)
    loop _ (Yield v np) = Yield v (loop True np)

--- Convenience operators

-- | Synonym to 'objectWithKey'.
(.:) :: T.Text -> Parser a -> Parser a
(.:) = objectWithKey

-- | Returns 'Nothing' if value is null or does not exist or match. Otherwise returns 'Just' value.
--
-- > key .:? val = defaultValue Nothing (key .: nullable val)
(.:?) :: T.Text -> Parser a -> Parser (Maybe a)
key .:? val = defaultValue Nothing (key .: nullable val)

-- | Converts 'Maybe' parser into normal one by providing default value instead of 'Nothing'.
--
-- > nullval .!= defval = fromMaybe defval <$> nullval
(.!=) :: Parser (Maybe a) -> a -> Parser a
nullval .!= defval = fromMaybe defval <$> nullval
---

-- | Result of parsing. Contains continuations to continue parsing.
data ParseOutput a = ParseYield a (ParseOutput a) -- ^ Returns a value from a parser.
                    | ParseNeedData (BS.ByteString -> ParseOutput a) -- ^ Parser needs more data to continue parsing.
                    | ParseFailed String -- ^ Parsing failed, error is reported.
                    | ParseDone BS.ByteString -- ^ Parsing finished, unparsed data is returned.

-- | Run streaming parser with initial input.
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

-- | Run streaming parser, immediately returns 'ParseNeedData'.
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


-- $use
--
-- > >>> parseByteString value "[1,2,3]" :: [[Int]]
-- > [[1,2,3]]
-- The 'value' parser matches any 'AE.FromJSON' value. The above command is essentially
-- identical to the aeson decode function; the parsing process can generate more
-- objects, therefore the results is [a].
--
-- json-stream style parsing would rather look like this:
--
-- > >>> parseByteString (array integer) "[1,2,3]" :: [Int]
-- > [1,2,3]
--
-- Parsers can be combinated using  '<*>' and '<|>' operators. These operators cause
-- parallel parsing and yield some combination of the parsed values.
--
-- > JSON: text = [{"name": "John", "age": 20}, {"age": 30, "name": "Frank"} ]
-- > >>> let parser = array $ (,) <$> "name" .: string
-- >                              <*> "age"  .: integer
-- > >>> parseByteString  parser text :: [(Text,Int)]
-- > [("John",20),("Frank",30)]
--
-- When parsing larger values, it is advisable to use lazy ByteStrings as the chunking
-- of the ByteStrings causes the parsing to continue more efficently because less state
-- is needed to be held in memory with parallel parsers.
--
-- More examples are available on <https://github.com/ondrap/json-stream>.


-- $constant
-- Constant space decoding is possible if the grammar does not specify non-constant
-- operation. The non-constant operations are 'value', 'string', 'toList' and in some instances
-- '<*>' (and currently object accessors as the key size is not limited).
--
-- The 'value' parser works by creating an aeson AST and passing it to the
-- 'parseJSON' method. The AST can consume a lot of memory before it is rejected
-- in 'parseJSON'. To achieve constant space the parsers 'boundedString', 'number', 'integer',
-- 'real' and 'bool'
-- must be used; these parsers reject and do not parse data if it does not match the
-- type. (Currently the object keys are not bounded, although it is quite easy to add,
-- if you are interested, let me know).
--
-- The 'toList' parser works by accumulating all obtained values. Obviously, number
-- of such values influences the amount of used memory.
--
-- The '<*>' operator runs both parsers in parallel and when they are both done, it
-- produces combinations of the received values. It is constant-space as long as the
-- child parsers produce constant number of values. This can be achieved by using
-- 'arrayWithIndex' and 'objectWithKey' functions combined with constant space
-- parsers or limiting the number of returned elements with 'takeI'.
--
-- If the source object contains an object with multiple keys with a same name,
-- json-stream considers and matches the key multiple times. The only exception
-- is 'objectWithKey' ('.:' and '.:?') that switch to ignore parser as soon as
-- a value is returned.

-- $aeson
-- The parser uses internally "Data.Aeson" types, so that the FromJSON instances are
-- directly usable with the 'value' parser. It may be more convenient to parse the
-- outer structure with json-stream and the inner objects with aeson as long as constant-space
-- decoding is not required.
--
-- Json-stream defines the object-access operators '.:', '.:?' and '.!=',
-- but in a slightly different albeit more natural way.
--
-- > -- JSON: [{"name": "test1", "value": 1}, {"name": "test2", "value": null}, {"name": "test3"}]
-- > >>> let parser = arrayOf $ (,) <$> "name" .: string
-- > >>>                            <*> "value" .:? integer .!= (-1)
-- > >>> parseByteString parser (..JSON..)
-- > [("test1",1),("test2",-1),("test3",-1)]
