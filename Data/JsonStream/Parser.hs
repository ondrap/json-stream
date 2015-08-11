{-# LANGUAGE BangPatterns      #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections     #-}

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

    -- * Performance
    -- $performance

    -- * Constant space decoding
    -- $constant

    -- * Aeson compatibility
    -- $aeson

    -- * The @Parser@ type
    Parser
  , ParseOutput(..)
    -- * Operators
  , (>^>)
    -- * Parsing functions
  , runParser
  , runParser'
  , parseByteString
  , parseLazyByteString
    -- * FromJSON parser
  , value
  , string
  , bytestring
    -- * Constant space parsers
  , safeString
  , number
  , integer
  , real
  , bool
  , jNull
    -- * Structure operators
  , (.:)
  , (.:?)
  , (.|)
  , (.!)
    -- * Structure parsers
  , objectWithKey
  , objectItems
  , objectValues
  , arrayOf
  , arrayWithIndexOf
  , indexedArrayOf
  , nullable
    -- * Parsing modifiers
  , filterI
  , takeI
  , toList
  , mapWithFailure
    -- * SAX-like parsers
  , arrayFound
  , objectFound
) where

import           Control.Applicative
import qualified Data.Aeson                  as AE
import qualified Data.ByteString             as BS
import qualified Data.ByteString.Lazy        as BL
import qualified Data.HashMap.Strict         as HMap
import           Data.Scientific             (Scientific, isInteger,
                                              toBoundedInteger, toRealFloat)
import qualified Data.Text                   as T
import           Data.Text.Encoding          (encodeUtf8)
import qualified Data.Text.Lazy              as TL
import           Data.Text.Lazy.Encoding     (decodeUtf8')
import qualified Data.Vector                 as Vec

import           Data.JsonStream.CLexer      (tokenParser)
import           Data.JsonStream.TokenParser


-- | Limit for the size of an object key
objectKeyStringLimit :: Int
objectKeyStringLimit = 65536

-- | Private parsing result
data ParseResult v =  MoreData (Parser v, BS.ByteString -> TokenResult)
                    | Failed String
                    | Done BS.ByteString TokenResult
                    -- The bytestring is remaining unparsed data, we need to return it somehow
                    | Yield v (ParseResult v)


instance Functor ParseResult where
  fmap f (MoreData (np, ntok)) = MoreData (fmap f np, ntok)
  fmap _ (Failed err) = Failed err
  fmap _ (Done ctx tok) = Done ctx tok
  fmap f (Yield v np) = Yield (f v) (fmap f np)

-- | A representation of the parser.
newtype Parser a = Parser {
    callParse :: TokenResult -> ParseResult a
}

instance Functor Parser where
  fmap f (Parser p) = Parser $ \d -> fmap f (p d)

-- | Yield list of results, finish with last action
yieldResults :: [a] -> ParseResult a -> ParseResult a
yieldResults values end = foldr Yield end values

-- | '<*>' will run both parsers in parallel and combine results. It
-- behaves as a list functor (produces all combinations), but the typical
-- use is:
--
-- > JSON: text = [{"name": "John", "age": 20}, {"age": 30, "name": "Frank"} ]
-- > >>> let parser = arrayOf $ (,) <$> "name" .: string
-- >                                <*> "age"  .: integer
-- > >>> parseByteString parser text :: [(Text,Int)]
-- > [("John",20),("Frank",30)]
instance Applicative Parser where
  pure x = Parser $ \tok -> process (callParse ignoreVal tok)
    where
      process (Failed err) = Failed err
      process (Done ctx tok) = Yield x (Done ctx tok)
      process (MoreData (np, ntok)) = MoreData (Parser (process . callParse np), ntok)
      process _ = Failed "Internal error in pure, ignoreVal doesn't yield"

  (<*>) m1 m2 = Parser $ \tok -> process ([], []) (callParse m1 tok) (callParse m2 tok)
    where
      process ([], _) (Done ctx ntok) _ = Done ctx ntok -- Optimize, return immediately when first parser fails
      process (lst1, lst2) (Yield v np1) p2 = process (v:lst1, lst2) np1 p2
      process (lst1, lst2) p1 (Yield v np2) = process (lst1, v:lst2) p1 np2
      process (lst1, lst2) (Done ctx ntok) (Done {}) =
        yieldResults [ mx my | mx <- reverse lst1, my <- reverse lst2 ] (Done ctx ntok)
      process lsts (MoreData (np1, ntok1)) (MoreData (np2, _)) =
        MoreData (Parser (\tok -> process lsts (callParse np1 tok) (callParse np2 tok)), ntok1)
      process _ (Failed err) _ = Failed err
      process _ _ (Failed err) = Failed err
      process _ _ _ = Failed "Unexpected error in parallel processing <*>."


-- | '<|>' will run both parsers in parallel yielding from both as the data comes
--
-- > json: [{"key1": [1,2], "key2": [5,6], "key3": [8,9]}]
-- > >>> let parser = arrayOf $     "key1" .: (arrayOf value)
-- >                            <|> "key2" .: (arrayOf value)
-- > >>> parseByteString parser json :: [Int]
-- > [1,2,5,6]
instance Alternative Parser where
  empty = ignoreVal
  (<|>) m1 m2 = Parser $ \tok -> process (callParse m1 tok) (callParse m2 tok)
    where
      process (Yield v np1) p2 = Yield v (process np1 p2)
      process p1 (Yield v np2) = Yield v (process p1 np2)
      process (Done ctx ntok) (Done {}) = Done ctx ntok
      process (MoreData (np1, ntok)) (MoreData (np2, _)) =
          MoreData (Parser $ \tok -> process (callParse np1 tok) (callParse np2 tok), ntok)
      process (Failed err) _ = Failed err
      process _ (Failed err) = Failed err
      process _ _ = error "Unexpected error in parallel processing <|>"


-- | Match items from the first parser, if none is matched, return items
-- from the second parser. Constant-space if second parser returns
-- constant number of items. '.|' is implemented using this operator.
--
-- > >>> let json = "[{\"key1\": [1,2], \"key2\": [5,6], \"key3\": [8,9]}]"
-- > >>> let parser = arrayOf $ "key1" .: (arrayOf value) >^> "key2" .: (arrayOf value)
-- > >>> parseByteString parser json :: [Int]
-- > [1,2]
-- > >>> let parser = arrayOf $ "key-non" .: (arrayOf value) >^> "key2" .: (arrayOf value)
-- > >>> parseByteString parser json :: [Int]
-- > [5,6]

(>^>) :: Parser a -> Parser a -> Parser a
m1 >^> m2 = Parser $ \tok -> process [] (callParse m1 tok) (Just $ callParse m2 tok)
  where
    -- First returned item -> disable second parser
    process _ (Yield v np1) _ = Yield v (process [] np1 Nothing)
    -- First done with disabled second -> exit
    process _ (Done ctx ntok) Nothing = Done ctx ntok
    -- Both done but second not disabled -> yield items from the second
    process lst (Done ctx ntok) (Just (Done {})) = yieldResults (reverse lst) (Done ctx ntok)
    -- Second yield - remember data
    process lst np1 (Just (Yield v np2)) = process (v:lst) np1 (Just np2)
    -- Moredata processing
    process lst (MoreData (np1, ntok)) Nothing =
        MoreData (Parser $ \tok -> process lst (callParse np1 tok) Nothing, ntok)
    process lst (MoreData (np1, ntok)) (Just (MoreData (np2, _))) =
        MoreData (Parser $ \tok -> process lst (callParse np1 tok) (Just $ callParse np2 tok), ntok)
    process _ (Failed err) _ = Failed err
    process _ _ (Just (Failed err)) = Failed err
    process _ _ _ = error "Unexpected error in parallel processing >^>"

infixl 3 >^>

array' :: (Int -> Parser a) -> Parser a
array' valparse = Parser $ \tp ->
  case tp of
    (PartialResult ArrayBegin ntp) -> moreData (nextitem 0) ntp
    (PartialResult _ _) -> callParse ignoreVal tp -- Run ignoreval parser on the same output we got
    (TokMoreData ntok) -> MoreData (array' valparse, ntok)
    (TokFailed) -> Failed "Array - token failed"
  where
    nextitem _ _ (ArrayEnd ctx) ntok = Done ctx ntok
    nextitem i tok _ _ = arrcontent i (callParse (valparse i) tok)

    arrcontent i (Done _ ntp) = moreData (nextitem (i+1)) ntp
    arrcontent i (MoreData (Parser np, ntp)) = MoreData (Parser (arrcontent i . np), ntp)
    arrcontent i (Yield v np) = Yield v (arrcontent i np)
    arrcontent _ (Failed err) = Failed err

-- | Match all items of an array.
arrayOf :: Parser a -> Parser a
arrayOf valparse = array' (const valparse)

-- | Generate start/end objects when an element is found, in between run a parser.
-- The inner parser is not run if an array is not found.
elemFound :: Element -> a -> a -> Parser a -> Parser a
elemFound elsearch start end parser = Parser $ moreData handle
  where
    handle tok el _
      | el == elsearch = Yield start (parseAndAppend (callParse parser tok))
    handle tok _ _ = callParse ignoreVal tok

    parseAndAppend (Failed err) = Failed err
    parseAndAppend (Yield v np) = Yield v (parseAndAppend np)
    parseAndAppend (MoreData (Parser np, ntp)) = MoreData (Parser (parseAndAppend . np), ntp)
    parseAndAppend (Done ctx ntp) = Yield end (Done ctx ntp)

-- | Generate start/end values when an object is found, in between run a parser.
-- The inner parser is not run if an array is not found.
objectFound :: a -> a -> Parser a -> Parser a
objectFound = elemFound ObjectBegin

-- | Generate start/end values when an array is found, in between run a parser.
-- The inner parser is not run if an array is not found.
--
-- > >>> let test = "[[1,2,3],true,[],false,{\"key\":1}]" :: ByteString
-- > >>> parseByteString (arrayOf (arrayFound 10 20 (1 .! integer))) test :: [Int]
-- > [10,2,20,10,20]
arrayFound :: a -> a -> Parser a -> Parser a
arrayFound = elemFound ArrayBegin

-- | Match nith item in an array.
arrayWithIndexOf :: Int -> Parser a -> Parser a
arrayWithIndexOf idx valparse = array' itemFn
  where
    itemFn aidx
      | aidx == idx = valparse
      | otherwise = ignoreVal

-- | Match all items of an array, add index to output.
indexedArrayOf :: Parser a -> Parser (Int, a)
indexedArrayOf valparse = array' (\(!key) -> (key,) <$> valparse)


-- | Go through an object; if once is True, yield only first success, then ignore the rest
object' :: Bool -> (T.Text -> Parser a) -> Parser a
object' once valparse = Parser $ \tp ->
  case tp of
    (PartialResult ObjectBegin ntp) -> moreData (nextitem False) ntp
    (PartialResult _ _) -> callParse ignoreVal tp -- Run ignoreval parser on the same output we got
    (TokMoreData ntok) -> MoreData (object' once valparse, ntok)
    (TokFailed) -> Failed "Array - token failed"
  where
    nextitem _ _ (ObjectEnd ctx) ntok = Done ctx ntok
    nextitem yielded _ (JValue (AE.String key)) ntok = objcontent yielded (callParse (valparse key) ntok)
    nextitem yielded _ (StringContent str) ntok =
          objcontent yielded $ moreData (getLongKey [str] (BS.length str)) ntok
    nextitem _ _ el _ = Failed $ "Object - unexpected item: " ++ show el

    -- If we already yielded and should yield once, ignore the rest of the object
    objcontent yielded (Done _ ntp)
      | once && yielded = callParse (ignoreVal' 1) ntp
      | otherwise = moreData (nextitem yielded) ntp -- Reset to next value
    objcontent yielded (MoreData (Parser np, ntok)) = MoreData (Parser (objcontent yielded. np), ntok)
    objcontent _ (Yield v np) = Yield v (objcontent True np)
    objcontent _ (Failed err) = Failed err

    getLongKey acc !len _ el ntok =
      case el of
        StringEnd
          | Right key <- decodeUtf8' (BL.fromChunks $ reverse acc) ->
              callParse (valparse $ T.concat $ TL.toChunks key) ntok
          | otherwise -> Failed "Error decoding UTF8"
        StringContent str
          | len > objectKeyStringLimit -> callParse (ignoreStrRestThen ignoreVal) ntok
          | otherwise -> moreData (getLongKey (str:acc) (len + BS.length str)) ntok
        _ -> Failed "Object longstr - lexer failed."

-- | Helper function to deduplicate TokMoreData/FokFailed logic
moreData :: (TokenResult -> Element -> TokenResult -> ParseResult v) -> TokenResult -> ParseResult v
moreData parser tok =
  case tok of
    PartialResult el ntok -> parser tok el ntok
    TokMoreData ntok -> MoreData (Parser (moreData parser), ntok)
    TokFailed -> Failed "More data - lexer failed."

-- | Match all key-value pairs of an object, return them as a tuple.
-- If the source object defines same key multiple times, all values
-- are matched.
objectItems :: Parser a -> Parser (T.Text, a)
objectItems valparse = object' False $ \(!key) -> (key,) <$> valparse

-- | Match all key-value pairs of an object, return only values.
-- If the source object defines same key multiple times, all values
-- are matched. Keys are ignored.
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
aeValue = Parser $ moreData value'
  where
    value' tok el ntok =
      case el of
        JValue val -> Yield val (Done "" ntok)
        JInteger val -> Yield (AE.Number $ fromIntegral val) (Done "" ntok)
        StringContent _ -> callParse (AE.String <$> longString Nothing) tok
        ArrayBegin -> AE.Array . Vec.fromList <$> callParse (toList (arrayOf aeValue)) tok
        ObjectBegin -> AE.Object . HMap.fromList <$> callParse (toList (objectItems aeValue)) tok
        _ -> Failed ("aeValue - unexpected token: " ++ show el)

-- | Convert a strict aeson value (no object/array) to a value.
-- Non-matching type is ignored and not parsed (unlike 'value')
jvalue :: (AE.Value -> Maybe a) -> (Int -> Maybe a) -> Parser a
jvalue convert cvtint = Parser (moreData value')
  where
    value' tok el ntok =
      case el of
        JValue val
          | Just convValue <- convert val  -> Yield convValue (Done "" ntok)
          | otherwise -> Done "" ntok
        JInteger val
          | Just convValue <- cvtint val -> Yield convValue (Done "" ntok)
          | otherwise -> Done "" ntok
        _ -> callParse ignoreVal tok


-- | Match a possibly bounded string roughly limited by a limit
longString :: Maybe Int -> Parser T.Text
longString mbounds = Parser $ moreData (handle [] 0)
  where
    handle acc !len tok el ntok =
      case el of
        JValue (AE.String str) -> Yield str (Done "" ntok)
        StringContent str
          | (Just bounds) <- mbounds, len > bounds -- If the string exceeds bounds, discard it
                          -> callParse (ignoreStrRestThen (Parser $ Done "")) ntok
          | otherwise     -> moreData (handle (str:acc) (len + BS.length str)) ntok
        StringEnd
          | Right val <- decodeUtf8' (BL.fromChunks $ reverse acc)
                      -> Yield (T.concat $ TL.toChunks val) (Done "" ntok)
          | otherwise -> Failed "Error decoding UTF8"
        _ ->  callParse ignoreVal tok

-- | Match string as a ByteString without decoding the data from UTF8 (strings larger than input chunk,
-- small get always decoded).
bytestring :: Parser BL.ByteString
bytestring = Parser $ moreData (handle [])
  where
    handle acc tok el ntok =
      case el of
        JValue (AE.String str) -> Yield (BL.fromChunks [encodeUtf8 str]) (Done "" ntok)
        StringContent str -> moreData (handle (str:acc)) ntok
        StringEnd -> Yield (BL.fromChunks $ reverse acc) (Done "" ntok)
        _ -> callParse ignoreVal tok


-- | Parse string value, skip parsing otherwise.
string :: Parser T.Text
string = longString Nothing

-- | Stops parsing string after the limit is reached. The string will not be matched
-- if it exceeds the size.
safeString :: Int -> Parser T.Text
safeString limit = longString (Just limit)

-- | Parse number, return in scientific format.
number :: Parser Scientific
number = jvalue cvt (Just . fromIntegral)
  where
    cvt (AE.Number num) = Just num
    cvt _ = Nothing

-- | Parse to bounded integer type (not 'Integer').
-- If you are using integer numbers, use this parser.
-- It skips the conversion JSON -> 'Scientific' -> 'Int' and uses an 'Int' directly.
integer :: (Integral i, Bounded i) => Parser i
integer = jvalue cvt (Just . fromIntegral)
  where
    cvt (AE.Number num)
      | isInteger num = toBoundedInteger num
    cvt _ = Nothing

-- | Parse to float/double.
real :: RealFloat a => Parser a
real = jvalue cvt (Just . fromIntegral)
  where
    cvt (AE.Number num) = Just $ toRealFloat num
    cvt _ = Nothing

-- | Parse bool, skip if the type is not bool.
bool :: Parser Bool
bool = jvalue cvt (const Nothing)
  where
    cvt (AE.Bool b) = Just b
    cvt _ = Nothing

-- | Match a null value.
jNull :: Parser ()
jNull = jvalue cvt (const Nothing)
  where
    cvt (AE.Null) = Just ()
    cvt _ = Nothing

-- | Parses a field with a possible null value.
nullable :: Parser a -> Parser (Maybe a)
nullable valparse = Parser (moreData value')
  where
    value' _ (JValue AE.Null) ntok = Yield Nothing (Done "" ntok)
    value' tok _ _ = callParse (Just <$> valparse) tok

-- | Match 'FromJSON' value. Calls parseJSON on the parsed value.
--
-- > >>> let json = "[{\"key1\": [1,2], \"key2\": [5,6]}]"
-- > >>> parseByteString (arrayOf value) json :: [Value]
-- > [Object fromList [("key2",Array (fromList [Number 5.0,Number 6.0])),("key1",Array (fromList [Number 1.0,Number 2.0]))]]
value :: AE.FromJSON a => Parser a
value = Parser $ \ntok -> loop (callParse aeValue ntok)
  where
    loop (Done ctx ntp) = Done ctx ntp
    loop (Failed err) = Failed err
    loop (MoreData (Parser np, ntok)) = MoreData (Parser (loop . np), ntok)
    loop (Yield v np) =
      case AE.fromJSON v of
        AE.Error _ -> loop np
        AE.Success res -> Yield res (loop np)

-- | Take maximum n matching items.
--
-- > >>> parseByteString (takeI 3 $ arrayOf integer) "[1,2,3,4,5,6,7,8,9,0]" :: [Int]
-- > [1,2,3]
takeI :: Int -> Parser a -> Parser a
takeI num valparse = Parser $ \tok -> loop num (callParse valparse tok)
  where
    loop _ (Done ctx ntp) = Done ctx ntp
    loop _ (Failed err) = Failed err
    loop n (MoreData (Parser np, ntok)) = MoreData (Parser (loop n . np), ntok)
    loop 0 (Yield _ np) = loop 0 np
    loop n (Yield v np) = Yield v (loop (n-1) np)

-- | Skip rest of string + call next parser
ignoreStrRestThen :: Parser a -> Parser a
ignoreStrRestThen next = Parser $ moreData handle
  where
    handle _ el ntok =
      case el of
        StringContent _ -> moreData handle ntok
        StringEnd -> callParse next ntok
        _ -> Failed "Unexpected result in ignoreStrRestPlusOne"


-- | Skip value; cheat to avoid parsing and make it faster
ignoreVal :: Parser a
ignoreVal = ignoreVal' 0

ignoreVal' :: Int -> Parser a
ignoreVal' stval = Parser $ moreData (handleTok stval)
  where
    handleLongString level _ (StringContent _) ntok = moreData (handleLongString level) ntok
    handleLongString 0 _ StringEnd ntok = Done "" ntok
    handleLongString level _ StringEnd ntok = moreData (handleTok level) ntok
    handleLongString _ _ el _ = Failed $ "Unexpected element in handleLongStr: " ++ show el

    handleTok :: Int -> TokenResult -> Element -> TokenResult -> ParseResult a
    handleTok 0 _ (JValue _) ntok = Done "" ntok
    handleTok 0 _ (JInteger _) ntok = Done "" ntok
    handleTok 0 _ (ArrayEnd _) _ = Failed "ArrayEnd in ignoreval on 0 level"
    handleTok 0 _ (ObjectEnd _) _ = Failed "ObjectEnd in ignoreval on 0 level"
    handleTok 1 _ (ArrayEnd ctx) ntok = Done ctx ntok
    handleTok 1 _ (ObjectEnd ctx) ntok = Done ctx ntok
    handleTok level _ el ntok =
      case el of
        JValue _ -> moreData (handleTok level) ntok
        JInteger _ -> moreData (handleTok level) ntok
        StringContent _ -> moreData (handleLongString level) ntok
        ArrayEnd _ -> moreData (handleTok (level - 1)) ntok
        ObjectEnd _ -> moreData (handleTok (level - 1)) ntok
        ArrayBegin -> moreData (handleTok (level + 1)) ntok
        ObjectBegin -> moreData (handleTok (level + 1)) ntok
        StringEnd -> Failed "Internal error - out of order StringEnd"

-- | Gather matches and return them as list.
--
-- > >>> let json = "[{\"keys\":[1,2], \"values\":[5,6]}, {\"keys\":[9,8], \"values\":[7,6]}]"
-- > >>> let parser = arrayOf $ (,) <$> toList ("keys" .: arrayOf integer)
-- >                                <*> toList ("values" .: arrayOf integer)
-- > >>> parseByteString parser json :: [([Int], [Int])]
-- > [([1,2],[5,6]),([9,8],[7,6])]
toList :: Parser a -> Parser [a]
toList f = Parser $ \ntok -> loop [] (callParse f ntok)
  where
    loop acc (Done ctx ntp) = Yield (reverse acc) (Done ctx ntp)
    loop acc (MoreData (Parser np, ntok)) = MoreData (Parser (loop acc . np), ntok)
    loop acc (Yield v np) = loop (v:acc) np
    loop _ (Failed err) = Failed err

-- | Let only items matching a condition pass.
--
-- > >>> parseByteString (filterI (>5) $ arrayOf integer) "[1,2,3,4,5,6,7,8,9,0]" :: [Int]
-- > [6,7,8,9]
filterI :: (a -> Bool) -> Parser a -> Parser a
filterI cond valparse = Parser $ \ntok -> loop (callParse valparse ntok)
  where
    loop (Done ctx ntp) = Done ctx ntp
    loop (Failed err) = Failed err
    loop (MoreData (Parser np, ntok)) = MoreData (Parser (loop . np), ntok)
    loop (Yield v np)
      | cond v = Yield v (loop np)
      | otherwise = loop np

-- | 
-- A back-door for lifting of possibly failing actions.
mapWithFailure :: (a -> Either String b) -> Parser a -> Parser b
mapWithFailure mapping =
  updateParser
  where
    updateParser (Parser run) = Parser $ updateParseResult . run
    updateParseResult x = case x of
      MoreData (parser, continuation) -> MoreData (updateParser parser, continuation)
      Failed message -> Failed message
      Done a b -> Done a b
      Yield value parseResult -> case mapping value of
        Left message -> Failed message
        Right value' -> Yield value' (updateParseResult parseResult)

--- Convenience operators

-- | Synonym for 'objectWithKey'. Matches key in an object. The '.:' operators can be chained.
--
-- > >>> let json = "{\"key1\": {\"nested-key\": 3}}"
-- > >>> parseByteString ("key1" .: "nested-key" .: integer) json :: [Int]
-- > [3]
(.:) :: T.Text -> Parser a -> Parser a
(.:) = objectWithKey
infixr 7 .:

-- | Returns 'Nothing' if value is null or does not exist or match. Otherwise returns 'Just' value.
--
-- > key .:? val = Just <$> key .: val >^> pure Nothing
(.:?) :: T.Text -> Parser a -> Parser (Maybe a)
key .:? val = Just <$> key .: val >^> pure Nothing
infixr 7 .:?

-- | Return default value if the parsers on the left hand didn't produce a result.
--
-- > p .| defval = p >^> pure defval
--
-- The operator works on complete left side, the following statements are equal:
--
-- > Record <$>  "key1" .: "nested-key" .: value .| defaultValue
-- > Record <$> (("key1" .: "nested-key" .: value) .| defaultValue)
(.|) :: Parser a -> a -> Parser a
p .| defval = p >^> pure defval
infixl 6 .|


-- | Synonym for 'arrayWithIndexOf'. Matches n-th item in array.
--
-- > >>> parseByteString (arrayOf (1 .! bool)) "[ [1,true,null], [2,false], [3]]" :: [Bool]
-- > [True,False]
(.!) :: Int -> Parser a -> Parser a
(.!) = arrayWithIndexOf
infixr 7 .!

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
    parse (Yield v np) = ParseYield v (parse np)
    parse (Done ctx _) = ParseDone ctx

-- | Run streaming parser, immediately returns 'ParseNeedData'.
runParser :: Parser a -> ParseOutput a
runParser parser = runParser' parser BS.empty

-- | Parse a bytestring, generate lazy list of parsed values. If an error occurs, throws an exception.
--
-- > parseByteString (arrayOf integer) "[1,2,3,4]" :: [Int]
-- > [1,2,3,4]
--
-- > parseByteString (arrayOf ("name" .: string)) "[{\"name\":\"KIWI\"}, {\"name\":\"BIRD\"}]"
-- > ["KIWI","BIRD"]
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
-- Example of json-stream style parsing:
--
-- > >>> parseByteString (arrayOf integer) "[1,2,3]" :: [Int]
-- > [1,2,3]
--
-- Parsers can be combinated using  '<*>' and '<|>' operators. The parsers are
-- run in parallel and return combinations of the parsed values.
--
-- > JSON: text = [{"name": "John", "age": 20}, {"age": 30, "name": "Frank"} ]
-- > >>> let parser = arrayOf $ (,) <$> "name" .: string
-- >                                <*> "age"  .: integer
-- > >>> parseByteString  parser text :: [(Text,Int)]
-- > [("John",20),("Frank",30)]
--
-- When parsing larger values, it is advisable to use lazy ByteStrings. The parsing
-- is then more memory efficient as less lexical state
-- is needed to be held in memory for parallel parsers.
--
-- More examples are available on <https://github.com/ondrap/json-stream>.


-- $constant
-- Constant space decoding is possible if the grammar does not specify non-constant
-- operations. The non-constant operations are 'value', 'string', 'toList' and in some instances
-- '<*>'.
--
-- The 'value' parser works by creating an aeson AST and passing it to the
-- 'parseJSON' method. The AST can consume a lot of memory before it is rejected
-- in 'parseJSON'. To achieve constant space the parsers 'safeString', 'number', 'integer',
-- 'real' and 'bool'
-- must be used; these parsers reject and do not parse data if it does not match the
-- type.
--
-- The object key length is limited to ~64K. Object records with longer key are ignored and unparsed.
--
-- Numbers are limited to 200.000 digits. Longer numbers will make the parsing fail.
--
-- The 'toList' parser works by accumulating all matched values. Obviously, number
-- of such values influences the amount of used memory.
--
-- The '<*>' operator runs both parsers in parallel and when they are both done, it
-- produces combinations of the received values. It is constant-space as long as the
-- number of element produced by child parsers is limited by a constant. This can be achieved by using
-- '.!' and '.:' functions combined with constant space
-- parsers or limiting the number of returned elements with 'takeI'.
--
-- If the source object contains an object with multiple keys with a same name,
-- json-stream matches the key multiple times. The only exception
-- is 'objectWithKey' ('.:' and '.:?') that return at most one value for a given key.

-- $aeson
-- The parser uses internally "Data.Aeson" types, so that the FromJSON instances are
-- directly usable with the 'value' parser. It may be more convenient to parse the
-- outer structure with json-stream and the inner objects with aeson as long as constant-space
-- decoding is not required.
--
-- Json-stream defines the object-access operators '.:', '.:?'
-- but in a slightly different albeit more natural way. New operators are '.!' for
-- array access and '.|' to handle missing values.
--
-- > -- JSON: [{"name": "test1", "value": 1}, {"name": "test2", "value": null}, {"name": "test3"}]
-- > >>> let person = (,) <$> "name" .: string
-- > >>>                  <*> "value" .: integer .| (-1)
-- > >>> let people = arrayOf person
-- > >>> parseByteString people (..JSON..) :: [(Text, Int)]
-- > [("test1",1),("test2",-1),("test3",-1)]

-- $performance
-- The parser tries to do the least amount of work to get the job done, skipping over items that
-- are not required. General guidelines to get best performance:
--
-- Do not use the 'value' parser for the whole object if the object is big. Using json-stream
-- parsers will produce better results with less memory. The 'integer' parser was optimized in such
-- a way that the integer numbers skip the conversion to scientific, which is unavoidable in aeson.
--
-- It is possible to use the '*>' operator to filter objects based on a condition, e.g.:
--
-- > arrayOf $ id <$> "error" .: number
-- >               *> "name" .: string
--
-- This will return all objects that contain attribute error with number content. The parser will
-- skip trying to decode the name attribute if error is not found.
--
