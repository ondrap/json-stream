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

    -- * Performance
    -- $performance

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
  , bytestring
    -- * Constant space parsers
  , safeString
  , number
  , integer
  , real
  , bool
  , jNull
    -- * Convenience aeson-like operators
  , (.:)
  , (.:?)
  , (.!=)
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
import           Data.Maybe                  (fromMaybe)
import           Data.Scientific             (Scientific, isInteger,
                                              toBoundedInteger, toRealFloat)
import qualified Data.Text                   as T
import           Data.Text.Encoding          (encodeUtf8)
import qualified Data.Text.Lazy              as TL
import           Data.Text.Lazy.Encoding     (decodeUtf8')
import qualified Data.Vector                 as Vec

import           Data.JsonStream.TokenParser


-- | Limit for the size of an object key
objectKeyStringLimit :: Int
objectKeyStringLimit = 65536

-- | Private parsing result
data ParseResult v =  MoreData (Parser v, BS.ByteString -> TokenResult)
                    | Failed String
                    | Done (Maybe Element) TokenResult -- ^ The element is ] or }, it is propagated down to proper arr/obj
                    | Yield v (ParseResult v)


instance Functor ParseResult where
  fmap f (MoreData (np, ntok)) = MoreData (fmap f np, ntok)
  fmap _ (Failed err) = Failed err
  fmap _ (Done el tok) = Done el tok
  fmap f (Yield v np) = Yield (f v) (fmap f np)

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
      process (Done Nothing tok) = Yield x (Done Nothing tok)
      process (Done (Just el) tok) = Done (Just el) tok -- This is for the end of array, we have already yielded content of it
      process (MoreData (np, ntok)) = MoreData (Parser (process . callParse np), ntok)
      process _ = Failed "Internal error in pure, ignoreVal doesn't yield"

  -- | Run both parsers in parallel using a shared token parser, combine results
  (<*>) m1 m2 = Parser $ \tok -> process ([], []) (callParse m1 tok) (callParse m2 tok)
    where
      process ([], _) (Done el ntok) _ = Done el ntok -- Optimize, return immediately when first parser fails
      process (lst1, lst2) (Yield v np1) p2 = process (v:lst1, lst2) np1 p2
      process (lst1, lst2) p1 (Yield v np2) = process (lst1, v:lst2) p1 np2
      process (lst1, lst2) (Done el ntok) (Done _ _) =
        yieldResults [ mx my | mx <- lst1, my <- lst2 ] (Done el ntok)
      process lsts (MoreData (np1, ntok1)) (MoreData (np2, _)) =
        MoreData (Parser (\tok -> process lsts (callParse np1 tok) (callParse np2 tok)), ntok1)
      process _ (Failed err) _ = Failed err
      process _ _ (Failed err) = Failed err
      process _ _ _ = Failed "Unexpected error in parallel processing <*>."

      yieldResults values end = foldr Yield end values


instance Alternative Parser where
  empty = ignoreVal
  -- | Run both parsers in parallel using a shared token parser, yielding from both as the data comes
  (<|>) m1 m2 = Parser $ \tok -> process (callParse m1 tok) (callParse m2 tok)
    where
      process (Yield v np1) p2 = Yield v (process np1 p2)
      process p1 (Yield v np2) = Yield v (process p1 np2)
      process (Done el ntok) (Done _ _) = Done el ntok
      process (MoreData (np1, ntok)) (MoreData (np2, _)) =
          MoreData (Parser $ \tok -> process (callParse np1 tok) (callParse np2 tok), ntok)
      process (Failed err) _ = Failed err
      process _ (Failed err) = Failed err
      process _ _ = error "Unexpected error in parallel processing <|>"

array' :: (Int -> Parser a) -> Parser a
array' valparse = Parser $ \tp ->
  case tp of
    (PartialResult ArrayBegin ntp _) -> arrcontent 0 (callParse (valparse 0) ntp)
    (PartialResult el ntp _)
      | el == ArrayEnd || el == ObjectEnd -> Done (Just el) ntp
      | otherwise -> callParse ignoreVal tp -- Run ignoreval parser on the same output we got
    (TokMoreData ntok _) -> MoreData (array' valparse, ntok)
    (TokFailed _) -> Failed "Array - token failed"
  where
    arrcontent i (Done Nothing ntp) = arrcontent (i+1) (callParse (valparse (i + 1)) ntp) -- Reset to next value
    arrcontent i (MoreData (Parser np, ntp)) = MoreData (Parser (arrcontent i . np), ntp)
    arrcontent i (Yield v np) = Yield v (arrcontent i np)
    arrcontent _ (Failed err) = Failed err
    arrcontent _ (Done (Just ArrayEnd) ntp) = Done Nothing ntp
    arrcontent _ (Done (Just el) _) = Failed ("Array - UnexpectedEnd: " ++ show el)

-- | Match all items of an array.
arrayOf :: Parser a -> Parser a
arrayOf valparse = array' (const valparse)

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
object' once valparse = Parser $ moreData object''
  where
    object'' tok el ntok =
      case el of
         ObjectBegin -> objcontent False (moreData keyValue ntok)
         ArrayEnd -> Done (Just el) ntok
         ObjectEnd -> Done (Just el) ntok
         _ -> callParse ignoreVal tok

    -- If we already yielded and should yield once, ignore the rest of the object
    objcontent yielded (Done Nothing ntp)
      | once && yielded = callParse (ignoreVal' 1) ntp
      | otherwise = objcontent yielded (moreData keyValue ntp) -- Reset to next value
    objcontent yielded (MoreData (Parser np, ntok)) = MoreData (Parser (objcontent yielded. np), ntok)
    objcontent _ (Yield v np) = Yield v (objcontent True np)
    objcontent _ (Failed err) = Failed err
    objcontent _ (Done (Just ObjectEnd) ntp) = Done Nothing ntp
    objcontent _ (Done (Just el) _) = Failed ("Object - UnexpectedEnd: " ++ show el)

    keyValue _ el ntok =
      case el of
        JValue (AE.String key) -> callParse (valparse key) ntok
        StringBegin str -> moreData (getLongKey [str] (BS.length str)) ntok
        _| el == ArrayEnd || el == ObjectEnd -> Done (Just el) ntok
         | otherwise -> Failed ("Object - unexpected token: " ++ show el)

    getLongKey acc len _ el ntok =
      case el of
        StringEnd
          | Right key <- decodeUtf8' (BL.fromChunks $ reverse acc) ->
              callParse (valparse $ T.concat $ TL.toChunks key) ntok
          | otherwise -> Failed "Error decoding UTF8"
        StringContent str
          | len > objectKeyStringLimit -> callParse (ignoreStrRestThen ignoreVal) ntok
          | otherwise -> moreData (getLongKey (str:acc) (len + BS.length str)) ntok
        _ -> Failed "Object longstr - unexpected token."

-- | Helper function to deduplicate TokMoreData/FokFailed logic
moreData :: (TokenResult -> Element -> TokenResult -> ParseResult v) -> TokenResult -> ParseResult v
moreData parser tok =
  case tok of
    PartialResult el ntok _ -> parser tok el ntok
    TokMoreData ntok _ -> MoreData (Parser (moreData parser), ntok)
    TokFailed _ -> Failed "Object longstr - unexpected token."

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
        JValue val -> Yield val (Done Nothing ntok)
        StringBegin _ -> callParse (AE.String <$> longString Nothing) tok
        ArrayBegin -> AE.Array . Vec.fromList <$> callParse (toList (arrayOf aeValue)) tok
        ObjectBegin -> AE.Object . HMap.fromList <$> callParse (toList (objectItems aeValue)) tok
        ArrayEnd -> Done (Just el) ntok
        ObjectEnd -> Done (Just el) ntok
        _ -> Failed ("aeValue - unexpected token: " ++ show el)

-- | Convert a strict aeson value (no object/array) to a value.
-- Non-matching type is ignored and not parsed (unlike 'value')
jvalue :: (AE.Value -> Maybe a) -> Parser a
jvalue convert = Parser (moreData value')
  where
    value' tok el ntok =
      case el of
        JValue val
          | Just convValue <- convert val  -> Yield convValue (Done Nothing ntok)
          | otherwise -> Done Nothing ntok
        ArrayEnd -> Done (Just el) ntok
        ObjectEnd -> Done (Just el) ntok
        _ -> callParse ignoreVal tok


-- | Match a possibly bounded string roughly limited by a limit
longString :: Maybe Int -> Parser T.Text
longString mbounds = Parser $ moreData (handle [] 0)
  where
    handle acc len tok el ntok =
      case el of
        JValue (AE.String str) -> Yield str (Done Nothing ntok)
        StringBegin str -> moreData (handle [str] (BS.length str)) ntok
        StringContent str
          | (Just bounds) <- mbounds, len > bounds -- If the string exceeds bounds, discard it
                          -> callParse (ignoreVal' 1) ntok
          | otherwise     -> moreData (handle (str:acc) (len + BS.length str)) ntok
        StringEnd
          | Right val <- decodeUtf8' (BL.fromChunks $ reverse acc)
                      -> Yield (T.concat $ TL.toChunks val) (Done Nothing ntok)
          | otherwise -> Failed "Error decoding UTF8"
        _ ->  callParse ignoreVal tok

-- | Match string as a ByteString without decoding the data from UTF8 (strings larger than input chunk,
-- small get always decoded).
bytestring :: Parser BL.ByteString
bytestring = Parser $ moreData (handle [])
  where
    handle acc tok el ntok =
      case el of
        JValue (AE.String str) -> Yield (BL.fromChunks [encodeUtf8 str]) (Done Nothing ntok)
        StringBegin str -> moreData (handle [str]) ntok
        StringContent str -> moreData (handle (str:acc)) ntok
        StringEnd -> Yield (BL.fromChunks $ reverse acc) (Done Nothing ntok)
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
nullable valparse = Parser (moreData value')
  where
    value' _ (JValue AE.Null) ntok = Yield Nothing (Done Nothing ntok)
    value' tok _ _ = callParse (Just <$> valparse) tok

-- | Match 'FromJSON' value.
value :: AE.FromJSON a => Parser a
value = Parser $ \ntok -> loop (callParse aeValue ntok)
  where
    loop (Done el ntp) = Done el ntp
    loop (Failed err) = Failed err
    loop (MoreData (Parser np, ntok)) = MoreData (Parser (loop . np), ntok)
    loop (Yield v np) =
      case AE.fromJSON v of
        AE.Error _ -> loop np
        AE.Success res -> Yield res (loop np)

-- | Take maximum n matching items.
takeI :: Int -> Parser a -> Parser a
takeI num valparse = Parser $ \tok -> loop num (callParse valparse tok)
  where
    loop _ (Done el ntp) = Done el ntp
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
    handleTok :: Int -> TokenResult -> Element -> TokenResult -> ParseResult a
    handleTok 0 _ (JValue _) ntok = Done Nothing ntok
    handleTok 0 _ elm ntok
      | elm == ArrayEnd || elm == ObjectEnd = Done (Just elm) ntok
    handleTok 1 _ elm ntok
      | elm == ArrayEnd || elm == ObjectEnd || elm == StringEnd = Done Nothing ntok
    handleTok level _ el ntok =
      case el of
        JValue _ -> moreData (handleTok level) ntok
        StringBegin _ -> moreData (handleTok (level + 1)) ntok
        StringEnd -> moreData (handleTok (level - 1)) ntok
        StringContent _ -> moreData (handleTok level) ntok
        _| el == ArrayBegin || el == ObjectBegin -> moreData (handleTok (level + 1)) ntok
         | el == ArrayEnd || el == ObjectEnd -> moreData (handleTok (level - 1)) ntok
         | otherwise -> Failed "UnexpectedEnd "

-- | Gather matches and return them as list.
toList :: Parser a -> Parser [a]
toList f = Parser $ \ntok -> loop [] (callParse f ntok)
  where
    loop acc (Done el ntp) = Yield (reverse acc) (Done el ntp)
    loop acc (MoreData (Parser np, ntok)) = MoreData (Parser (loop acc . np), ntok)
    loop acc (Yield v np) = loop (v:acc) np
    loop _ (Failed err) = Failed err

-- | Let only items matching a condition pass
filterI :: (a -> Bool) -> Parser a -> Parser a
filterI cond valparse = Parser $ \ntok -> loop (callParse valparse ntok)
  where
    loop (Done el ntp) = Done el ntp
    loop (Failed err) = Failed err
    loop (MoreData (Parser np, ntok)) = MoreData (Parser (loop . np), ntok)
    loop (Yield v np)
      | cond v = Yield v (loop np)
      | otherwise = loop np

-- | Returns a value if none is found upstream.
defaultValue :: a -> Parser a -> Parser a
defaultValue defvalue valparse = Parser $ \ntok -> loop False (callParse valparse ntok)
  where
    loop False (Done Nothing ntp) = Yield defvalue (Done Nothing ntp)
    loop _ (Done el ntp) = Done el ntp
    loop _ (Failed err) = Failed err
    loop found (MoreData (Parser np, ntok)) = MoreData (Parser (loop found . np), ntok)
    loop _ (Yield v np) = Yield v (loop True np)

--- Convenience operators

-- | Synonym for 'objectWithKey'. Matches key in an object.
(.:) :: T.Text -> Parser a -> Parser a
(.:) = objectWithKey
infixr 7 .:

-- | Returns 'Nothing' if value is null or does not exist or match. Otherwise returns 'Just' value.
--
-- > key .:? val = defaultValue Nothing (key .: nullable val)
(.:?) :: T.Text -> Parser a -> Parser (Maybe a)
key .:? val = defaultValue Nothing (key .: nullable val)
infixr 7 .:?

-- | Converts 'Maybe' parser into normal one by providing default value instead of 'Nothing'.
--
-- > nullval .!= defval = fromMaybe defval <$> nullval
(.!=) :: Parser (Maybe a) -> a -> Parser a
nullval .!= defval = fromMaybe defval <$> nullval
infixl 6 .!=


-- | Synonym for 'arrayWithIndexOf'. Matches n-th item in array.
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
    parse (Done Nothing (PartialResult _ _ rest)) = ParseDone rest
    parse (Done Nothing (TokFailed rest)) = ParseDone rest
    parse (Done Nothing (TokMoreData _ rest)) = ParseDone rest
    parse (Done (Just el) _) = ParseFailed $ "UnexpectedEnd item: " ++ show el

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
-- Json-stream defines the object-access operators '.:', '.:?' and '.!=',
-- but in a slightly different albeit more natural way.
--
-- > -- JSON: [{"name": "test1", "value": 1}, {"name": "test2", "value": null}, {"name": "test3"}]
-- > >>> let person = (,) <$> "name" .: string
-- > >>>                  <*> "value" .:? integer .!= (-1)
-- > >>> let people = arrayOf person
-- > >>> parseByteString people (..JSON..)
-- > [("test1",1),("test2",-1),("test3",-1)]

-- $performance
-- The parser tries to do the least amount of work to get the job done. The speed is limited mostly
-- by the lexer (which is not very good). The parser itself is quite efficient in eliminating
-- the work that does not need to be done.
--
-- This can become quite significant if the resulting structure contains only a subset of the data.
-- The parser skips pieces that are not relevant. Using parsers 'string', 'integer' etc. is preferable
-- to the FromJSON 'value'.
--
-- It is possible to use the '*>' operator to filter objects based on a condition, e.g.:
--
-- > arrayOf $ id <$> "error" .: number
-- >               *> "name" .: string
--
-- This will return all objects that contain attribute error with number content. The parser will
-- skip trying to decode the name attribute if error is not found.
