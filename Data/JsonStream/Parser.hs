{-# LANGUAGE TupleSections #-}
module Data.JsonStream.Parser (
    Parser
  , ParseOutput(..)
  , runParser
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
) where

import Control.Applicative
import qualified Data.Text as T
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL

import           Data.JsonStream.TokenParser

data ParseResult v =  MoreData (Parser v, BS.ByteString -> TokenParser)
                    | Failed String
                    | Done TokenParser
                    | Yield v (ParseResult v)
                    | Unexpected Element TokenParser


instance Functor ParseResult where
  fmap f (MoreData (np, ntok)) = MoreData (fmap f np, ntok)
  fmap _ (Failed err) = Failed err
  fmap _ (Done tok) = Done tok
  fmap f (Yield v np) = Yield (f v) (fmap f np)
  fmap _ (Unexpected el tok) = Unexpected el tok

instance Functor Parser where
  fmap f (Parser p) = Parser $ \d -> fmap f (p d)

instance Applicative Parser where
  pure x = Parser $ \tok -> Yield x (callParse ignoreVal tok)
  -- | Run both parsers in parallel using a shared token parsen, combine results
  (<*>) m1 m2 = Parser (process (m1, []) (m2, []))
    where
      process (dm1,lst1) (dm2,lst2) tok =
        let (m1p, m1lst) = processParam dm1 lst1 tok
            (m2p, m2lst) = processParam dm2 lst2 tok
        in
          case (m1p, m2p) of
            (MoreData (np1, ntok1), MoreData (np2, _)) ->
                MoreData (Parser (process (np1, m1lst) (np2, m2lst)), ntok1)
            (Done ntok, Done _) -> yieldResults [ mx my | mx <- m1lst, my <- m2lst ] (Done ntok)
            (Unexpected el ntok, Unexpected _ _) ->
                  yieldResults [ mx my | mx <- m1lst, my <- m2lst ] (Unexpected el ntok)
            (Failed err, _) -> Failed err
            (_, Failed err) -> Failed err
            (_, _) -> Failed "Unexpected error in parallel processing."
      yieldResults values end = foldr Yield end values

      processParam :: Parser a -> [a] -> TokenParser -> (ParseResult a, [a])
      processParam p acc' tok = processParam' (callParse p tok) acc'
        where
          processParam' (Failed err) acc = (Failed err, acc)
          processParam' (Done ntok) acc = (Done ntok, acc)
          processParam' (MoreData np) acc = (MoreData np, acc)
          processParam' (Unexpected el ntok) acc = (Unexpected el ntok, acc)
          processParam' (Yield v np) acc = processParam' np (v:acc)

-- instance Alternative Parser where
--   empty = ignoreVal
--   -- | Run both parsers in parallel yielding from both as the data comes
--   (<|>) m1 m2 = undefined


newtype Parser a = Parser {
    callParse :: TokenParser -> ParseResult a
}

array' :: (Int -> Parser a) -> Parser a
array' valparse = Parser $ \tp ->
  case tp of
    (PartialResult ArrayBegin ntp _) -> arrcontent 0 (callParse (valparse 0) ntp)
    (PartialResult el ntp _) -> Unexpected el ntp
    (TokMoreData ntok _) -> MoreData (array' valparse, ntok)
    (TokFailed _) -> Failed "Array - token failed"
  where
    arrcontent i (Done ntp) = arrcontent (i+1) (callParse (valparse (i + 1)) ntp) -- Reset to next value
    arrcontent i (MoreData (Parser np, ntp)) = MoreData (Parser (arrcontent i . np), ntp)
    arrcontent i (Yield v np) = Yield v (arrcontent i np)
    arrcontent _ (Failed err) = Failed err
    arrcontent _ (Unexpected ArrayEnd ntp) = Done ntp
    arrcontent _ (Unexpected el _) = Failed ("Array - unexpected: " ++ show el)

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
indexedArray valparse = array' (\key -> (key,) <$> valparse)

object' :: (T.Text -> Parser a) -> Parser a
object' valparse = Parser $ \tp ->
  case tp of
    (PartialResult ObjectBegin ntp _) -> objcontent (keyValue ntp)
    (PartialResult el ntp _) -> Unexpected el ntp
    (TokMoreData ntok _) -> MoreData (object' valparse, ntok)
    (TokFailed _) -> Failed "Object - token failed"
  where
    objcontent (Done ntp) = objcontent (keyValue ntp) -- Reset to next value
    objcontent (MoreData (Parser np, ntok)) = MoreData (Parser (objcontent . np), ntok)
    objcontent (Yield v np) = Yield v (objcontent np)
    objcontent (Failed err) = Failed err
    objcontent (Unexpected ObjectEnd ntp) = Done ntp
    objcontent (Unexpected el _) = Failed ("Object - unexpected: " ++ show el)

    keyValue (TokFailed _) = Failed "KeyValue - token failed"
    keyValue (TokMoreData ntok _) = MoreData (Parser keyValue, ntok)
    keyValue (PartialResult (ObjectKey key) ntok _) = callParse (valparse key) ntok
    keyValue (PartialResult el ntok _) = Unexpected el ntok

-- | Match all key-value pairs of an object, return them as a tuple
objectItems :: Parser a -> Parser (T.Text, a)
objectItems valparse = object' $ \key -> (key,) <$> valparse

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

-- | Parses underlying values and generates a JValue
value :: Parser JValue
value = Parser value'
  where
    value' (TokFailed _) = Failed "Value - token failed"
    value' (TokMoreData ntok _) = MoreData (Parser value', ntok)
    value' (PartialResult (JValue val) ntok _) = Yield val (Done ntok)
    value' tok@(PartialResult ArrayBegin _ _) =
        JArray <$> callParse (toList (array value)) tok
    value' tok@(PartialResult ObjectBegin _ _) =
        JObject <$> callParse (toList (objectItems value)) tok
    value' (PartialResult el ntok _) = Unexpected el ntok

-- | Skip value; cheat to avoid parsing and make it faster
ignoreVal :: Parser a
ignoreVal = Parser $ handleTok 0
  where
    handleTok :: Int -> TokenParser -> ParseResult a
    handleTok _ (TokFailed _) = Failed "Token error"
    handleTok level (TokMoreData ntok _) = MoreData (Parser (handleTok level), ntok)

    handleTok 0 (PartialResult (JValue _) ntok _) = Done ntok
    handleTok 0 (PartialResult (ObjectKey _) ntok _) = Done ntok
    handleTok level (PartialResult (JValue _) ntok _) = handleTok level ntok
    handleTok level (PartialResult (ObjectKey _) ntok _) = handleTok level ntok

    handleTok 1 (PartialResult elm ntok _)
      | elm == ArrayEnd || elm == ObjectEnd = Done ntok
    handleTok level (PartialResult elm ntok _)
      | elm == ArrayBegin || elm == ObjectBegin = handleTok (level + 1) ntok
      | elm == ArrayEnd || elm == ObjectEnd = handleTok (level - 1) ntok
    handleTok _ _ = Failed "Unexpected "

-- | Fetch yields of a function and return them as list
toList :: Parser a -> Parser [a]
toList f = Parser $ \ntok -> loop [] (callParse f ntok)
  where
    loop acc (Done ntp) = Yield (reverse acc) (Done ntp)
    loop acc (MoreData (Parser np, ntok)) = MoreData (Parser (loop acc . np), ntok)
    loop acc (Yield v np) = loop (v:acc) np
    loop _ (Failed err) = Failed err
    loop _ (Unexpected el _) = Failed ("getYields - unexpected: " ++ show el)

-- | Let only items matching a condition pass
filterI :: (a -> Bool) -> Parser a -> Parser a
filterI cond valparse = Parser $ \ntok -> loop (callParse valparse ntok)
  where
    loop (Done ntp) = Done ntp
    loop (Failed err) = Failed err
    loop (Unexpected el b) = Unexpected el b
    loop (MoreData (Parser np, ntok)) = MoreData (Parser (loop . np), ntok)
    loop (Yield v np)
      | cond v = Yield v (loop np)
      | otherwise = loop np

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
    parse (Unexpected el _) = ParseFailed $ "Unexpected item: " ++ show el
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

-- | Parse input using IO monad or transformers on IO.
-- Calls "fail" on error, return Left on end of input, Right on value.
-- parseIOInput :: MonadIO m => Parser a -> IO ByteString -> m (Either ByteString (a, m a))
-- parseIOInput parser newdata =
--   where
