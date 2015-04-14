{-# LANGUAGE MultiWayIf        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections     #-}
{-# LANGUAGE DeriveFunctor     #-}

import           Control.Applicative
import qualified Data.ByteString.Char8    as BS
import qualified Data.Text                as T

import           Data.JStream.TokenParser

data ParseResult v =  MoreData (BS.ByteString -> ParseResult v)
                | Failed String
                | Done TokenParser
                | Yield v (ParseResult v)
                | Unexpected Element TokenParser


instance Functor ParseResult where
  fmap f (MoreData np) = MoreData (fmap f . np)
  fmap _ (Failed err) = Failed err
  fmap _ (Done tp) = Done tp
  fmap f (Yield v np) = Yield (f v) (fmap f np)
  fmap _ (Unexpected el tp) = Unexpected el tp

instance Functor Parser where
  fmap f (Parser p) = Parser $ \d -> fmap f (p d)


newtype Parser a = Parser {
    callParse :: TokenParser -> ParseResult a
}

-- Special parser for key-value pairs
newtype KeyParser a = KeyParser {
  callKeyParse :: Parser a
} deriving (Functor)

-- instance Applicative (ParseResult v) where
--   pure x =
--   (<*>) m marg =

array :: Parser a -> Parser a
array valparse = Parser $ \tp ->
  case tp of
    (PartialResult ArrayBegin ntp _) -> arrcontent (callParse valparse ntp)
    (PartialResult el ntp _) -> Unexpected el ntp
    (TokMoreData ntok) -> MoreData (callParse (array valparse) . ntok)
    TokFailed -> Failed "Array - token failed"
  where
    arrcontent (Done ntp) = arrcontent (callParse valparse ntp) -- Reset to next value
    arrcontent (MoreData np) = MoreData (arrcontent . np)
    arrcontent (Yield v np) = Yield v (arrcontent np)
    arrcontent (Failed err) = Failed err
    arrcontent (Unexpected ArrayEnd ntp) = Done ntp
    arrcontent (Unexpected el _) = Failed ("Array - unexpected: " ++ show el)

object :: KeyParser a -> Parser a
object valparse = Parser $ \tp ->
  case tp of
    (PartialResult ObjectBegin ntp _) -> objcontent (callParse (callKeyParse valparse) ntp)
    (PartialResult el ntp _) -> Unexpected el ntp
    (TokMoreData ntok) -> MoreData (callParse (object valparse) . ntok)
    TokFailed -> Failed "Object - token failed"
  where
    objcontent (Done ntp) = objcontent (callParse (callKeyParse valparse) ntp) -- Reset to next value
    objcontent (MoreData np) = MoreData (objcontent . np)
    objcontent (Yield v np) = Yield v (objcontent np)
    objcontent (Failed err) = Failed err
    objcontent (Unexpected ObjectEnd ntp) = Done ntp
    objcontent (Unexpected el _) = Failed ("Object - unexpected: " ++ show el)

keyValue :: Parser a -> KeyParser (T.Text, a)
keyValue valparse = KeyParser $ Parser $ keyValue_'
  where
    keyValue_' TokFailed = Failed "KeyValue - token failed"
    keyValue_' (TokMoreData ntok) = MoreData (keyValue_' . ntok)
    keyValue_' (PartialResult (ObjectKey key) ntok _) = (key,) <$> callParse valparse ntok
    keyValue_' (PartialResult el ntok _) = Unexpected el ntok

objKey :: T.Text -> Parser a -> KeyParser a
objKey name valparse = KeyParser $ Parser $ \tok -> filterKey $ callParse (callKeyParse (keyValue valparse)) tok
  where
    filterKey :: ParseResult (T.Text, a) -> ParseResult a
    filterKey (Done ntp) = Done ntp
    filterKey (MoreData np) = MoreData (filterKey . np)
    filterKey (Yield (ykey, v) np)
      | ykey == name = Yield v (filterKey np)
      | otherwise = filterKey np
    filterKey (Failed err) = Failed err
    filterKey (Unexpected el ntp) = Unexpected el ntp

-- | Parses underlying values and generates a JValue
value :: Parser JValue
value = Parser value'
  where
    value' TokFailed = Failed "Value - token failed"
    value' (TokMoreData ntok) = MoreData (value' . ntok)
    value' (PartialResult (JValue val) ntok _) = Yield val (Done ntok)
    value' tok@(PartialResult ArrayBegin _ _) = JArray <$> callParse (getYields (array value)) tok
    value' (PartialResult el ntok _) = Unexpected el ntok

-- | Fetch yields of a function and return them as list
getYields :: Parser a -> Parser [a]
getYields f = Parser $ \ntok -> loop [] (callParse f ntok)
  where
    loop acc (Done ntp) = Yield (reverse acc) (Done ntp)
    loop acc (MoreData np) = MoreData (loop acc . np)
    loop acc (Yield v np) = loop (v:acc) np
    loop _ (Failed err) = Failed err
    loop _ (Unexpected el _) = Failed ("getYields - unexpected: " ++ show el)

execIt :: Show a => [BS.ByteString] -> Parser a -> IO ()
execIt input parser = loop (tail input) $ callParse parser (tokenParser $ head input)
  where
    loop [] (MoreData _) = putStrLn "Out of data - "
    loop _ (Failed err) = putStrLn $ "Failed: " ++ err
    loop _ (Done _) = putStrLn "Done"
    loop (dta:rest) (MoreData np) =
        loop rest $ np dta
    loop dta (Yield item np) = do
        putStrLn $ "Got: " ++ show item
        loop dta np
    loop _ (Unexpected _ _) = putStrLn "Unexpected - failed"

testParser = object (objKey "ondra" value)

main :: IO ()
main = do
  -- let test = ["[1,2", "2,3,\"", "ond\\\"ra\"","t", "rue,fal", "se,[null]", "{\"ondra\":\"martin\", \"x\":5}", "]"]
  -- let test = ["[[1, 2], [3, 4 ], [5, \"ondra\", true, false, null] ] "]
  let test = ["{\"ondra\":12, \"martin\":true}"]
  execIt test testParser
  return ()
