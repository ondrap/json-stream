{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections     #-}

import           Control.Applicative
import qualified Data.ByteString.Char8    as BS
import qualified Data.Text                as T

import           Data.JStream.TokenParser

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

instance Alternative Parser where
  empty = ignoreVal
  -- | Run both parsers in parallel yielding from both as the data comes
  (<|>) m1 m2 = undefined


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

array :: Parser a -> Parser a
array valparse = array' (const valparse)

arrayIndex :: Int -> Parser a -> Parser a
arrayIndex idx valparse = array' itemFn
  where
    itemFn aidx
      | aidx == idx = valparse
      | otherwise = ignoreVal

object' :: Parser a -> Parser a
object' valparse = Parser $ \tp ->
  case tp of
    (PartialResult ObjectBegin ntp _) -> objcontent (callParse valparse ntp)
    (PartialResult el ntp _) -> Unexpected el ntp
    (TokMoreData ntok _) -> MoreData (object' valparse, ntok)
    (TokFailed _) -> Failed "Object - token failed"
  where
    objcontent (Done ntp) = objcontent (callParse valparse ntp) -- Reset to next value
    objcontent (MoreData (Parser np, ntok)) = MoreData (Parser (objcontent . np), ntok)
    objcontent (Yield v np) = Yield v (objcontent np)
    objcontent (Failed err) = Failed err
    objcontent (Unexpected ObjectEnd ntp) = Done ntp
    objcontent (Unexpected el _) = Failed ("Object - unexpected: " ++ show el)

objectItems :: Parser a -> Parser (T.Text, a)
objectItems valparse = object' $ Parser keyValue_'
  where
    keyValue_' (TokFailed _) = Failed "KeyValue - token failed"
    keyValue_' (TokMoreData ntok _) = MoreData (Parser keyValue_', ntok)
    keyValue_' (PartialResult (ObjectKey key) ntok _) = (key,) <$> callParse valparse ntok
    keyValue_' (PartialResult el ntok _) = Unexpected el ntok

objectValues :: Parser a -> Parser a
objectValues valparse = snd <$> objectItems valparse

objectKey :: T.Text -> Parser a -> Parser a
objectKey name valparse = object' $ Parser filterKey'
  where
    filterKey' (TokFailed _) = Failed "KeyValue - token failed"
    filterKey' (TokMoreData ntok _) = MoreData (Parser filterKey', ntok)
    filterKey' (PartialResult (ObjectKey key) ntok _)
      | key == name = callParse valparse ntok
      | otherwise = callParse ignoreVal ntok
    filterKey' (PartialResult el ntok _) = Unexpected el ntok

-- | Parses underlying values and generates a JValue
value :: Parser JValue
value = Parser value'
  where
    value' (TokFailed _) = Failed "Value - token failed"
    value' (TokMoreData ntok _) = MoreData (Parser value', ntok)
    value' (PartialResult (JValue val) ntok _) = Yield val (Done ntok)
    value' tok@(PartialResult ArrayBegin _ _) = JArray <$> callParse (getYields (array value)) tok
    value' tok@(PartialResult ObjectBegin _ _) = JObject <$> callParse (getYields (objectItems value)) tok
    value' (PartialResult el ntok _) = Unexpected el ntok

-- | Continue parsing, thus skipping any value.
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
getYields :: Parser a -> Parser [a]
getYields f = Parser $ \ntok -> loop [] (callParse f ntok)
  where
    loop acc (Done ntp) = Yield (reverse acc) (Done ntp)
    loop acc (MoreData (Parser np, ntok)) = MoreData (Parser (loop acc . np), ntok)
    loop acc (Yield v np) = loop (v:acc) np
    loop _ (Failed err) = Failed err
    loop _ (Unexpected el _) = Failed ("getYields - unexpected: " ++ show el)

execIt :: Show a => [BS.ByteString] -> Parser a -> IO ()
execIt input parser = loop (tail input) $ callParse parser (tokenParser $ head input)
  where
    loop [] (MoreData _) = putStrLn "Out of data - "
    loop _ (Failed err) = putStrLn $ "Failed: " ++ err
    loop _ (Done (PartialResult _ _ rest)) = putStrLn $ "Done: "  ++ show rest
    loop _ (Done (TokFailed rest)) = putStrLn $ "Done: " ++ show rest
    loop _ (Done (TokMoreData _ bl)) = putStrLn $ "Done md - more data: " ++ show bl
    loop (dta:rest) (MoreData (Parser np, ntok)) = loop rest $ np (ntok dta)
    loop dta (Yield item np) = do
        putStrLn $ "Got: " ++ show item
        loop dta np
    loop _ (Unexpected _ _) = putStrLn "Unexpected - failed"

testParser = arrayIndex 2 $ arrayIndex 1 value
-- testParser = (,) <$> array value <*> array value

main :: IO ()
main = do
  let test = ["[1,2,[3,4],5]"]
  execIt test testParser
  return ()
