{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}

import qualified Data.ByteString.Char8 as BS
import Control.Applicative

import Data.JStream.TokenParser

data ParseResult v =  MoreData (BS.ByteString -> ParseResult v)
                | Failed
                | Done TokenParser
                | Yield v (ParseResult v)
                | Unexpected Element TokenParser

-- Convention: np -> next ParseResult, f -> function, tp -> TokenParser, v -> value

instance Functor ParseResult where
  fmap f (MoreData np) = MoreData (fmap f . np)
  fmap _ Failed = Failed
  fmap _ (Done tp) = Done tp
  fmap f (Yield v np) = Yield (f v) (fmap f np)
  fmap _ (Unexpected el tp) = Unexpected el tp

-- instance Applicative (ParseResult v) where
--   pure x = Yield x Done
--   (<*>) m marg =

type Parser a = TokenParser -> ParseResult a

array :: Parser a -> Parser a
array valparse tp =
  case tp of
    (PartialResult ArrayBegin ntp _) -> arrcontent (valparse ntp)
    (PartialResult el ntp _) -> Unexpected el ntp
    (TokMoreData ntok) -> MoreData (array valparse . ntok)
    TokFailed -> Failed
  where
    arrcontent (Done ntp) = arrcontent (valparse ntp) -- Reset to next value
    arrcontent (MoreData np) = MoreData (arrcontent . np)
    arrcontent (Yield v np) = Yield v (arrcontent np)
    arrcontent Failed = Failed
    arrcontent (Unexpected ArrayEnd ntp) = Done ntp
    arrcontent (Unexpected _ _) = Failed

value :: Parser JValue
value TokFailed = Failed
value (TokMoreData ntok) = MoreData (value . ntok)
value (PartialResult (JValue val) ntok _) = Yield val (Done ntok)
value (PartialResult el ntok _) = Unexpected el ntok

execIt :: [BS.ByteString] -> (TokenParser -> ParseResult JValue) -> IO ()
execIt input startParseResult = loop (tail input) $ startParseResult (tokenParser $ head input)
  where
    loop [] (MoreData _) = putStrLn "Out of data - "
    loop _ (Failed) = putStrLn "Failed"
    loop _ (Done _) = putStrLn "Done"
    loop (dta:rest) (MoreData np) =
        loop rest $ np dta
    loop dta (Yield item np) = do
        putStrLn $ "Got: " ++ show item
        loop dta $ np
    loop _ (Unexpected _ _) = putStrLn "Unexpected - failed"

testParser = array (array value)

main :: IO ()
main = do
  -- let test = ["[1,2", "2,3,\"", "ond\\\"ra\"","t", "rue,fal", "se,[null]", "{\"ondra\":\"martin\", \"x\":5}", "]"]
  let test = ["[[1, 2], [3, 4 ], [5, \"ondra\", true, false, null] ] "]
  execIt test testParser
  return ()
