{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}

import qualified Data.ByteString.Lazy.Char8 as BL

import Data.JStream.TokenParser



iterParser _ Failed = putStrLn "Failed"
iterParser [] (MoreData _) = putStrLn "No more data"
iterParser (dta:rest) (MoreData p) = iterParser rest (p dta)
iterParser dta (PartialResult e p) = do
  print e
  iterParser dta p

main :: IO ()
main = do
  let test = ["[1,2", "2,3,\"", "ond\\\"ra\"","t", "rue,fal", "se,[null]", "{\"ondra\":\"martin\", \"x\":5}", "]"]
  iterParser (tail test) (tokenParser $ head test)
