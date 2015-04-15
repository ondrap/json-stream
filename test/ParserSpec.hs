{-# LANGUAGE OverloadedStrings #-}
module ParserSpec where

import Control.Applicative
import Test.Hspec
import qualified Data.Aeson as AE
import Data.Aeson (Value(..))
import qualified Data.ByteString.Char8 as BS
import qualified Data.Text as T
import Data.Text.Encoding (encodeUtf8)

import Data.JsonStream.Parser
import Data.JsonStream.TokenParser

-- During the tests the single quotes are replaced with double quotes in the strings as
-- otherwise it would be unreadable in haskell

todquotes = BS.map squotes
  where
    squotes '\'' = '"'
    squotes x = x


parse parser text = parseByteString parser (todquotes text)

tokenTest :: [BS.ByteString] -> [TokenResult]
tokenTest chunks = reverse $ test [] (tail chunks) (tokenParser $ head chunks)
  where
    test acc [] o@(TokMoreData {}) = o:acc
    test acc (dta:rest) o@(TokMoreData ntok _) = test (o:acc) rest (ntok dta)
    test acc lst o@(TokFailed {}) = o:acc
    test acc lst o@(PartialResult _ ntok _) = test (o:acc) lst ntok

specBase :: Spec
specBase = describe "Basic parsing" $ do
  it "Parses null values" $ do
    let test = "[null,3]"
        res = parse (array value) test :: [Maybe Int]
    res `shouldBe` [Nothing, Just 3 ]

  it "Parses bool values" $ do
    let test = "[true,false]"
        res = parse (array value) test :: [Bool]
    res `shouldBe` [True, False]

  it "Parses string values with special chracters" $ do
    let test = "['" `BS.append` (encodeUtf8 "žluť")  `BS.append` "', '\\n\\b\\r\\'', '\\u0041\\u0078\\u0161']"
        res = parse (array value) test :: [T.Text]
    res `shouldBe` ["\382lu\357","\n\b\r\"","Ax\353"]

  it "Parses fractional values with exponent" $ do
    let test = "[1, 2.5, -3.6, 6e1, -3.2e-2]"
        res = parse (array value) test :: [Double]
    show res `shouldBe` "[1.0,2.5,-3.6,60.0,-3.2e-2]"

  it "Parses objects 1" $ do
    let test = "{'key1': 'value2', 'key2': 'value2'}"
        res = parse (objectItems value) test :: [(T.Text, AE.Value)]
    res `shouldBe` [("key1",String "value2"),("key2",String "value2")]

specObjComb :: Spec
specObjComb = describe "Object accesors" $ do
  it "objectWithKey works" $ do
    let test = "[{'name': 'John', 'age': 20}, {'age': 30, 'name': 'Frank' } ]"
        msg = parse (array $ (,) <$> objectWithKey "name" value <*> objectWithKey "age" value) test :: [(T.Text,Int)]
    msg `shouldBe` [("John",20),("Frank",30)]

  it "yield test 1" $ do
    let test = "[{'key1': [1,2,3], 'key2': [5,6,7]}]"
        msg1 = parse (array $ objectItems value) test :: [(T.Text, [Int])]
        msg2 = parse (array $ objectItems $ array value) test :: [(T.Text, Int)]
    msg1 `shouldBe` [("key1",[1,2,3]),("key2",[5,6,7])]
    msg2 `shouldBe` [("key1",1),("key1",2),("key1",3),("key2",5),("key2",6),("key2",7)]

  it "<*> test 1 reverse keys" $ do
    let test = "[{'key1': [1,2], 'key2': [5,6], 'key3': [8,9]}]"
        parser = array $ (,) <$> objectWithKey "key2" (array value) <*> objectWithKey "key1" (array value)
        msg = parse parser test :: [(Int, Int)]
    msg `shouldBe` [(6,2),(6,1),(5,2),(5,1)]

  it "<|> test 1" $ do
    let test = "[{'key1': [1,2], 'key2': [5,6], 'key3': [8,9]}]"
        parser = array $ objectWithKey "key1" (array value) <|> objectWithKey "key2" (array value)
        msg = parse parser test :: [Int]
    msg `shouldBe` [1,2,5,6]

-- specEdge :: Spec
-- specEdge = describe "Edge cases"
--   it "Correct incremental parsing 1" $ do
--   it "Correct incremental parsing 2" $ do
--
--   it "Correctly returns unparsed data 1" $ do
--   it "Correctly returns unparsed data 2" $ do
--   it "Correctly returns unparsed data 3" $ do

spec = do
  specBase
  specObjComb
  -- specEdge

main :: IO ()
main = do
  hspec spec
