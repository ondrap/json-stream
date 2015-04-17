{-# LANGUAGE OverloadedStrings #-}
module ParserSpec where

import Control.Applicative
import Test.Hspec
import qualified Data.Aeson as AE
import Data.Aeson (Value(..))
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text as T
import Data.Text.Encoding (encodeUtf8)
import Control.Monad (forM_)

import Data.JsonStream.Parser
import Data.JsonStream.TokenParser

-- During the tests the single quotes are replaced with double quotes in the strings as
-- otherwise it would be unreadable in haskell

todquotes :: BS.ByteString -> BS.ByteString
todquotes = BS.map squotes
  where
    squotes '\'' = '"'
    squotes x = x


parse parser text = parseByteString parser (todquotes text)

testRemaining :: Parser a -> BS.ByteString -> BS.ByteString
testRemaining parser startdata = loop (runParser' parser startdata)
  where
    loop (ParseNeedData _) = error "Not enough data."
    loop (ParseDone rem1) = rem1
    loop (ParseFailed err) = error err
    loop (ParseYield _ np) = loop np


specBase :: Spec
specBase = describe "Basic parsing" $ do
  it "Parses null values" $ do
    let test = "[null,3]"
        res = parse (arrayOf value) test :: [Maybe Int]
    res `shouldBe` [Nothing, Just 3 ]

  it "Parses bool values" $ do
    let test = "[true,false]"
        res = parse (arrayOf value) test :: [Bool]
    res `shouldBe` [True, False]

  it "Parses string values with special chracters" $ do
    let test = "['" `BS.append` (encodeUtf8 "žluť")  `BS.append` "', '\\n\\b\\r\\'', '\\u0041\\u0078\\u0161']"
        res = parse (arrayOf value) test :: [T.Text]
    res `shouldBe` ["\382lu\357","\n\b\r\"","Ax\353"]

  it "Parses fractional values with exponent" $ do
    let test = "[1, 2.5, -3.6, 6e1, -3.2e-2]"
        res = parse (arrayOf value) test :: [Double]
    show res `shouldBe` "[1.0,2.5,-3.6,60.0,-3.2e-2]"

  it "Parses objects 1" $ do
    let test = "{'key1': 'value2', 'key2': 'value2'}"
        res = parse (objectItems value) test :: [(T.Text, AE.Value)]
    res `shouldBe` [("key1",String "value2"),("key2",String "value2")]

specObjComb :: Spec
specObjComb = describe "Object accesors" $ do
  it "objectWithKey works" $ do
    let test = "[{'name': 'John', 'age': 20}, {'age': 30, 'name': 'Frank' } ]"
        msg = parse (arrayOf $ (,) <$> objectWithKey "name" value <*> objectWithKey "age" value) test :: [(T.Text,Int)]
    msg `shouldBe` [("John",20),("Frank",30)]

  it "yield test 1" $ do
    let test = "[{'key1': [1,2,3], 'key2': [5,6,7]}]"
        msg1 = parse (arrayOf $ objectItems value) test :: [(T.Text, [Int])]
        msg2 = parse (arrayOf $ objectItems $ arrayOf value) test :: [(T.Text, Int)]
    msg1 `shouldBe` [("key1",[1,2,3]),("key2",[5,6,7])]
    msg2 `shouldBe` [("key1",1),("key1",2),("key1",3),("key2",5),("key2",6),("key2",7)]

  it "<*> test 1 reverse keys" $ do
    let test = "[{'key1': [1,2], 'key2': [5,6], 'key3': [8,9]}]"
        parser = arrayOf $ (,) <$> objectWithKey "key2" (arrayOf value) <*> objectWithKey "key1" (arrayOf value)
        msg = parse parser test :: [(Int, Int)]
    msg `shouldBe` [(6,2),(6,1),(5,2),(5,1)]

  it "<|> test 1" $ do
    let test = "[{'key1': [1,2], 'key2': [5,6], 'key3': [8,9]}]"
        parser = arrayOf $ objectWithKey "key1" (arrayOf value) <|> objectWithKey "key2" (arrayOf value)
        msg = parse parser test :: [Int]
    msg `shouldBe` [1,2,5,6]

specEdge :: Spec
specEdge = describe "Edge cases" $ do
  it "Correct incremental parsing 1" $ do
    let msg1 = "[ {\"test1\"  :[1,true,false,null,-3.591e+1,[12,13]], \"test2\":\"123\\r\\n\\\"\\u0041\"}]"
        pmsg = BL.fromChunks $ map BS.singleton msg1
        res = parseLazyByteString value pmsg :: [AE.Value]
    show res `shouldBe` "[Array (fromList [Object fromList [(\"test2\",String \"123\\r\\n\\\"A\"),(\"test1\",Array (fromList [Number 1.0,Bool True,Bool False,Null,Number -35.91,Array (fromList [Number 12.0,Number 13.0])]))]])]"

  it "Correctly skips data" $ do
    let msg1 = "[{\"123\":[1,2,[3,4]]},11]"
        res = parseByteString (arrayWithIndex 0 (objectValues (arrayOf $ pure "x")) <|> arrayWithIndex 1 (pure "y") <|> arrayOf (pure "z")) msg1 :: [String]
    res `shouldBe` ["x", "x", "x", "y", "z", "z"]

  it "Correctly returns unparsed data 1" $ do
    let msg1 = "[{\"123\":[1,2,[3,4]]},11] "
        rem1 = testRemaining (pure "x" :: Parser String) msg1
    rem1 `shouldBe` " "
  it "Correctly returns unparsed data 2" $ do
    let msg1 = "[{\"123\":[1,2,[3,4]]},11] !x!"
        rem1 = testRemaining (pure "x" :: Parser String) msg1
    rem1 `shouldBe` " !x!"
  it "Correctly returns unparsed data 3" $ do
    let msg1 = "[{\"123\":[1,2,[3,4]]},11] 25 "
        rem1 = testRemaining (pure "x" :: Parser String) msg1
    rem1 `shouldBe` " 25 "
  it "Correctly returns unparsed data 4" $ do
    let msg1 = "[{\"123\":[1,2,[3,4]]},11] 25"
        rem1 = testRemaining (pure "x" :: Parser String) msg1
    rem1 `shouldBe` " 25"
  it "Correctly returns unparsed data 4" $ do
    let msg1 = "[{\"123\":[1,2,[3,4]]},11] \""
        rem1 = testRemaining (pure "x" :: Parser String) msg1
    rem1 `shouldBe` " \""
  it "Correctly returns unparsed data 4" $ do
    let msg1 = "[{\"123\":[1,2,[3,4]]},11] \"aa\""
        rem1 = testRemaining (pure "x" :: Parser String) msg1
    rem1 `shouldBe` " \"aa\""

  it "Handles values in interleaving order" $ do
    let msg1 = BL.fromChunks ["{\"err\":true,\"values\":[1,2,3",   "4,5,6,7]}"]
        parser = (Right <$> objectWithKey "values" (arrayOf value))
                  <|> (Left <$> objectWithKey "err" value)
        res = parseLazyByteString parser msg1 :: [Either Bool Int]
    res `shouldBe` [Right 1,Right 2,Left True,Right 34,Right 5,Right 6,Right 7]

specControl :: Spec
specControl = describe "Control parser" $ do
  -- it "toList" $ do
  -- it "fileterI" $ do
  it "defaultValue" $ do
    let test = "[{\"key1\":\"value1\", \"key2\":\"value2\"}, {\"key1\":\"test\"}, {}]"
        parser = arrayOf $ (,) <$> defaultValue "default-key1" (objectWithKey "key1" value)
                             <*> defaultValue "default-key2" (objectWithKey "key2" value)
        res = parse parser test :: [(T.Text, T.Text)]
    res `shouldBe` [("value1","value2"),("test","default-key2"),("default-key1","default-key2")]

  it "takeI limits number of values" $ do
    let test = "[[1,2,3], [4,5,6]]"
        parser = arrayOf $ takeI 2 $ arrayOf integer
        res = parse parser test :: [Int]
    res `shouldBe` [1,2,4,5]

  it "ignores non-match for array" $ do
    let test = "[1,2,[3,4,5]]"
        parser = arrayOf (arrayOf value)
        res = parse parser test :: [Int]
    res `shouldBe` [3,4,5]

  it "ignores non-match for object" $ do
    let test = "[1,2,{\"test\": 3}]"
        parser = arrayOf $ objectWithKey "test" value
        res = parse parser test :: [Int]
    res `shouldBe` [3]
  it "ignores non-match for string" $ do
    let test = "[1,2,[\"a\", 3, null], \"test\",{}, \"test2\"]"
        res = parse (arrayOf string) test :: [T.Text]
    res `shouldBe` ["test", "test2"]
  it "ignores non-match for number" $ do
    let test = "[{\"aa\":3},2,3,\"test\",4, \"test2\"]"
        res = parse (arrayOf integer) test :: [Int]
    res `shouldBe` [2,3,4]
  it "ignores non-match for bool" $ do
    let test = "[1,[],true,\"test\",{\"t\":true}, \"test2\",false]"
        res = parse (arrayOf bool) test :: [Bool]
    res `shouldBe` [True, False]
  it "nullable sets values correctly" $ do
    let test = "[1,2,null,\"test\",null,3,[],{}]"
        res = parse (arrayOf $ nullable integer) test :: [Maybe Int]
    res `shouldBe` [Just 1, Just 2, Nothing, Nothing, Just 3]

-- Tests of things that were found to be buggy
errTests :: Spec
errTests = describe "Tests of previous errors" $ do
  it "arrayOf (pure True) should return only n*True, not (n+1)" $ do
    let test1 = "[]"
        res1 = parse (arrayOf (pure True)) test1 :: [Bool]
    length res1 `shouldBe` 0
    let test2 = "[{},2,3,[]]"
        res2 = parse (arrayOf (pure True)) test2 :: [Bool]
    length res2 `shouldBe` 4
  it "objectWithKey should return only first key with given name" $ do
    let test1 = "{\"test1\":1, \"test2\":2, \"test1\": 3}"
        res1 = parse (objectWithKey "test1" value) test1 :: [Int]
    res1 `shouldBe` [1]

  it "binds correctly convenience operators" $ do
    let test1 = "[{\"name\": \"test1\", \"value\": 1}, {\"name\": \"test2\", \"value\": null}, {\"name\": \"test3\"}, {\"name\": \"test4\", \"value\": true}]"
        parser = arrayOf $ (,) <$> "name" .: string
                               <*> "value" .:? integer .!= (-1)
        res = parse parser test1 :: [(T.Text, Int)]
    res `shouldBe` [("test1",1),("test2",-1),("test3",-1),("test4",-1)]

aeCompare :: Spec
aeCompare = describe "Compare parsing of strings aeason vs json-stream" $ do
  let values = [
          "{}"
        , "{ \"v\":\"1\"} "
        , "{ \"v\":\"1\"\r\n} "
        , "{ \"v\":1}"
        , "{ \"v\":\"ab'c\"}"
        , "{ \"PI\":3.141E-10}"
        , "{ \"PI\":3.141e-10}"
        , "{ \"v\":12345123456789} "
        , "{ \"v\":123456789123456789123456789}"
        , "[ 1,2,3,4] "
        , "[ \"1\",\"2\",\"3\",\"4\"] "
        , "[ { }, { },[]] "
        , "{ \"v\":\"\\u2000\\u20ff\"} "
        , "{ \"v\":\"\\u2000\\u20FF\"} "
        , "{ \"a\":\"hp://foo\"} "
        , "{ \"a\":null} "
        , "{ \"a\":true} "
        , "  { \"a\" : true }   "
        , "{ \"v\":1.7976931348623157E308} "
        ]
  forM_ values $ \test -> it ("Parses " ++ show test ++ " the same as aeson") $ do
    let resStream = head $ parseByteString value test :: AE.Value
    let Just resAeson = AE.decode (BL.fromChunks [test])
    resStream `shouldBe` resAeson

spec :: Spec
spec = do
  specBase
  specObjComb
  specEdge
  specControl
  errTests
  aeCompare

main :: IO ()
main = do
  hspec spec
