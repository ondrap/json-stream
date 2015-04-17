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


tokenTest :: [BS.ByteString] -> [TokenResult]
tokenTest chunks = reverse $ test [] (tail chunks) (tokenParser $ head chunks)
  where
    test acc [] o@(TokMoreData {}) = o:acc
    test acc (dta:rest) o@(TokMoreData ntok _) = test (o:acc) rest (ntok dta)
    test acc _ o@(TokFailed {}) = o:acc
    test acc lst o@(PartialResult _ ntok _) = test (o:acc) lst ntok

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
  it "catchFail" $ do
    let test = "[{\"key1\":\"pre1\", \"key2\":\"value1\"}, {\"key1\":12}, {\"key1\":\"after1\", \"key2\": \"v2\"}]"
        parser = arrayOf $ (,) <$> (objectWithKey "key1" value)
                             <*> (objectWithKey "key2" value)
        res = parse parser test :: [(T.Text, T.Text)]
    ((last res) `seq` return ()) `shouldThrow` anyException
    let parser2 = arrayOf $ (,) <$> (objectWithKey "key1" $ catchFail value)
                              <*> (objectWithKey "key2" value)
        -- Test it so that we get many calls to MoreData
        pmsg = BL.fromChunks $ map BS.singleton (BS.unpack test)
        res2 = parseLazyByteString parser2 pmsg :: [(T.Text, T.Text)]
    res2 `shouldBe` [("pre1","value1"),("after1","v2")]

-- Tests of things that were found to be buggy
errTests :: Spec
errTests = describe "Tests of previous errors" $
  it "arrayOf (pure True) should return only n*True, not (n+1)" $ do
    let test1 = "[]"
        res1 = parse (arrayOf (pure True)) test1 :: [Bool]
    length res1 `shouldBe` 0
    let test2 = "[{},2,3,[]]"
        res2 = parse (arrayOf (pure True)) test2 :: [Bool]
    length res2 `shouldBe` 4


spec :: Spec
spec = do
  specBase
  specObjComb
  specEdge
  specControl
  errTests

main :: IO ()
main = do
  hspec spec
