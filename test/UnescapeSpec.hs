{-# LANGUAGE OverloadedStrings #-}

module UnescapeSpec where

import Data.Text.Encoding (encodeUtf8)
import qualified Data.Text as T
import Data.Either (isLeft)
import qualified Data.ByteString as BS

import Test.Hspec
import Data.JsonStream.Unescape

spec :: Spec
spec = do
  describe "Correctly converts correct data" $ do
    it "Converts empty string" $
      unescapeText "" `shouldBe` Right ""
    it "Converts ascii text" $
      unescapeText "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ" `shouldBe` Right "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ"
    it "Converts UTF-8 data" $ do
      let txt = "žluťoučký kůň úpěl ďábelské kódy" :: T.Text
      unescapeText (encodeUtf8 txt) `shouldBe` Right txt
    it "Converts backslash chars" $
      unescapeText "abc\\b\\n\\r\\t\\\\\\/\\\"a konec" `shouldBe` Right "abc\b\n\r\t\\/\"a konec"
    it "Converts surrogate chars" $
      unescapeText "\\ud801\\udc37" `shouldBe` Right "𐐷"

  describe "Fails on incorrect JSON backslashing" $ do
    it "Fails on backslash at end of string" $
      unescapeText "aaa\\" `shouldSatisfy` isLeft
    it "Fails on incorrect backslash char" $
      unescapeText "asfdjfk\\xyyy" `shouldSatisfy` isLeft
    it "Fails on incomplete unicode" $
      unescapeText "aaa\\u1" `shouldSatisfy` isLeft
    it "Fails on incomplete unicode 2" $
      unescapeText "aaa\\u1Xyz" `shouldSatisfy` isLeft

    it "Fails on unexpected lower surrogate" $
      unescapeText "\\udc37\\ud801" `shouldSatisfy` isLeft
    it "Fails on uncompleted surrogate" $
      unescapeText "\\ud801" `shouldSatisfy` isLeft
    it "Fails on uncompleted surrogate 2" $
      unescapeText "\\ud801a" `shouldSatisfy` isLeft
    it "Fails on uncompleted surrogate 3" $
      unescapeText "\\ud801\\u0012" `shouldSatisfy` isLeft
  describe "Fails on incorrect data UTF8" $ do
    it "Fails on bad utf-8" $ do
      let txt = "žluťoučký kůň úpěl ďábelské kódy" :: T.Text
      unescapeText (BS.drop 1 $ encodeUtf8 txt) `shouldSatisfy` isLeft

main :: IO ()
main = hspec spec
