{-# LANGUAGE OverloadedStrings #-}

module UnescapeSpec where

import           Control.Applicative      ((<$>))
import qualified Data.Aeson               as AE
import qualified Data.ByteString          as BS
import qualified Data.ByteString.Char8    as BSC
import qualified Data.ByteString.Lazy     as BSL
import           Data.Either              (isLeft)
import qualified Data.Text                as T
import           Data.Text.Encoding       (encodeUtf16BE, encodeUtf8)
import           Numeric                  (showHex)

import           Data.JsonStream.Unescape
import           Test.Hspec
import           Test.QuickCheck
import qualified Test.QuickCheck.Unicode  as QUNI

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
  describe "Fails on incorrect data UTF8" $
    it "Fails on bad utf-8" $ do
      let txt = "žluťoučký kůň úpěl ďábelské kódy" :: T.Text
      unescapeText (BS.drop 1 $ encodeUtf8 txt) `shouldSatisfy` isLeft

  describe "It correctly decodes aeson encoded string" $ do
    it "QuickCheck with aeson encode - standard UTF8" $ do
      let check txt =
              let encoded = BS.init $ BS.tail (BSL.toStrict $ AE.encode (AE.String txt))
              in unescapeText encoded `shouldBe` Right txt
      deepCheck check
    it "QuickCheck with aeson encode - \\u encoded data" $ do
      let check txt = -- Convert everything to \\uXXXX notation
              let u16chars = BS.concat $ map ((BS.append "\\u" . btohex) . BS.take 2)
                              $ take (BS.length u16 `div` 2)
                              $ iterate (BS.drop 2) u16
                  u16 = encodeUtf16BE txt
                  btohex cb = BSC.pack $ concatMap tohex $ BS.unpack cb
                  tohex c
                    | c < 16 = "0" ++ showHex c ""
                    | otherwise = showHex c ""
              in unescapeText u16chars `shouldBe` Right txt
      deepCheck check

deepCheck :: (T.Text -> Expectation) -> IO ()
deepCheck = quickCheckWith (stdArgs { maxSuccess = 10000})

instance Arbitrary T.Text where
  arbitrary = T.pack <$> QUNI.string

main :: IO ()
main = hspec spec
