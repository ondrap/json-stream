module Main where

import Test.DocTest

main :: IO ()
main = doctest [ "-idist/build", "dist/build/doctest/doctest-tmp/c_lib/lexer.o", "dist/build/doctest/doctest-tmp/c_lib/unescape_string.o", "Data/JsonStream/Parser.hs"]
