# json-stream - Applicative incremental JSON parser for Haskell

> Current state: the library should be normally usable, the parsing is
> from 40% faster to 30% slower than aeson (depending on the parser grammar).
> In general if you use the applicative parser grammar, it will have lower
> memory consumption and it will be faster. When you use streaming, the lower
> memory consumption becomes significant.
>
> Counting number of array elements in 120MB
> JSON file needed 1.7GB in aeson, 1.5GB with json-stream in the aeson mode
> (the grammar being just `value`). It needed 700MB when json-stream grammar
> was used and only 2MB in streaming mode when parsed data was discarded.

Standard aeson parsing library reads the whole input, creates an object in memory representing
the JSON structure which is then converted into proper values using FromJSON instances.
This library is compatibile with aeson - you can immediately use FromJSON instances almost without
any change in code and enjoy incremental parsing. The real strength is in the applicative interface
which allows to parse only those parts of JSON that are of interest while skipping what is not needed.

The parsing process uses the least amount of memory possible and is completely lazy. It does not perfectly
check for JSON syntax and the behaviour on incorrect JSON input is undefined (it cheats quite a lot).
**The result on badly formed input is undefined.**

- If the parser expects an array and a number is found, it is reported as an error.
  Not finding a particular key in an object (`objectWithKey`) will not be reported as an error.
- The ',' character in the lexer is treated as white-space.
- When a value is not needed to be parsed, it is parsed by a parser counting braces and brackets.
  Anything can happen, the parser just waits for the sum of openings to equal sum of closings.

```haskell
-- The parseByteString function always returns a list of 'things'.
-- Other functions are available.
>>> :t parseByteString
parseByteString :: Parser a -> BS.ByteString -> [a]

-- 'value' stands for FromJSON instance that will be yielded;
-- most normal types work by default
>>> parseByteString value "[1,2,3]" :: [[Int]]
[[1,2,3]]

-- the parser says we have an 'array of values'; i.e. return each value in array
>>> parseByteString (array value) "[1,2,3]" :: [Int]
[1,2,3]

-- Use <*> for parallel parsing. Order is not important.
-- JSON: [{"name": "John", "age": 20}, {"age": 30, "name": "Frank"} ]
>>> let parser = array $ (,) <$> objectWithKey "name" value
                             <*> objectWithKey "age" value
>>> parseByteString  parser (..json..) :: [(Text,Int)]
[("John",20),("Frank",30)]

-- If you have more results returned from each branch, all are combined.
-- JSON: [{"key1": [1,2], "key2": [5,6], "key3": [8,9]}]
>>> let parser = array $ (,) <$> objectWithKey "key2" (array value)
                             <*> objectWithKey "key1" (array value)
>>> parse parser (..json..) :: [(Int, Int)]
[(6,2),(6,1),(5,2),(5,1)]

-- Use <|> to return both branches
>>> let parser = array $ objectWithKey "key1" (array value)
                        <|> objectWithKey "key2" (array value)
>>> parse parser test :: [Int]
[1,2,5,6]

-- objectItems function enriches value with object key
-- JSON: [{"key1": [1,2,3], "key2": [5,6,7]}]
>>> parseByteString (array $ objectItems value) (..json..):: [(Text, [Int])]
[("key1",[1,2,3]),("key2",[5,6,7])]
>>> parseByteString (array $ objectItems $ array value) (..json..) :: [(Text, Int)]
[("key1",1),("key1",2),("key1",3),("key2",5),("key2",6),("key2",7)]

-- catchFail can catch an error - it depends where you put it
>>> parseByteString (array value) "[1,2,true,4,5]" :: [Int]
[1,2*** Exception: when expecting a Int, encountered Boolean instead
>>> parseByteString (catchFail $ array value) "[1,2,true,4,5]" :: [Int]
[1,2]
>>> parseByteString (array $ catchFail value) "[1,2,true,4,5]" :: [Int]
[1,2,4,5]

-- defaultValue produces a value if none is found
-- JSON: [{"name":"John", "value": 12}, {"name":"name2"}]
>>> let parser = array $ (,) <$> objectWithKey "name" value
                             <*> defaultValue 0 (objectWithKey "value" value)
>>> parseByteString parser (..json..) :: [(String,Int)]
[("John",12),("name2",0)]
```

See haddocks documentation for more details.
