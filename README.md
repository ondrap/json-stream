# json-stream - Applicative incremental JSON parser for Haskell

[![Hackage](https://img.shields.io/hackage/v/json-stream.svg)](https://hackage.haskell.org/package/json-stream)

# When to use this library

- use [aeson](https://hackage.haskell.org/package/aeson) if you can; compile aeson with `cffi` flag if you need better performance
- use `json-stream` if you
  - need streaming
  - need every bit of performance (do profile; the best course could be using the aeson `value` parser with json-stream)
  - do not care that parsing may not fail on malformed JSON data
  - do not need advanced error reporting; json-stream tends to skip data that
    doesn't fit parsing rules (this might be implemented better in the future)

# Intro

Most haskellers use the excellent [aeson](https://hackage.haskell.org/package/aeson) library
to decode and encode JSON structures. Unfortunately, although very fast, this parser
must read the whole structure into memory. Json-stream allows incremental parsing.

> Parsing performance is generally better than aeson, sometimes significantly better.
> Json-stream uses a small and fast C lexer to parse the JSON into tokens. This results
> in quite significant performance gain. Ideal scenario is parsing large files
> where not all information is required; json-stream parses only what is really needed.

Standard aeson library reads the whole input, creates an object in memory representing
the JSON structure which is then converted into proper values using FromJSON instances.
This library is compatible with aeson - you can immediately use FromJSON instances almost without
any change in code and enjoy incremental parsing. The real strength is in the applicative interface
which allows to parse only those parts of JSON that are of interest while skipping what is not needed.

The parsing process uses the least amount of memory possible and is completely lazy. It does not perfectly
check for JSON syntax and the behaviour on incorrect JSON input is undefined (it cheats quite a lot;
but this is needed for constant-space parsing). **The result on badly formed input is undefined,
the parser does not guarantee failing on bad input.**

- The parser generally does not fail. If the data does not match, the parser silently ignores it.
  The failures should be only syntax errors in JSON.
- The ',' and ':' characters in the lexer are treated as white-space.
- When a value is not needed to be parsed, it is parsed by a parser counting braces and brackets.
  Anything can happen, the parser just waits for the sum of openings to equal sum of closings.
- The length of an object key is limited to ~64K, records with longer keys are ignored.

## Motivation

Result of ElasticSearch bulk operations is a large JSON with this structure:
```json
{
  "took":42,
  "errors":true,
  "items": [
    {"index": {"_index":"test","_type":"type1","_id":"3","status":400,"error":"Some error "}},
    {"index":{"_index":"test","_type":"type1","_id":"4","_version":2,"status":200}}
  ]
}
```

We want the parser to return an empty list immediately when it encounters the *errors* key
and the value is *false*. If the value is *true*, we want the parser to return a list of
`_id` keys with an error status.


```haskell
-- | Result of bulk operation
resultParser :: Parser [(Text, Text)]
resultParser =    const [] <$> filterI not ("errors" .: bool)
              <|> many ("items" .: arrayOf bulkItemError)

bulkItemError :: Parser (Text, Text)
bulkItemError = objectWithKey "index" $
    (,) <$> "_id"   .: string
        <*> "error" .: string
        <*  filterI statusError ("status" .: integer)
  where
    statusError s = s < 200 || s > (299 :: Int)

```
## Performance

The crude lexing is done by a C-optimized code in batches, the
lexed pieces are then parsed using the user-specified parser. Json-stream
is generally slightly faster than aeson. It is significantly faster
in the following scenarios:

- parsing numbers
- parsing strings when aeson is not compiled with `cffi` flag
  (the `cffi` flag of aeson enables fast text decoding borrowed from json-stream)
- parsing only subset of big JSON structures

Json-stream in streaming mode is also much friendlier to the GC.

Using json-stream parser instead of aeson `value` evades the need to build the structure
using aeson `Value` and then converting it to the user-requested structure. Instead
the structure is built on the fly directly during the parsing phase.

Json-stream can optimize certain scenarios. If not all data from the input stream is
required, it is skipped by the parsers.

## Constant space parsing

If the matching parser follows certain rules and the input chunks have limited size,
the parsing should run in constant space. If you have a large JSON structure but need
only small pieces, the parsing can be very fast - when the data does not match what
is expected, it is parsed only by the lexical parser and ignored. The object key
length is limited to 64K, maximum length of a string can be limited with `safeString`
parser. The number of digits in a number is limited to 200.000, longer number will
make the parser fail.

## Examples

```haskell
-- The parseByteString function always returns a list of 'things'.
-- Other functions are available.
>>> :t parseByteString
parseByteString :: Parser a -> ByteString -> [a]

-- 'value' stands for FromJSON instance that will be yielded;
-- most normal types work by default
>>> parseByteString value "[1,2,3]" :: [[Int]]
[[1,2,3]]

-- the parser says we have an 'array of values'; i.e. return each value in array
>>> parseByteString (arrayOf value) "[1,2,3]" :: [Int]
[1,2,3]

-- Use <*> for parallel parsing. Order is not important.
>>> let test = "[{\"name\": \"John\", \"age\": 20}, {\"age\": 30, \"name\": \"Frank\"} ]"
>>> let parser = arrayOf $ (,) <$> "name" .: value
                               <*> "age" .: value
>>> parseByteString parser test :: [(Text,Int)]
[("John",20),("Frank",30)]

-- Use objectOf for parsing objects (it's faster than parallel parsing).
>>> let test = "[{\"name\": \"John\", \"age\": 20}, {\"age\": 30, \"name\": \"Frank\"} ]"
>>> let parser = arrayOf $ objectOf $ (,) <$> "name" .: value
                                          <*> "age" .: value
>>> parseByteString parser test :: [(Text,Int)]
[("John",20),("Frank",30)]

-- If you have more results returned from each branch, all are combined.
>>> let test = "[{\"key1\": [1,2], \"key2\": [5,6], \"key3\": [8,9]}]"
>>> let parser = arrayOf $ (,) <$> "key2" .: (arrayOf value)
                               <*> "key1" .: (arrayOf value)
>>> parseByteString parser test :: [(Int, Int)]
[(6,2),(6,1),(5,2),(5,1)]

-- Use <> to return both branches
let test = "[{\"key1\": [1,2], \"key2\": [5,6], \"key3\": [8,9]}]"
>>> let parser = arrayOf $    "key1" .: (arrayOf value)
                           <> "key2" .: (arrayOf value)
>>> parseByteString parser test :: [Int]
[1,2,5,6]

-- objectItems function enriches value with object key
>>> let test = "[{\"key1\": [1,2,3], \"key2\": [5,6,7]}]"
>>> parseByteString (arrayOf $ objectItems value) test :: [(Text, [Int])]
[("key1",[1,2,3]),("key2",[5,6,7])]
>>> parseByteString (arrayOf $ objectItems $ arrayOf value) test :: [(Text, Int)]
[("key1",1),("key1",2),("key1",3),("key2",5),("key2",6),("key2",7)]

-- .:? produces a maybe value; Nothing if match is not found or is null.
-- .| produces a default value if the preceding didn't produce anything
>>> let test = "[{\"name\":\"John\", \"value\": 12}, {\"name\":\"name2\"}, {\"value\":12}]"
>>> let parser = arrayOf $ (,) <$> "name"  .:? string
                               <*> "value" .: integer .| 0
>>> parseByteString parser test :: [(Maybe Text, Int)]
[(Just "John",12),(Just "name2",0),(Nothing,12)]

-- And it works the same with the objectOf parser
>>> let test = "[{\"name\":\"John\", \"value\": 12}, {\"name\":\"name2\"}, {\"value\":12}]"
>>> let parser = arrayOf $ objectOf $ (,) <$> "name"  .:? string
                                          <*> "value" .: integer .| 0
>>> parseByteString parser test :: [(Maybe Text, Int)]
[(Just "John",12),(Just "name2",0),(Nothing,12)]

```

See [haddocks](https://hackage.haskell.org/package/json-stream) documentation for more details.
