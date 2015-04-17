# json-stream - Applicative incremental JSON parser for Haskell

Most haskellers use the excellent [aeson](https://hackage.haskell.org/package/aeson) library
to decode and encode JSON structures. Unfortunately, although very fast, this parser
must read the whole structure into memory. At a first sight it seemed that creating
an incremental JSON parser would be very hard thing to do; it turned out to be
remarkable easy. Just wondering, why nobody came with this earlier...

> Parsing is
> from 40% faster to ~50% slower than aeson, depending on the parser
> grammar and the test. Generally, parsing small pieces is faster with aeson, parsing
> large structures consisting of smaller objects works better with
> json-stream because of lower memory
> consumption and less stress on the GC. Creating parsers for complex
> JSON structures can be much easier using json-stream.
>
> Counting number of array elements in 120MB
> JSON file (1M elements) needed 1.7GB in aeson, 1.5GB with json-stream in the aeson mode
> (the grammar being just `value`; in reality json-stream needed more memory, the GC just did the job
> differently). 700MB was needed when json-stream grammar was used
> and only 2MB in streaming mode when parsed data was discarded after processing.

Standard aeson library reads the whole input, creates an object in memory representing
the JSON structure which is then converted into proper values using FromJSON instances.
This library is compatibile with aeson - you can immediately use FromJSON instances almost without
any change in code and enjoy incremental parsing. The real strength is in the applicative interface
which allows to parse only those parts of JSON that are of interest while skipping what is not needed.

The parsing process uses the least amount of memory possible and is completely lazy. It does not perfectly
check for JSON syntax and the behaviour on incorrect JSON input is undefined (it cheats quite a lot;
but this is needed for constant-space parsing). **The result on badly formed input is undefined,
the parser is not guaranteed to fail on bad input.**

- The parser generally does not fail. If the data does not match, the parser silently ignores it.
  The failures should be only syntax errors in JSON.
- The ',' character in the lexer is treated as white-space.
- When a value is not needed to be parsed, it is parsed by a parser counting braces and brackets.
  Anything can happen, the parser just waits for the sum of openings to equal sum of closings.

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

We want the parser to return an empty list as soon as it encounters the *errors* key
and the value is *false*. If the value is *true*, we want the parser to return a list of
`_id` keys with an error status. Then we can just take the first value from
the parser and close the HTTP connection.


```haskell
-- | Result of bulk operation
resultParser :: Parser [(Text, Text)]
resultParser = (const [] <$> filterI not ("errors" .: value))
              <|> toList ("items" .: arrayOf bulkItemError)

bulkItemError :: Parser (Text, Text)
bulkItemError = objectValues $
    (,) <$> "_id" .: value
        <*> "error" .:? value .!= "Unknown error."
        <*  filterI statusError ("status" .: value)
  where
    statusError s = s < 200 || s > (299 :: Int)

```
## Constant space parsing

If the matching grammar follows certain rules and the input chunks have limited size,
the parsing should run in constant space. If you have a large JSON structure but need
only small pieces, the parsing can be very fast - when the data does not match what
is expected, it is parsed only by the lexical parser and ignored.

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
-- JSON: [{"name": "John", "age": 20}, {"age": 30, "name": "Frank"} ]
>>> let parser = arrayOf $ (,) <$> "name" .: value
                               <*> "age" .: value
>>> parseByteString  parser (..json..) :: [(Text,Int)]
[("John",20),("Frank",30)]

-- If you have more results returned from each branch, all are combined.
-- JSON: [{"key1": [1,2], "key2": [5,6], "key3": [8,9]}]
>>> let parser = arrayOf $ (,) <$> "key2" .: (arrayOf value)
                               <*> "key1" .: (arrayOf value)
>>> parse parser (..json..) :: [(Int, Int)]
[(6,2),(6,1),(5,2),(5,1)]

-- Use <|> to return both branches
>>> let parser = arrayOf $     "key1" .: (arrayOf value)
                           <|> "key2" .: (arrayOf value)
>>> parse parser test :: [Int]
[1,2,5,6]

-- objectItems function enriches value with object key
-- JSON: [{"key1": [1,2,3], "key2": [5,6,7]}]
>>> parseByteString (arrayOf $ objectItems value) (..json..):: [(Text, [Int])]
[("key1",[1,2,3]),("key2",[5,6,7])]
>>> parseByteString (arrayOf $ objectItems $ arrayOf value) (..json..) :: [(Text, Int)]
[("key1",1),("key1",2),("key1",3),("key2",5),("key2",6),("key2",7)]

-- .:? produces a maybe value; Nothing if match is not found or is null.
-- .!= converts Maybe back with a default
-- JSON: [{"name":"John", "value": 12}, {"name":"name2"}]
>>> let parser = arrayOf $ (,) <$> "name"  .: string
                               <*> "value" .: integer .!= 0
>>> parseByteString parser (..json..) :: [(String,Int)]
[("John",12),("name2",0)]

```

See haddocks documentation for more details.
