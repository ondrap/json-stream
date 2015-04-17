# json-stream - Applicative incremental JSON parser for Haskell

> This is still work in progress, the names of the combinators will probably change,
> so that the use is more intuitive.
>
> Parsing is
> from 40% faster to ~50% slower than aeson, depending on the parser
> grammar and the test. Generally, parsing small pieces is faster with aeson, parsing
> large structures works better with json-stream because of lower memory
> consumption and less stress on the GC. Creating parsers for complex
> JSON structures can be much easier using json-stream.
>
> Counting number of array elements in 120MB
> JSON file needed 1.7GB in aeson, 1.5GB with json-stream in the aeson mode
> (the grammar being just `value`). It needed 700MB when json-stream grammar
> was used and only 2MB in streaming mode when parsed data was discarded
> after processing.

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

We want the parser to return en empty list as soon as it encounters the *errors* key
and the value is false. If it is true, we want the parser to return list of
message ids with an error status. Then we can just take the first value from
the parser and close the HTTP connection.


```haskell
-- | Result of bulk operation
resultParser :: Parser [(Text, Text)]
resultParser =   (const [] <$> is not ("errors" .: value))
              <|> toList ("items" .: arrayOf bulkItemErrors)

bulkItemErrors :: Parser (Text, Text)
bulkItemErrors = objectValues $
    (,) <$> "_id" .: value
        <*> defaultValue "Unknown error" ("error" .: value)
        <*  is statusError ("status" .: value)
  where
    statusError s = s < 200 || s > (299 :: Int)

-- Some definitions to make the output nicer - might make it to next version
arrayOf = array
(.:) = objectWithKey
is = filterI
```

## Examples

```haskell
-- Next version will deprecate 'array' in favour of 'arrayOf'
arrayOf :: Parser a -> Parser a
arrayOf = JS.array
-- Next version might define this operator, but there is a clash with aeson .:
(.:) :: T.Text -> Parser a -> Parser a
(.:) = objectWithKey

-- The parseByteString function always returns a list of 'things'.
-- Other functions are available.
>>> :t parseByteString
parseByteString :: Parser a -> BS.ByteString -> [a]

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

-- catchFail can catch an error - it depends where you put it
>>> parseByteString (arrayOf value) "[1,2,true,4,5]" :: [Int]
[1,2*** Exception: when expecting a Int, encountered Boolean instead
>>> parseByteString (catchFail $ arrayOf value) "[1,2,true,4,5]" :: [Int]
[1,2]
>>> parseByteString (arrayOf $ catchFail value) "[1,2,true,4,5]" :: [Int]
[1,2,4,5]

-- defaultValue produces a value if none is found
-- JSON: [{"name":"John", "value": 12}, {"name":"name2"}]
>>> let parser = arrayOf $ (,) <$> "name" .: value
                             <*> defaultValue 0 ("value" .: value)
>>> parseByteString parser (..json..) :: [(String,Int)]
[("John",12),("name2",0)]
```

See haddocks documentation for more details.
