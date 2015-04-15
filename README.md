# json-stream - Applicative incremental JSON parser for Haskell

Standard aeson parsing library reads the whole input, creates an object in memory representing
the JSON structure which is then converted into proper values using FromJSON instances.
This library is compatibile with aeson - you can immediately use FromJSON instances almost without
any change in code and enjoy incremental parsing. The real strength is in the applicative interface
which allows to parse only those parts of JSON that are of interest while skipping what is not needed.

The parsing process uses the least amount of memory possible and is completely lazy. It does not perfectly
check for JSON syntax and the behaviour on incorrect JSON input is undefined. In particular:

- I have not found a function in haskell that would have the signature `ByteString -> Maybe Text`.
  Parsing incorrect unicode casuses an exception which is rather inconvenient.
  Json-stream uses lenientDecoding that replaces incorrect unicode characters.

- Especially when parsing values that are supposed to be ignored the parser does not check
  syntax. As such it will likely not fail when encountering certain errors.
