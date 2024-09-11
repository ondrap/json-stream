# 0.4.6.0

- Show instance for ParseOutput
- Correctly return remaining data for direct String parser (doesn't return correct data for direct Number/Bool/Null parser)

# 0.4.5.3

- lifted upper bounds on aeson

# 0.4.5.2

- user unsafeCoerce to existential wrapper
- fix bad parsing with .| on object

# 0.4.5.1

- fixed testing suite

# 0.4.5.0

- objectOf parser for faster one-pass JSON object parsing
- set minimum base to 4.11

# 0.4.4.2

- aeson-2.1

# 0.4.4.1

- added objectKeyValues (Dylan Simon)
- optimization for reading ASCII strings


# 0.4.4.0

- added text 2.0 compatibility
- added conduit interface behind a flag (Dylan Simon)
- added manyReverse (Dylan Simon)
- added valueWith (Dylan Simon)

# 0.4.3.0

- Aeson 2.0 compatibility
- Added support for raw bytestring

# 0.4.2.4

Fix compiling with new ghc.

# 0.4.2.3

Fix 32-bit number parsing.

# 0.4.2.2

Speed optimization of `many` and aeson object.

# 0.4.2.0
Added Semigroup instance, compatibility with base-4.11

# 0.4.1.5
Renamed `_js_decode_string` function to avoid conflict with `aeson`.

# 0.4.1.4
Added support for GHC 8.2.

# 0.4.1.3
Fix windows build.

# 0.4.1.2
Slightly more strictness in arrayOf.

# 0.4.1.1
Fixed memory leak in arrayOf.

# 0.4.1.0
Added aeson-compatibile encode/decode functions.

# 0.4.0.0
Breaking changes (this could *really* break your code):
- Changed `<|>` to `<>` (`Monoid` is better for 'appending' than `Alternative`)
- Changed `>^>` to `<|>` - (`Alternative` now really means alternative)
- Changed `toList` to `many` (Use existing `Alternative` function instead of a custom one)
- Added `some` function (Alternative, default implementation won't work)
- C-lexer now supports parsing numbers up to 18 digits (E-notation is not optimized yet)

# 0.3.2.3
- Completely rewritten text unescapes based on text decodeUtf8; fixes some surprising crashes, speed improvements.

# 0.3.2.0
- Changed string parsing; parsing of escaped strings is now very fast
- Removed bytestring parser

# 0.3.0.4
- Fixed bug in safestring
- Fixed test so it doesn't depend on versions of other packages
- Added sax-like parsers

# 0.3.0.3
- Fixed wrong size of C structure in FFI that was causing a segfault.
