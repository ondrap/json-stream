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
