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
