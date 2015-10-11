#include <string.h>
#include <stdio.h>
#include <stdint.h>


#define UTF8_ACCEPT 0
#define UTF8_REJECT 12

static const uint8_t utf8d[] = {
  // The first part of the table maps bytes to character classes that
  // to reduce the size of the transition table and create bitmasks.
   0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
   0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
   0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
   0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
   1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,  9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,
   7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,  7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,
   8,8,2,2,2,2,2,2,2,2,2,2,2,2,2,2,  2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,
  10,3,3,3,3,3,3,3,3,3,3,3,3,4,3,3, 11,6,6,6,5,8,8,8,8,8,8,8,8,8,8,8,

  // The second part is a transition table that maps a combination
  // of a state of the automaton and a character class to a state.
   0,12,24,36,60,96,84,12,12,12,48,72, 12,12,12,12,12,12,12,12,12,12,12,12,
  12, 0,12,12,12,12,12, 0,12, 0,12,12, 12,24,12,12,12,12,12,24,12,24,12,12,
  12,12,12,12,12,12,12,24,12,12,12,12, 12,24,12,12,12,12,12,12,12,24,12,12,
  12,12,12,12,12,12,12,36,12,36,12,12, 12,36,12,12,12,12,12,36,12,36,12,12,
  12,36,12,12,12,12,12,12,12,12,12,12,
};

static uint32_t inline decode(uint32_t* state, uint32_t* codep, uint32_t byte) {
  uint32_t type = utf8d[byte];

  *codep = (*state != UTF8_ACCEPT) ?
    (byte & 0x3fu) | (*codep << 6) :
    (0xff >> type) & (byte);

  *state = utf8d[256 + *state + type];
  return *state;
}

static int inline ishexnum(uint32_t c)
{
  return (c >= '0' && c <= '9') || (c >= 'a' && c <= 'f') || (c >= 'A' && c <= 'F');
}

static uint32_t inline decode_hex(uint32_t c)
{
  if (c >= '0' && c <= '9')
    return c - '0';
  else if (c >= 'a' && c <= 'f')
    return c - 'a' + 10;
  else if (c >= 'A' && c <= 'F')
    return c - 'A' + 10;
  return 0; // Should not happen
}

static int inline isLowSurrogate(uint16_t c)
{
  return c >= 0xDC00 && c <= 0xDFFF;
}

static int inline isHighSurrogate(uint16_t c)
{
  return c >= 0xD800 && c <= 0xDBFF;
}

// Decode, return non-zero value on error
int
_js_decode_string(uint16_t *const dest, size_t *destoff,
                  const uint8_t *s, const uint8_t *const srcend)
{
  uint16_t *d = dest + *destoff;
  uint32_t state = 0;
  uint32_t codepoint;

  int surrogate = 0;
  uint16_t unidata;

  // Optimized version of dispatch when just an ASCII char is expected
  #define DISPATCH_ASCII(label) {\
    if (s >= srcend) {\
      return -1;\
    }\
    codepoint = *s++;\
    goto label;\
  }

  standard:
    // Test end of stream
    while (s < srcend) {
        if (*s <= 127)
          codepoint = *s++;
        else if (decode(&state, &codepoint, *s++) != UTF8_ACCEPT) {
          if (state == UTF8_REJECT)
            return -1;
          continue;
        }

        if (codepoint == '\\')
          DISPATCH_ASCII(backslash)
        else if (codepoint <= 0xffff)
          *d++ = (uint16_t) codepoint;
        else {
          *d++ = (uint16_t) (0xD7C0 + (codepoint >> 10));
          *d++ = (uint16_t) (0xDC00 + (codepoint & 0x3FF));
        }
    }
    *destoff = d - dest;
    // Exit point
    return (state != UTF8_ACCEPT);
  backslash:
    switch (codepoint) {
      case '"':
      case '\\':
      case '/':
        *d++ = (uint16_t) codepoint;
        goto standard;
        break;
      case 'b': *d++ = '\b';goto standard;
      case 'f': *d++ = '\f';goto standard;
      case 'n': *d++ = '\n';goto standard;
      case 'r': *d++ = '\r';goto standard;
      case 't': *d++ = '\t';goto standard;
      case 'u': DISPATCH_ASCII(unicode1);;break;
      default:
        return -1;
    }
  unicode1:
    if (!ishexnum(codepoint))
      return -1;
    unidata = decode_hex(codepoint) << 12;
    DISPATCH_ASCII(unicode2);
  unicode2:
    if (!ishexnum(codepoint))
      return -1;
    unidata |= decode_hex(codepoint) << 8;
    DISPATCH_ASCII(unicode3);
  unicode3:
    if (!ishexnum(codepoint))
      return -1;
    unidata |= decode_hex(codepoint) << 4;
    DISPATCH_ASCII(unicode4);
  unicode4:
    if (!ishexnum(codepoint))
      return -1;
    unidata |= decode_hex(codepoint);
    *d++ = (uint16_t) unidata;

    if (surrogate) {
      if (!isLowSurrogate(unidata))
        return -1;
      surrogate = 0;
    } else {
      if (isHighSurrogate(unidata)) {
        surrogate = 1;
        DISPATCH_ASCII(surrogate1);
      } else if (isLowSurrogate(unidata))
        return -1;
    }
    goto standard;
  surrogate1:
    if (codepoint != '\\')
      return -1;
    DISPATCH_ASCII(surrogate2)
  surrogate2:
    if (codepoint != 'u')
      return -1;
    DISPATCH_ASCII(unicode1)
}
