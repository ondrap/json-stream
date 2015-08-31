/**
  * Copied from https://github.com/agrafix/highjson project.
  */

#include <stdlib.h>

void bs_json_unescape(const unsigned long length, int *error, char *bsIn, char *bsOut);

inline void private_parse_hex(const char a, const char b, const char c, const char d, int *number, short *error) {
    const char hex[5] = { a, b, c, d, '\0' };
    char *internalError;
    *number = (int)strtol(hex, &internalError, 16);
    if (*internalError != '\0') {
        *error = 1;
    } else {
        *error = 0;
    }
}

#define UTF8_CHAR(N, OUT) \
    if (N < 0x80) { \
        OUT = (char)N; \
    } else if (N < 0x800) { \
        OUT = (char)((N >> 6) | 0xc0); \
        OUT = (char)((N & 0x3F) | 0x80); \
    } else if (N < 0xffff) { \
        OUT = (char)((N >> 12) | 0xe0); \
        OUT = (char)(((N >> 6) & 0x3f) | 0x80); \
        OUT = (char)((N & 0x3f) | 0x80); \
    } else { \
        OUT = (char)((N >> 18) | 0xF0); \
        OUT = (char)(((N >> 12) & 0x3F) | 0x80); \
        OUT = (char)(((N >> 6) & 0x3F) | 0x80); \
        OUT = (char)((N & 0x3f) | 0x80); \
    }

void bs_json_unescape(const unsigned long length, int *error, char *bsIn, char *bsOut)
{
    register unsigned long ptr = 0;
    while (ptr < length) {
        const char ch = *bsIn++;
        ptr++;
        if (ch == '\\') {
            if (ptr >= length) {
                *error = 1;
                return;
            }
            const char nextCh = *bsIn++;
            ptr++;
            switch (nextCh) {
            case '\\':
                *bsOut++ = '\\';
                break;
            case '"':
                *bsOut++ = '"';
                break;
            case '/':
                *bsOut++ = '/';
                break;
            case 'b':
                *bsOut++ = '\b';
                break;
            case 'f':
                *bsOut++ = '\f';
                break;
            case 'n':
                *bsOut++ = '\n';
                break;
            case 'r':
                *bsOut++ = '\r';
                break;
            case 't':
                *bsOut++ = '\t';
                break;
            case 'u':
                if (ptr+3 >= length) {
                    *error = 3;
                    return;
                }
                ptr += 4;
                int number = 0;
                short hexError = 0;
                const char a = *bsIn++; const char b = *bsIn++; const char c = *bsIn++; const char d = *bsIn++;
                private_parse_hex(a, b, c, d, &number, &hexError);
                if (hexError == 1) {
                    *error = 4;
                    return;
                }
                if (number < 0xd800 || number > 0xdfff) {
                    UTF8_CHAR(number, *bsOut++);
                } else if (number <= 0xdbff) {
                    if (ptr+5 >= length) {
                        *error = 5;
                        return;
                    }
                    ptr += 6;
                    if (*bsIn++ != '\\') {
                        *error = 6;
                        return;
                    }
                    if (*bsIn++ != 'u') {
                        *error = 7;
                        return;
                    }
                    int numberB = 0;
                    short hexErrorB = 0;
                    const char ai = *bsIn++; const char bi = *bsIn++; const char ci = *bsIn++; const char di = *bsIn++;
                    private_parse_hex(ai, bi, ci, di, &numberB, &hexErrorB);
                    if (hexErrorB == 1) {
                        *error = 8;
                        return;
                    }
                    if (numberB >= 0xdc00 && numberB <= 0xdfff) {
                        const int r = ((number - 0xdc00) << 10) + (numberB - 0xdc00) + 0x10000;
                        UTF8_CHAR(r, *bsOut++);
                    } else {
                        *error = 9;
                        return;
                    }
                }
                break;
            default:
                *error = 2;
                return;
            }
        } else {
            *bsOut++ = ch;
        }
    }
    *bsOut = '\0';
    *error = 0;
}
