#include <stdio.h>
#include <ctype.h>
#include <string.h>
#include <sys/types.h>
#include <sys/uio.h>
#include <unistd.h>
#include <fcntl.h>

#include "lexer.h"

/*
 * Batch lexer for JSON
 *
 * When each handle_* function is called, 2 things hold:
 * - at least 1 character is available in the input buffer
 * - at least 1 result slot is free
 *
 *
)
 */

static inline int isempty(char chr)
{
  return (chr == ':' || chr == ',' || isspace(chr));
}

static inline int isJnumber(char chr)
{
  return ((chr >= '0' && chr <= '9') || chr == '-' || chr == '.' || chr == '+' || chr == 'e' || chr == 'E');
}

// Add simple result to the result list
static inline void add_simple_res(int restype, struct lexer *lexer, int length)
{
  struct lexer_result *res = &lexer->result[lexer->result_num];

  res->restype = restype;
  res->startpos = lexer->position;
  res->length = length;
  lexer->result_num++;
}

static inline int handle_space(const char *input, struct lexer *lexer)
{
  /* Skip space */
  while (lexer->position < lexer->length && isempty(input[lexer->position]))
    lexer->position++;

  if (lexer->position >= lexer->length)
    return LEX_YIELD;

  return LEX_OK;
}

static inline int handle_base(const char *input, struct lexer *lexer)
{
  if (handle_space(input, lexer))
    return LEX_OK;

  char chr = input[lexer->position];
  switch (chr) {
    case '{': add_simple_res(RES_OPEN_BRACE, lexer, 1); lexer->position++;break;
    case '}': add_simple_res(RES_CLOSE_BRACE, lexer, 1); lexer->position++;break;
    case '[': add_simple_res(RES_OPEN_BRACKET, lexer, 1); lexer->position++;break;
    case ']': add_simple_res(RES_CLOSE_BRACKET, lexer, 1); lexer->position++;break;
    case '"': lexer->current_state = STATE_STRING; lexer->state_data = 0; lexer->position++;return LEX_OK;
    case 't': lexer->current_state = STATE_TRUE; lexer->state_data = 1; lexer->position++;return LEX_OK;
    case 'f': lexer->current_state = STATE_FALSE; lexer->state_data = 1; lexer->position++;return LEX_OK;
    case 'n': lexer->current_state = STATE_NULL; lexer->state_data = 1; lexer->position++;return LEX_OK;
    default:
      if (isJnumber(chr)) {
        lexer->current_state = STATE_NUMBER;
        lexer->state_data = 0;
        return LEX_OK;
      } else {
        // Unknown character
        return LEX_ERROR;
      }
  }
  return LEX_OK;
}

static inline int handle_ident(const char *input, struct lexer *lexer, const char *ident, int idtype)
{
  while (lexer->position < lexer->length) {
    char chr = input[lexer->position];
    if (!ident[lexer->state_data]) {
      // Check that the next character is allowed
      if (isempty(chr) || chr == ']' || chr == '}') {
        add_simple_res(idtype, lexer, lexer->state_data);
        lexer->current_state = STATE_BASE;
        return LEX_OK;
      } else {
        // Unexpected next character in handle_ident
        return LEX_ERROR;
      }
    }
    if (ident[lexer->state_data] != chr)
      return LEX_ERROR;
    lexer->state_data++;
    lexer->position++;
  }
  return LEX_OK;
}

/* Read a number; compute the number if the 'int' type can hold it */
int handle_number(const char *input, struct lexer *lexer)
{
  /* Just eat characters that can be numbers and feed them to a table */
  // Copy the character to buffer
  int startposition = lexer->position;

  // Try to compute the number fitting to int - 32-bit=9, 64-bit=18
  int maxdigits = sizeof(int) == 8 ? 18 : 9;
  int computedNumber = 0;
  int digits = 0;
  int gotDot = 0;
  int dotDigits = 0;
  int invalid = 0;
  int sign = 1;

  // Do not try on number continuation
  if (lexer->state_data)
      invalid = 1;

  for (;lexer->position < lexer->length && isJnumber(input[lexer->position]);++lexer->position) {
       char ch = input[lexer->position];
       if (!invalid) {
         if (lexer->position == startposition && ch == '-') {
            sign = -1;
         } else if (isdigit(ch)) {
           digits++;
           computedNumber = computedNumber * 10 + (ch - '0');
           if (gotDot)
              dotDigits++;
         } else if (ch == '.' && gotDot == 0) {
            gotDot = 1;
         } else
            invalid = 1; // We do not support E notation to optimize or some syntax error

         if (digits > maxdigits)
            invalid = 1;
        }
     }

  struct lexer_result *res = &lexer->result[lexer->result_num];
  res->adddata = lexer->state_data;
  if (lexer->position == lexer->length) {
    res->restype = RES_NUMBER_PARTIAL;
    // We can just point directly to the input
    res->startpos = startposition;
    res->length = lexer->position - startposition;
    lexer->state_data = 1;
  } else if (!invalid) {
    /* Optimized number generation, so that we don't have to parse it in haskell */
    res->restype = RES_NUMBER_SMALL;
    res->adddata = sign * computedNumber;
    res->length = dotDigits;

    lexer->current_state = STATE_BASE;
  } else {
    res->restype = RES_NUMBER;
    // We can just point directly to the input
    res->startpos = startposition;
    res->length = lexer->position - startposition;

    lexer->current_state = STATE_BASE;
  }

  lexer->result_num++;
  return LEX_OK;
}

static inline int safechar(char x) {
  return (x != '"' && x != '\\');
}

/* Handle beginning of a string, the '"' is already stripped */
int handle_string(const char *input, struct lexer *lexer)
{
    int startposition = lexer->position;
    char ch;
    for (ch=input[lexer->position]; lexer->position < lexer->length && safechar(ch); ch = input[++lexer->position])
      ;

    struct lexer_result *res = &lexer->result[lexer->result_num];
    res->startpos = startposition;
    res->length = lexer->position - startposition;
    if (lexer->position == lexer->length || input[lexer->position] == '\\') {
      // Emit partial string
      res->restype = RES_STRING_PARTIAL;
      res->adddata = 0;
      if (res->length != 0) // Do not add new result, if length == 0
          lexer->result_num++;

      // If we stopped because of backslash, change state, move one forward
      if (lexer->position < lexer->length) {
          lexer->current_state = STATE_STRING_SPECCHAR;
          lexer->state_data = 0;
          lexer->position++;
      } else
          lexer->state_data = 1;
      return LEX_OK;
    } else if (input[lexer->position] == '"') {
      res->restype = RES_STRING;
      res->adddata = lexer->state_data;

      lexer->result_num++;
      lexer->current_state = STATE_BASE;
      lexer->position++; // Skip the final '"'
      return LEX_OK;
    }
    // Internal error, shouldn't get here
    return LEX_ERROR;
}

/* Handle \uxxxx syntax */
static int handle_string_uni(const char *input, struct lexer *lexer)
{
  char chr = input[lexer->position];
  lexer->state_data_2 *= 16;
  if (chr >= 'a' && chr <='f')
    lexer->state_data_2 += 10 + (chr - 'a');
  else if (chr >= 'A' && chr <= 'F')
    lexer->state_data_2 += 10 + (chr - 'A');
  else if (chr >= '0' && chr <= '9')
    lexer->state_data_2 += chr - '0';
  else
    return LEX_ERROR;
  lexer->state_data += 1;
  lexer->position += 1;
  if (lexer->state_data == 4) {
      // Emit the result
      struct lexer_result *res = &lexer->result[lexer->result_num];
      res->startpos = lexer->position;
      res->length = 0;
      res->restype = RES_STRING_UNI;
      res->adddata = lexer->state_data_2;
      lexer->result_num++;

      lexer->current_state = STATE_STRING;
      lexer->state_data = 1; // Set that we are in partial string, see handle_string
  }
  return LEX_OK;
}

// Add a character to result, move position forward, change state back to string
static inline void emitchar(char ch, struct lexer *lexer)
{
  struct lexer_result *res = &lexer->result[lexer->result_num];

  res->restype = RES_STRING_PARTIAL;
  res->startpos = lexer->position;
  res->length = 0;
  res->adddata = ch;

  lexer->result_num++;
  lexer->position++;
  lexer->current_state = STATE_STRING;
  lexer->state_data = 1; // Set the string is in partial data
}

int handle_specchar(const char *input, struct lexer *lexer)
{
  char chr = input[lexer->position];
  switch (chr) {
    case '"': emitchar('"', lexer);break;
    case '\\':emitchar('\\', lexer);break;
    case '/':emitchar('/', lexer);break;
    case 'b':emitchar('\b', lexer);break;
    case 'f':emitchar('\f', lexer);break;
    case 'n':emitchar('\n', lexer);break;
    case 'r':emitchar('\r', lexer);break;
    case 't':emitchar('\t', lexer);break;
    case 'u':
      lexer->current_state = STATE_STRING_UNI;
      lexer->state_data = 0;
      lexer->state_data_2 = 0;
      lexer->position++;
      break;
    default:
      return LEX_ERROR;
  }
  return LEX_OK;
}

int lex_json(const char *input, struct lexer *lexer, struct lexer_result *result)
{
  lexer->result = result;
  int res = LEX_OK;
  static void* dispatch_table[] = {
      &&state_base, &&state_string, &&state_number, &&state_true,
      &&state_false, &&state_null, &&state_string_specchar,
      &&state_string_uni
  };
  #define DISPATCH() { \
     if (!(lexer->position < lexer->length && lexer->result_num < RESULT_COUNT && res == 0)) \
        return res; \
     goto *dispatch_table[lexer->current_state];\
     }

  DISPATCH();
  state_base:
    res = handle_base(input, lexer);
    DISPATCH();
  state_string:
    res = handle_string(input, lexer);
    DISPATCH();
  state_number:
    res = handle_number(input, lexer);
    DISPATCH();
  state_true:
    res = handle_ident(input, lexer, "true", RES_TRUE);
    DISPATCH();
  state_false:
    res = handle_ident(input, lexer, "false", RES_FALSE);
    DISPATCH();
  state_null:
    res = handle_ident(input, lexer, "null", RES_NULL);
    DISPATCH();
  state_string_specchar:
    res = handle_specchar(input, lexer);
    DISPATCH();
  state_string_uni:
    res = handle_string_uni(input, lexer);
    DISPATCH();

  return res;
}
