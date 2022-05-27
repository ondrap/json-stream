#include <stdio.h>
#include <ctype.h>
#include <string.h>
#include <sys/types.h>
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
static inline void add_simple_res(int restype, struct lexer *lexer, int length, struct lexer_result *result)
{
  struct lexer_result *res = &result[lexer->result_num];

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

static inline int handle_base(const char *input, struct lexer *lexer, struct lexer_result *result)
{
  if (handle_space(input, lexer))
    return LEX_OK;

  char chr = input[lexer->position];
  switch (chr) {
    case '{': add_simple_res(RES_OPEN_BRACE, lexer, 1, result); lexer->position++;break;
    case '}': add_simple_res(RES_CLOSE_BRACE, lexer, 1, result); lexer->position++;break;
    case '[': add_simple_res(RES_OPEN_BRACKET, lexer, 1, result); lexer->position++;break;
    case ']': add_simple_res(RES_CLOSE_BRACKET, lexer, 1, result); lexer->position++;break;
    case '"': lexer->current_state = STATE_STRING;
              lexer->state_data = 0;
              lexer->state_data_2 = 0;
              lexer->position++;
              return LEX_OK;
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

static inline int handle_ident(const char *input, struct lexer *lexer, const char *ident, int idtype,
                               struct lexer_result *result)
{
  while (lexer->position < lexer->length) {
    char chr = input[lexer->position];
    if (!ident[lexer->state_data]) {
      // Check that the next character is allowed
      if (isempty(chr) || chr == ']' || chr == '}') {
        add_simple_res(idtype, lexer, lexer->state_data, result);
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
int handle_number(const char *input, struct lexer *lexer, struct lexer_result *result)
{
  /* Just eat characters that can be numbers and feed them to a table */
  // Copy the character to buffer
  int startposition = lexer->position;

  // Try to compute the number fitting to int - 32-bit=9, 64-bit=18
  int maxdigits = sizeof(long) == 8 ? 18 : 9;
  long computedNumber = 0;
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

  struct lexer_result *res = &result[lexer->result_num];
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

/* Handle beginning of a string, the '"' is already stripped
 *
 * state_data:    1 - this is string continuation
 * state_data_2:  1 - we have just skipped the backslash
 */
int handle_string(const char *input, struct lexer *lexer, struct lexer_result *result)
{
    int startposition = lexer->position;
    char ch;
    int hasspecialchar = 0;

    for (ch=input[lexer->position]; lexer->position < lexer->length; ch = input[++lexer->position]) {
      if (ch < 32 || ch > 126)
        hasspecialchar = 1;
      if (lexer->state_data_2)
        lexer->state_data_2 = 0;
      else if (ch == '\\') {
        lexer->state_data_2 = 1;
        hasspecialchar = 1;
      } else if (ch == '"')
        break;
    }

    struct lexer_result *res = &result[lexer->result_num];
    res->startpos = startposition;
    res->length = lexer->position - startposition;
    if (lexer->position < lexer->length && input[lexer->position] == '"') {
      res->restype = RES_STRING;
      if (lexer->state_data)
        res->adddata = 1; // Indicate that we are final portion of the string
      else if (hasspecialchar)
        res->adddata = 0; // Indicate that the string contains escaped/UTF-8 characters
      else
        res->adddata = -1; // Indicate that the stirng is clean ASCII (optimization)

      lexer->result_num++;
      lexer->current_state = STATE_BASE;
      lexer->position++; // Skip the final '"'
      return LEX_OK;
    } else {
      // Emit partial string
      res->restype = RES_STRING_PARTIAL;
      res->adddata = 0;
      lexer->result_num++;

      lexer->state_data = 1;
      return LEX_OK;
    }
}

int lex_json(const char *input, struct lexer *lexer, struct lexer_result *result)
{
  int res = LEX_OK;
  static void* dispatch_table[] = {
      &&state_base, &&state_string, &&state_number, &&state_true,
      &&state_false, &&state_null
  };
  #define DISPATCH() { \
     if (!(lexer->position < lexer->length && lexer->result_num < lexer->result_limit && res == 0)) \
        return res; \
     goto *dispatch_table[lexer->current_state];\
     }

  DISPATCH();
  state_base:
    res = handle_base(input, lexer, result);
    DISPATCH();
  state_string:
    res = handle_string(input, lexer, result);
    DISPATCH();
  state_number:
    res = handle_number(input, lexer, result);
    DISPATCH();
  state_true:
    res = handle_ident(input, lexer, "true", RES_TRUE, result);
    DISPATCH();
  state_false:
    res = handle_ident(input, lexer, "false", RES_FALSE, result);
    DISPATCH();
  state_null:
    res = handle_ident(input, lexer, "null", RES_NULL, result);
    DISPATCH();

  return res;
}
