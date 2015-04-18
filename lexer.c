#include <stdio.h>
#include <ctype.h>
#include <string.h>

enum states {
  STATE_BASE = 0,
  STATE_STRING,
  STATE_NUMBER,
  STATE_TRUE,
  STATE_FALSE,
  STATE_NULL
};

enum restype {
  RES_NUMBER = 0,
  RES_STRING,
  RES_TRUE,
  RES_FALSE,
  RES_NULL,

  RES_OPEN_BRACE,
  RES_CLOSE_BRACE,
  RES_OPEN_BRACKET,
  RES_CLOSE_BRACKET,

  RES_STRING_START,
  RES_STRING_CONT,
  RES_STRING_END,
  RES_STRING_SPEC,
  RES_STRING_UNI
};

#define RESULT_COUNT 100
#define MAX_NUMBER 64

struct lexer_result {
  int restype;
  int startpos;
  int length;
  int data; // Additional data to result
  char text[MAX_NUMBER]; // Additional text for result
};

struct lexer {
  enum states current_state;
  int position;
  int length;

  int state_data;

  int result_num;
  struct lexer_result result[RESULT_COUNT];
};

typedef enum {
  LEX_OK = 0,
  LEX_YIELD = 1,
  LEX_ERROR = 2
} resstate;

int isempty(char chr) {
  return (chr == ':' || chr == ',' || isspace(chr));
}

// Add simple result to the result list
void add_simple_res(enum restype restype, struct lexer *lexer) {
  lexer->result[lexer->result_num].restype = restype;
  lexer->result[lexer->result_num].startpos = lexer->position;
  lexer->result_num++;
}

resstate handle_space(const char *input, struct lexer *lexer) {
  /* Skip space */
  while (lexer->position < lexer->length && isempty(input[lexer->position]))
    lexer->position++;

  if (lexer->position >= lexer->length)
    return LEX_YIELD;

  lexer->current_state = STATE_BASE;
  return LEX_OK;
}

resstate handle_base(const char *input, struct lexer *lexer) {

  char chr = input[lexer->position];
  switch (chr) {
    case '{': add_simple_res(RES_OPEN_BRACE, lexer); lexer->position++;break;
    case '}': add_simple_res(RES_CLOSE_BRACE, lexer); lexer->position++;break;
    case '[': add_simple_res(RES_OPEN_BRACKET, lexer); lexer->position++;break;
    case ']': add_simple_res(RES_CLOSE_BRACKET, lexer); lexer->position++;break;
    case '"': lexer->current_state = STATE_STRING; lexer->position++;break;
    case 't': lexer->current_state = STATE_TRUE; lexer->state_data = 1; lexer->position++;break;
    case 'f': lexer->current_state = STATE_FALSE; lexer->state_data = 1; lexer->position++;break;
    case 'n': lexer->current_state = STATE_NULL; lexer->state_data = 1; lexer->position++;break;
    case '-': lexer->current_state = STATE_NUMBER; break;
    default:
      if (chr >= '0' && chr <= '9')
        lexer->current_state = STATE_NUMBER;
      else {
        printf("Uknown character: %c\n", chr);
        return LEX_ERROR;
      }
  }
  return LEX_OK;
}

resstate handle_ident(const char *input, struct lexer *lexer, const char *ident, enum restype idtype) {
  char chr = input[lexer->position];
  if (!ident[lexer->state_data]) {
    // Check that the next character is allowed
    if (isempty(chr) || chr == ']' || chr == '}') {
      add_simple_res(idtype, lexer);
      lexer->current_state = STATE_BASE;
      return LEX_OK;
    } else {
      printf("Unexpected next character in handle_ident: %d\n", chr);
      return LEX_ERROR;
    }
  }
  if (ident[lexer->state_data] != chr)
    return LEX_ERROR;
  lexer->state_data++;
  lexer->position++;
  return LEX_OK;
}



resstate lexit(const char *input, struct lexer *lexer) {
  resstate res = LEX_OK;
  while (lexer->position < lexer->length && lexer->result_num < RESULT_COUNT && res == 0) {
    switch (lexer->current_state) {
        case STATE_BASE:
          res = handle_space(input, lexer);
          if (res == LEX_OK)
            res = handle_base(input, lexer);
          break;
        case STATE_TRUE:
          res = handle_ident(input, lexer, "true", RES_TRUE);
          break;
        case STATE_FALSE:
          res = handle_ident(input, lexer, "false", RES_FALSE);
          break;
        case STATE_NULL:
          res = handle_ident(input, lexer, "null", RES_NULL);
          break;
        default:
          printf("Unknown state: %d\n", lexer->current_state);
          return LEX_ERROR;
    }
  }
  return res;
}

char *test1 = "{}";
char *test2 = " [ [ true, false, null  ]]   ";

int test(char *input) {
  struct lexer lexer;
  lexer.position = 0;
  lexer.length = strlen(input);
  lexer.current_state = STATE_BASE;
  lexer.result_num = 0;

  int res = lexit(input, &lexer);
  printf("Result: %d\n", res);
  for (int i=0; i < lexer.result_num; i++) {
    printf("Item: TYPE: %d POS: %d, LEN: %d, DATA: %d\n",
            lexer.result[i].restype, lexer.result[i].startpos, lexer.result[i].length, lexer.result[i].data);
  }
  return 0;
}

int main(void) {
  test(test2);
}
