#ifndef _LEXER_H_
#define _LEXER_H_

#define RES_NUMBER  0
#define RES_STRING  1 /* Add-data: 0 - single string, 1 - final part of partials */
#define RES_TRUE    2
#define RES_FALSE   3
#define RES_NULL    4

#define RES_OPEN_BRACE    5
#define RES_CLOSE_BRACE   6
#define RES_OPEN_BRACKET  7
#define RES_CLOSE_BRACKET 8

#define RES_STRING_PARTIAL 9 /* Add-data: 0 - first part, 1 - other parts */
#define RES_NUMBER_PARTIAL 10 /* Add-data: 0 - first part, 1 - other parts */
#define RES_STRING_UNI     11
#define RES_NUMBER_SMALL   12

#define RESULT_COUNT 6000

enum states {
  STATE_BASE = 0,
  STATE_STRING,
  STATE_NUMBER,
  STATE_TRUE,
  STATE_FALSE,
  STATE_NULL,
  STATE_STRING_SPECCHAR,
  STATE_STRING_UNI
};

struct lexer_result {
  int restype;
  int startpos; // Startpos + length should point to unparsed data for } and ]
  int length;

  int adddata; // Additional data to result
};

struct lexer {
  int current_state;
  int state_data;
  int state_data_2;

  int position;
  int length;

  int result_num;
  struct lexer_result *result;
};

#define LEX_OK     0
#define LEX_YIELD  1
#define LEX_ERROR  2

extern int lex_json(const char *input, struct lexer *lexer, struct lexer_result *result);


#endif
