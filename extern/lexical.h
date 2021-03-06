#ifndef _LEXICAL_H
#define _LEXICAL_H

#include "bool.h"

/* List of tokens */
#define TOK_RELATION            1
#define TOK_BUILDER             2
#define TOK_NAME                3
#define TOK_VARIABLE            4
#define TOK_INTEGER             5
#define TOK_FLOAT               6
#define TOK_CHARACTER_CODE_LIST 7
#define TOK_LEFT_PARENTHESIS    8
#define TOK_RIGHT_PARENTHESIS   9
#define TOK_LEFT_BRACKET        10
#define TOK_RIGHT_BRACKET       11
#define TOK_COMMA               12
#define TOK_LIST_SEPARATOR      13
#define TOK_ERROR               14
#define TOK_EOF                 15
#define LAST_TOKEN              TOK_EOF

/* Error identifiers */
#define ERR_UNKNOWN_CHARACTER      1
#define ERR_UNTERMINATED_STRING    2
#define ERR_UNTERMINATED_DQ_STRING 3
#define ERR_UNTERMINATED_COMMENT   4

/* Predefined variables declared in the autogenerated lexical scanner */
#ifndef FLEX_SCANNER
extern char* yytext;
extern int yylineno;
extern size_t yyleng;
#endif
/* Custom variables declared in the autogenerated lexical scanner */
extern char* string;
extern int yycolumnno, yylinestart, yycolumnstart;
extern int custom_error;
extern bool long_token;

/* Predefined functions implemented in the autogenerated lexical scanner */
#ifndef FLEX_SCANNER
extern int yylex(void);
#endif
/* Custom functions implemented in the autogenerated lexical scanner */
extern void init_scanner(const char* string);
extern void destroy_scanner(void);
extern void init_new_string(void);
extern void append_string(char* text);
extern void destroy_string(void);

/* Note that Flex 2.5.33 and later can automatically create a header
   file with the declarations of all the predefined variables and
   functions declared in the lexical scanner, but the 2.5.4 version
   used in Windows doesn't have that option                          */

#endif

