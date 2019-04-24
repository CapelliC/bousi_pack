 /**********************************************************************
  * Definitions
  **********************************************************************/

%{
	#include <stdlib.h>
	#include <string.h>
	#include "lexical.h"

	/* Don't include <unistd.h> header for full Windows compatibility */
	#define YY_NO_UNISTD_H

	/* Column number */
	int yycolumnno = 1;

	/* Starting line and column number for comments and strings */
	int yylinestart = 0, yycolumnstart = 0;

	/* Pointer to last quoted or doubled quoted string */
	char* string = NULL;
	/* Capacity and length of "string" buffer */
	int string_capacity = 0, string_length = 0;
	/* Amount of bytes that are allocated for "string" buffer */
	#define STRING_CAPACITY_INCR 100

	/* TRUE when "string", "yylinestart" and "yycolumnstart" should
	   be used instead of "yytext", "yylineno" and "yycolumnno" to
	   get the text and the location of the matched token           */
	bool long_token = FALSE;

	/* Error identifier (used when ERROR token is returned) */
	int custom_error = 0;

	/* This action is executed before every matched rule's action */
	#define YY_USER_ACTION {                                \
		/* Keep tracking of column numbers ("yycolumnno" */ \
		/* must be set to 1 after every new line char)   */ \
		yycolumnno += yyleng;                               \
	}

	/* This action is executed before scanning a new file */
	#define YY_USER_INIT {                               \
		/* Reset custom variables used by the scanner */ \
		yycolumnno = 1;                                  \
		string = NULL;                                   \
		long_token = FALSE;                              \
	}
%}

/* Macros */
SMALL_LETTER         [a-z]
CAPITAL_LETTER       [A-Z]
BINARY_DIGIT         [0-1]
OCTAL_DIGIT          [0-7]
DECIMAL_DIGIT        [0-9]
HEXADECIMAL_DIGIT    [a-fA-F0-9]
ALPHANUMERIC_CHAR    [_a-zA-Z0-9]
GRAPHIC_CHAR         ("$"|"&"|"*"|"+"|"-"|"."|"/"|":"|"<"|"="|">"|"?"|"@"|"^")
NEW_LINE_CHAR        (\r|\n|\r\n)
ESCAPE_CHAR          [abfnrtv\\\'\"]
VARIABLE_INDICATOR   "_"
CONTINUATION_ESCAPE  \\{NEW_LINE_CHAR}
INTEGER_NUMBER       {DECIMAL_DIGIT}{DECIMAL_DIGIT}*
BINARY_NUMBER        {BINARY_DIGIT}{BINARY_DIGIT}*
OCTAL_NUMBER         {OCTAL_DIGIT}{OCTAL_DIGIT}*
HEXADECIMAL_NUMBER   {HEXADECIMAL_DIGIT}{HEXADECIMAL_DIGIT}*
NON_QUOTE_CHAR       ([^\\\'\"\r\n]|\\{OCTAL_NUMBER}\\|\\x{HEXADECIMAL_NUMBER}\\|\\{ESCAPE_CHAR})

/* End scanning after reading a file */
%option noyywrap

/* Keep tracking of line numbers */
%option yylineno

/* Scanner states (aka start conditions) */
%x COMMENT QUOTE DOUBLE_QUOTE

%%

 /**********************************************************************
  * Rules
  **********************************************************************/

 /* INITIAL state */

<INITIAL>{

	"/*"([^*\r\n]|"*"[^/\r\n])* {
		/* Multi-line comment start */
		yylinestart = yylineno;
		yycolumnstart = yycolumnno - yyleng;
		BEGIN COMMENT;
	}

	"%"[^\r\n]*{NEW_LINE_CHAR} {
		/* Single-line comment (comments are ignored) */
		yycolumnno = 1;
	}
	
	"%"[^\r\n]* {
		/* Single-line comment without new line at 
		   the end: end of file (comments are ignored) */
		yycolumnno = 1;
	}
	

	("~1~"|"~2~"|"~3~"|"~>"|"<~"|"~") {
		/* Bousi-Prolog specific operator */
		return TOK_RELATION;
	}

	"#" {
		/* Bousi-Prolog symbol for building linguistic terms */
		return TOK_BUILDER;
	}

	{SMALL_LETTER}{ALPHANUMERIC_CHAR}* {
		/* Identifier */
		return TOK_NAME;
	}
	({GRAPHIC_CHAR}|\\)({GRAPHIC_CHAR}|\\|"#"|"~")* {
		/* 'Graphic' identifier (can't start with "#" nor "~") */
		return TOK_NAME;
	}

	"'" {
		/* Quoted string start */
		yylinestart = yylineno;
		yycolumnstart = yycolumnno - yyleng;
		init_new_string();
		append_string(yytext);
		BEGIN QUOTE;
	}

	";" {
		/* Semicolon */
		return TOK_NAME;
	}
	"!" {
		/* Cut symbol */
		return TOK_NAME;
	}

	({VARIABLE_INDICATOR}|{CAPITAL_LETTER}){ALPHANUMERIC_CHAR}* {
		/* Variable */
		return TOK_VARIABLE;
	}

	{INTEGER_NUMBER} {
		/* Integer constant */
		return TOK_INTEGER;
	}
	"0'"({NON_QUOTE_CHAR}|"''"|"\"") {
		/* Character code constant */
		return TOK_INTEGER;
	}
	"0b"{BINARY_NUMBER} {
		/* Binary constant */
		return TOK_INTEGER;
	}
	"0o"{OCTAL_NUMBER} {
		/* Octal constant */
		return TOK_INTEGER;
	}
	"0x"{HEXADECIMAL_NUMBER} {
		/* Hexadecimal constant */
		return TOK_INTEGER;
	}

	{INTEGER_NUMBER}"."{INTEGER_NUMBER}(("e"|"E")("-"|"+")?{INTEGER_NUMBER})? {
		/* Floating point constant */
		return TOK_FLOAT;
	}

	"\"" {
		/* Double quoted string start */
		yylinestart = yylineno;
		yycolumnstart = yycolumnno - yyleng;
		init_new_string();
		append_string(yytext);
		BEGIN DOUBLE_QUOTE;
	}

	"(" {
		return TOK_LEFT_PARENTHESIS;
	}
	")" {
		return TOK_RIGHT_PARENTHESIS;
	}

	"[" {
		return TOK_LEFT_BRACKET;
	}
	"]" {
		return TOK_RIGHT_BRACKET;
	}

	"," {
		return TOK_COMMA;
	}
	"|" {
		return TOK_LIST_SEPARATOR;
	}

	{NEW_LINE_CHAR} {
		/* New line characters outside strings are ignored */
		yycolumnno = 1;
	}
	[ \t\f]* {
		/* White space is ignored */
	}

	. {
		/* ERROR: Unknown character found */
		custom_error = ERR_UNKNOWN_CHARACTER;
		return TOK_ERROR;
	}

}

 /* COMMENT state */

<COMMENT>{

	"*/" {
		/* Multi-line comment end (comments are ignored) */
		BEGIN INITIAL;
	}

	{NEW_LINE_CHAR} {
		/* Text inside a comment is ignored */
		yycolumnno = 1;
	}

	. {
		/* Text inside a comment is ignored */
	}

	<<EOF>> {
		/* ERROR: End-of-file has been reached before closing comment */
		BEGIN INITIAL;
		custom_error = ERR_UNTERMINATED_COMMENT;
		return TOK_ERROR;
	}

}

 /* QUOTE state */

<QUOTE>{

	{CONTINUATION_ESCAPE} {
		/* Appends text to current quoted string */
		append_string(yytext);
		yycolumnno = 1;
	}

	({NON_QUOTE_CHAR}|"''"|"\"")* {
		/* Appends text to current quoted string */
		append_string(yytext);
	}

	\\[^\r\n] {
		/* Unknown escape sequence (the escape char is copied verbatim) */
		append_string(yytext);
	}

	"'" {
		/* Quoted string end (appends quote and returns token) */
		BEGIN INITIAL;
		append_string(yytext);
		long_token = TRUE;
		return TOK_NAME;
	}

	{NEW_LINE_CHAR} {
		/* ERROR: Found new line character before closing string */
		BEGIN INITIAL;
		custom_error = ERR_UNTERMINATED_STRING;
		yycolumnno = 1;
		return TOK_ERROR;
	}

	<<EOF>> {
		/* ERROR: End-of-file has been reached before closing string */
		BEGIN INITIAL;
		custom_error = ERR_UNTERMINATED_STRING;
		return TOK_ERROR;
	}

}

 /* DOUBLE_QUOTE state */

<DOUBLE_QUOTE>{

	{CONTINUATION_ESCAPE} {
		/* Appends text to current double quoted string */
		append_string(yytext);
		yycolumnno = 1;
	}

	({NON_QUOTE_CHAR}|"\"\""|"'")* {
		/* Appends text to current double quoted string */
		append_string(yytext);
	}

	\\[^\r\n] {
		/* Unknown escape sequence (the escape char is copied verbatim) */
		append_string(yytext);
	}

	"\"" {
		/* Double quoted string end (appends quote and returns token) */
		BEGIN INITIAL;
		append_string(yytext);
		long_token = TRUE;
		return TOK_CHARACTER_CODE_LIST;
	}

	{NEW_LINE_CHAR} {
		/* ERROR: Found new line character before closing string */
		BEGIN INITIAL;
		custom_error = ERR_UNTERMINATED_DQ_STRING;
		yycolumnno = 1;
		return TOK_ERROR;
	}

	<<EOF>> {
		/* ERROR: End-of-file has been reached before closing string */
		BEGIN INITIAL;
		custom_error = ERR_UNTERMINATED_DQ_STRING;
		return TOK_ERROR;
	}

}

%%

 /**********************************************************************
  * User code
  **********************************************************************/

/* Initializes the lexical scanner and prepares it to read from a string. */
void init_scanner(const char* string) {
	yy_scan_string(string);
}

/* Frees all memory used by lexical scanner. */
void destroy_scanner(void) {
#if YY_FLEX_MAJOR_VERSION < 2 \
  || (YY_FLEX_MAJOR_VERSION == 2 && YY_FLEX_MINOR_VERSION < 5) \
  || (YY_FLEX_MAJOR_VERSION == 2 && YY_FLEX_MINOR_VERSION == 5 && \
      (!defined(YY_FLEX_SUBMINOR_VERSION) || YY_FLEX_SUBMINOR_VERSION < 9))
	/* 'yylex_destroy' function is defined in Flex 2.5.9 and later, but not
	   in previous versions, such as the 2.5.4 version used in Windows      */
	yy_delete_buffer(YY_CURRENT_BUFFER);
	yy_init = 1;
	yylineno = 1;
#else
	yylex_destroy();
#endif
}

/* Allocates memory for a new quoted or double quoted string. */
void init_new_string(void) {
	string_capacity = STRING_CAPACITY_INCR;
	string_length = 0;
	string = (char*)malloc(string_capacity);
	string[0] = '\0';
}

/* Appends some text to the current quoted or double quoted string. */
void append_string(char* text) {
	string_length += strlen(text);
	if(string_length + 1 > string_capacity) {
		/* allocates more memory for the string */
		string_capacity += (string_length + 1) - string_capacity + STRING_CAPACITY_INCR;
		string = (char*)realloc(string, string_capacity);
	}
	strcat(string, text);
}

/* Frees the memory space used by the quoted or double quoted string. */
void destroy_string(void) {
	free(string);
	string_capacity = 0;
	string_length = 0;
	string = NULL;
}

