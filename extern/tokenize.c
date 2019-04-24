/***********************************************************************
 * Lexical scanner for Bousi-Prolog programs and queries
 */

#include <SWI-Prolog.h>
#include <SWI-Stream.h>
#include <stdio.h>
#include <string.h>
#include <locale.h>
#include "array.h"
#include "lexical.h"
#include "tokenize.h"

/***********************************************************************/



/***********************************************************************
 * Declarations
 ***********************************************************************/

/* Private function prototypes */
static void create_functors(void);
static void generate_error_message(char *sErrorMessage, char *sTokenText,
                                   int nErrorId);
static void add_token(array *pmTokens, int nType, term_t tText, int nLine,
                      int nColumn, int *pnNumTokens);
static void send_error(char *sMessage, int nLine, int nColumn);

/* List of functors used to build tokens */
static functor_t fTokenFunc[LAST_TOKEN + 1];



/***********************************************************************
 * Public functions
 ***********************************************************************/


/** ext_tokenize(+StringAtom, -Tokens)
 *
 *    Scans the Bousi-Prolog program or query contained in StringAtom,
 *    performs a lexical analysis and returns a list with all the tokens
 *    found. Each token will be defined by a compound term of arity 2
 *    with this syntax: "<type>(<text>, [<line>, <column>])" (e.g.
 *    "comma(',', [3, 5])" or "name('aaa bbb', [10, 15])").
 */
foreign_t pl_tokenize(term_t tStringAtom, term_t tTokens) {
	char sErrorMessage[MAX_ERROR_SIZE + 1];
	array mTokens;
	term_t tTokenList;
	term_t tTokenText;
	term_t tToken;
	char *sInput;
	int nNumTokens;
	int nLine, nColumn, nLastLine, nLastColumn;
	int nToken;
	int i;

	/* Set locale to deal with fractional numbers appropriately in any platform */
	setlocale(LC_NUMERIC, "C");

	/* checks that arguments have the expected types */
	if(!PL_is_atom(tStringAtom) || !(PL_is_variable(tTokens)
	 || PL_is_list(tTokens))) {
		PL_fail;
	}

	/* creates all the functors that will be used to build tokens */
	create_functors();

	/* initializes list of tokens */
	nNumTokens = 0;
	create_array(&mTokens, 1, INI_TOKEN_LIST_SIZE, POINTER_ARRAY);
	tTokenList = PL_new_term_ref();
	tTokenText = PL_new_term_ref();
	PL_put_nil(tTokenList);

	/* sets string to be scanned */
    if ( !PL_get_atom_chars(tStringAtom, &sInput) ) {
        printf("ERROR (module tokenize: pl_tokenize): line 76. \n");
        PL_fail;
    };
	init_scanner(sInput);

	/* main loop */
	while((nToken = yylex()) != 0) {

		if(nToken == TOK_ERROR) {

			/* gets line and column number of current error */
			if(custom_error == ERR_UNTERMINATED_COMMENT
			 || custom_error == ERR_UNTERMINATED_STRING
			 || custom_error == ERR_UNTERMINATED_DQ_STRING) {
				nLine = yylinestart;
				nColumn = yycolumnstart;
			} else {
				nLine = yylineno;
				nColumn = yycolumnno - yyleng;
			}
			/* generates lexical error message */
			generate_error_message(sErrorMessage, yytext, custom_error);
			/* sends error to Prolog */
			send_error(sErrorMessage, nLine, nColumn);

		} else {

			/* converts token text into a term (all tokens are returned
			   as string atoms, except integer and floating point numbers) */
			switch(nToken) {
			case TOK_INTEGER:
                if ( !PL_put_integer(tTokenText, atoi(yytext)) ) {
                    printf("ERROR (module tokenize: pl_tokenize): line 108. \n");
                    PL_fail;
                };
				break;
			case TOK_FLOAT:
                if ( !PL_put_float(tTokenText, atof(yytext)) ) {
                    printf("ERROR (module tokenize: pl_tokenize): line 114. \n");
                    PL_fail;
                };
				break;
			default:
				if(long_token) {
					/* token text is in "string", not in "yytext" */
			 		PL_put_atom_chars(tTokenText, string);
					destroy_string();
				} else {
			 		PL_put_atom_chars(tTokenText, yytext);
				}
				break;
			}

			/* gets line and column number of token's first character */
			if(long_token) {
				nLine = yylinestart;
				nColumn = yycolumnstart;
			} else {
				nLine = yylineno;
				nColumn = yycolumnno - yyleng;
			}

			/* creates a term with type, text and location of the
			   last token: "<type>(<text>, [<line>, <column>])"   */
			add_token(&mTokens, nToken, tTokenText, nLine, nColumn, &nNumTokens);

			/* saves line and column number of token's last character */
			nLastLine = yylineno;
			nLastColumn = yycolumnno;

		}

		long_token = FALSE;
	}

	/* frees memory used by lexical scanner */
	destroy_scanner();

	/* adds an end-of-file token, which is needed to manage errors */
	PL_put_atom_chars(tTokenText, "");
	add_token(&mTokens, TOK_EOF, tTokenText, nLastLine, nLastColumn, &nNumTokens);

	/* creates list of tokens, and then frees memory */
	for(i = nNumTokens - 1; i >= 0; i--) {
		tToken = (term_t)array_get(&mTokens, 0, i);
        if ( !PL_cons_list(tTokenList, tToken, tTokenList) ) {
            printf("ERROR (module tokenize: pl_tokenize): line 162. \n");
            PL_fail;
        };
	}
	destroy_array(&mTokens);

	/* returns list of tokens */
	if(!PL_unify(tTokens, tTokenList)) {
		PL_fail;
	}

	PL_succeed;
}



/***********************************************************************
 * Private functions
 ***********************************************************************/


/* Creates all the functors that will be used to build tokens. */
static void create_functors(void) {
	fTokenFunc[TOK_RELATION] = PL_new_functor(PL_new_atom("relation"), 2);
	fTokenFunc[TOK_BUILDER] = PL_new_functor(PL_new_atom("builder"), 2);
	fTokenFunc[TOK_NAME] = PL_new_functor(PL_new_atom("name"), 2);
	fTokenFunc[TOK_VARIABLE] = PL_new_functor(PL_new_atom("variable"), 2);
	fTokenFunc[TOK_INTEGER] = PL_new_functor(PL_new_atom("integer"), 2);
	fTokenFunc[TOK_FLOAT] = PL_new_functor(PL_new_atom("float"), 2);
	fTokenFunc[TOK_CHARACTER_CODE_LIST] = PL_new_functor(PL_new_atom("character_code_list"), 2);
	fTokenFunc[TOK_LEFT_PARENTHESIS] = PL_new_functor(PL_new_atom("left_parenthesis"), 2);
	fTokenFunc[TOK_RIGHT_PARENTHESIS] = PL_new_functor(PL_new_atom("right_parenthesis"), 2);
	fTokenFunc[TOK_LEFT_BRACKET] = PL_new_functor(PL_new_atom("left_bracket"), 2);
	fTokenFunc[TOK_RIGHT_BRACKET] = PL_new_functor(PL_new_atom("right_bracket"), 2);
	fTokenFunc[TOK_COMMA] = PL_new_functor(PL_new_atom("comma"), 2);
	fTokenFunc[TOK_LIST_SEPARATOR] = PL_new_functor(PL_new_atom("list_separator"), 2);
	fTokenFunc[TOK_EOF] = PL_new_functor(PL_new_atom("eof"), 2);
}


/* Copies the message of a lexical error to a char array. */
static void generate_error_message(char *sErrorMessage, char *sTokenText, int nErrorId) {
	/* copies error message to char array */
	switch(nErrorId) {
	case ERR_UNKNOWN_CHARACTER:
		snprintf_portable(sErrorMessage, MAX_ERROR_SIZE, "Unexpected character in input: '%s'.", sTokenText);
		break;
	case ERR_UNTERMINATED_STRING:
		snprintf_portable(sErrorMessage, MAX_ERROR_SIZE, "Missing terminating ' character.");
		break;
	case ERR_UNTERMINATED_DQ_STRING:
		snprintf_portable(sErrorMessage, MAX_ERROR_SIZE, "Missing terminating \" character.");
		break;
	case ERR_UNTERMINATED_COMMENT:
		snprintf_portable(sErrorMessage, MAX_ERROR_SIZE, "Unterminated comment.");
		break;
	}
	/* adds NULL byte to error message because Windows'
	   '_snprintf' doesn't guarantee NULL-termination   */
	sErrorMessage[MAX_ERROR_SIZE] = '\0';
}


/* Adds a new token to the list of tokens. Each token created by this
   function will be a term with two arguments following this syntax:
   "<type>(<text>, [<line>, <column>])".                              */
static void add_token(array *pmTokens, int nType, term_t tText, int nLine,
                      int nColumn, int *pnNumTokens) {
	term_t tLine, tColumn, tLineColumn;
	term_t tToken;
	
	/* initializes terms */
	tLine = PL_new_term_ref();
	tColumn = PL_new_term_ref();
	tLineColumn = PL_new_term_ref();
	tToken = PL_new_term_ref();

	/* creates the token with the desired structure */
    if ( !PL_put_integer(tLine, nLine) ) {
        printf("ERROR (module tokenize: add_token): line 241. \n");
    };
    if ( !PL_put_integer(tColumn, nColumn) ) {
        printf("ERROR (module tokenize: add_token): line 244. \n");
    };
	PL_put_nil(tLineColumn);
    if ( !PL_cons_list(tLineColumn, tColumn, tLineColumn) ) {
        printf("ERROR (module tokenize: add_token): line 248. \n");
    };
    if ( !PL_cons_list(tLineColumn, tLine, tLineColumn) ) {
        printf("ERROR (module tokenize: add_token): line 251. \n");
    };
    if ( !PL_cons_functor(tToken, fTokenFunc[nType], tText, tLineColumn) ) {
        printf("ERROR (module tokenize: add_token): line 254. \n");
    };

	/* adds the new token to the list of tokens */
	array_set(pmTokens, 0, *pnNumTokens, (void*)tToken);
	(*pnNumTokens)++;
}


/* Calls the add_message/4 predicate with the indicated error message,
   line and column.                                                    */
static void send_error(char *sMessage, int nLine, int nColumn) {
	predicate_t pAddMessage;
	term_t tArg1, tArg2, tArg3, tArg4;
	term_t tLine, tColumn;

	/* initializes terms and predicates */
	pAddMessage = PL_predicate(PREDICATE_ADD_MESSAGE, 4, MODULE_ADD_MESSAGE);
	tArg1 = PL_new_term_refs(4);
	tArg2 = tArg1 + 1;
	tArg3 = tArg1 + 2;
	tArg4 = tArg1 + 3;
	tLine = PL_new_term_ref();
	tColumn = PL_new_term_ref();

	/* sends error to Prolog */
    if ( !PL_put_integer(tLine, nLine) ) {
        printf("ERROR (module tokenize: send_error): line 281. \n");
    };
    if ( !PL_put_integer(tColumn, nColumn) ) {
        printf("ERROR (module tokenize: send_error): line 284. \n");
    };
	PL_put_atom_chars(tArg1, sMessage);
	PL_put_atom_chars(tArg4, "error");
    if ( !PL_unify(tArg2, tLine) ) {
        printf("ERROR (module tokenize: send_error): line 289. \n");
    };
    if ( !PL_unify(tArg3, tColumn) ) {
        printf("ERROR (module tokenize: send_error): line 292. \n");
    };
	PL_call_predicate(MODULE_ADD_MESSAGE, PL_Q_NORMAL, pAddMessage, tArg1);
}

