#ifndef _TOKENIZE_H
#define _TOKENIZE_H

/* Name of the 'add_message/4' predicate and the module where it's defined */
#define PREDICATE_ADD_MESSAGE "add_message"
#define MODULE_ADD_MESSAGE "parser"

/* Maximum size for error messages */
#define MAX_ERROR_SIZE 250

/* Maximum size for variable names */
#define MAX_VAR_NAME_SIZE 250

/* Initial size for lists of tokens and variable bindings */
#define INI_TOKEN_LIST_SIZE 50
#define INI_BINDING_LIST_SIZE 5

/* Name of the 'snprintf' function, which is '_snprintf' on Windows */
#ifdef _WIN32
#	define snprintf_portable _snprintf
#else
#	define snprintf_portable snprintf
#endif

/* Structure used to represent a variable binding */
typedef struct {
	char sVarName[MAX_VAR_NAME_SIZE];
	term_t tVariable;
} binding;

#endif

