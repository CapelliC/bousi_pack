/***********************************************************************
 * Command-line interface
 */

#include <SWI-Prolog.h>
#include <SWI-Stream.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#ifndef _WIN32
#	include <editline/readline.h>
#	include <histedit.h>
#else
#	include "leditwin.h"
#endif
#include "shell.h"
#include "array.h"
#include "bool.h"

/***********************************************************************/

/*
 * This module makes use of several functions that belong to the
 * editline library.
 *
 * ----------------------------------------------------------------------
 *  Original editline's copyright and disclaimer
 * ----------------------------------------------------------------------
 *
 * http://sourceforge.net/projects/libedit/
 *
 * Copyright (c) 1997 The NetBSD Foundation, Inc.
 * All rights reserved.
 *
 * This code is derived from software contributed to The NetBSD Foundation
 * by Jaromir Dolecek.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 * 1. Redistributions of source code must retain the above copyright
 *    notice, this list of conditions and the following disclaimer.
 * 2. Redistributions in binary form must reproduce the above copyright
 *    notice, this list of conditions and the following disclaimer in the
 *    documentation and/or other materials provided with the distribution.
 * 3. All advertising materials mentioning features or use of this software
 *    must display the following acknowledgement:
 *	This product includes software developed by the NetBSD
 *	Foundation, Inc. and its contributors.
 * 4. Neither the name of The NetBSD Foundation nor the names of its
 *    contributors may be used to endorse or promote products derived
 *    from this software without specific prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE NETBSD FOUNDATION, INC. AND CONTRIBUTORS
 * ``AS IS'' AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED
 * TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR
 * PURPOSE ARE DISCLAIMED.  IN NO EVENT SHALL THE FOUNDATION OR CONTRIBUTORS
 * BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
 * CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
 * SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
 * INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
 * CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
 * ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
 * POSSIBILITY OF SUCH DAMAGE.
 */



/***********************************************************************
 * Declarations
 ***********************************************************************/

/* Private function prototypes */
static char** shell_completion(const char *sText, int nStart, int nEnd);
static char* command_generator(const char *sText, int nState);
static char* predicate_generator(const char *sText, int nState);

/* NULL-terminated list of available commands in Bousi-Prolog shell */
static char* psCommands[] = { BPL_COMMANDS, NULL };

/* NULL-terminated array with the list of available predicates */
static array aPredicates;

/* Number of predefined predicates stored in predicates array */
static int nSystemPredicates = 0;

/* TRUE if editline library has been initialized */
static bool bInitialized = FALSE;

/* TRUE if array of predicates has already been initialized */
static bool bPredsInitialized = FALSE;



/***********************************************************************
 * Public functions
 ***********************************************************************/


/** ext_read_shell_line(+Prompt, -String, -Arguments)
 *
 *    Reads a line from standard input using editline library, which
 *    allows command-line editing and history features. The line entered
 *    by the user is returned as an atom in String. A list with all the
 *    arguments found in the line (using space-bar characters as
 *    delimiters) is returned in Arguments.
 */
foreign_t pl_read_shell_line(term_t tPrompt, term_t tString, term_t tArguments) {
	term_t tArgList, tArg;
	term_t tLine;
	char **psTokensIterator, **psTokens;
	char *sPromptChars, *sLineChars;

	/* checks that arguments have the expected types */
	if(!PL_is_atom(tPrompt) || !(PL_is_variable(tString) || PL_is_atom(tString))
	 || !(PL_is_variable(tArguments) || PL_is_list(tArguments))) {
		PL_fail;
	}

	/* retrieves the chars from the prompt and stores them in an array */
    if ( !PL_get_atom_chars(tPrompt, &sPromptChars) ) {
        printf("ERROR (module shell: pl_read_shell_line): line 121. \n");
        PL_fail;
    };

	/* sets the custom completion function for editline library */
	if(!bInitialized) {
		bInitialized = TRUE;
		/*rl_attempted_completion_function = shell_completion;*/
		/*rl_bind_key('\t', rl_complete);*/
	}

	/* reads a line from standard input */
	sLineChars = readline(sPromptChars);

	if(sLineChars != NULL) {

		/* adds line to command history */
		if(strlen(sLineChars) > 0) {
			add_history(sLineChars);
		}

		/* saves the characters of the line in a new atom */
		tLine = PL_new_term_ref();
		PL_put_atom_chars(tLine, sLineChars);

		/* splits the line using editline library default delimiters */
		psTokens = history_tokenize(sLineChars);

		/* saves the generated tokens in a list */
		psTokensIterator = psTokens;
		tArgList = PL_new_term_ref();
		tArg = PL_new_term_ref();
		PL_put_nil(tArgList);
		if(psTokens != NULL) {
			while(*psTokensIterator != NULL) {
				psTokensIterator++;
			}
			while(psTokensIterator != psTokens) {
				psTokensIterator--;
				PL_put_atom_chars(tArg, *psTokensIterator);
                if ( !PL_cons_list(tArgList, tArg, tArgList) ) {
                    printf("ERROR (module shell: pl_read_shell_line): line 162. \n");
                    PL_fail;
                };
			}
		}

		/* frees all the memory used by the line and the tokens */
		free_portable(sLineChars);
		if(psTokens != NULL) {
			while(*psTokens != NULL) {
				free_portable(*psTokens);
				psTokens++;
			}
		}

	} else {

		/* sets an empty line to be returned */
		tLine = PL_new_term_ref();
		PL_put_atom_chars(tLine, "");
		tArgList = PL_new_term_ref();
		PL_put_nil(tArgList);

	}

	/* returns the line and the list of arguments */
	if(!PL_unify(tString, tLine)) {
		PL_fail;
	}
	if(!PL_unify(tArguments, tArgList)) {
		PL_fail;
	}

	PL_succeed;
}


/** ext_load_shell_history(+File)
 *
 *    Replaces the current command history used by editline library with
 *    the command history saved in the specified File. If File doesn't
 *    exist, it'll be created.
 */
foreign_t pl_load_shell_history(term_t tFile) {
	char *sFileChars;

	/* checks that arguments have the expected types */
	if(!PL_is_atom(tFile)) {
		PL_fail;
	}

	/* clears the current history */
	clear_history();

	/* reads the new history file */
    if ( !PL_get_atom_chars(tFile, &sFileChars) ) {
        printf("ERROR (module shell: pl_save_shell_history): line 218. \n");
        PL_fail;
    };
	read_history(sFileChars);

	PL_succeed;
}


/** ext_save_shell_history(+File, +MaxCommands)
 *
 *    Saves command history used by editline library in File, storing
 *    no more commands than the specified in MaxCommands. If File
 *    already exists, it'll be overwritten.
 */
foreign_t pl_save_shell_history(term_t tFile, term_t tMaxCommands) {
	char *sFileChars;
	int nMaxCommands;

	/* checks that arguments have the expected types */
	if(!PL_is_atom(tFile) || !PL_is_number(tMaxCommands)) {
		PL_fail;
	}

	/* saves and truncates the history file */
    if ( !PL_get_atom_chars(tFile, &sFileChars) ) {
        printf("ERROR (module shell: pl_save_shell_history): line 244. \n");
        PL_fail;
    };
    if ( !PL_get_integer_ex(tMaxCommands, &nMaxCommands) ) {
        printf("ERROR (module shell: pl_save_shell_history): line 248. \n");
        PL_fail;
    };
	write_history(sFileChars);
	history_truncate_file(sFileChars, nMaxCommands);

	PL_succeed;
}


/** ext_set_system_predicate_list(+List)
 *
 *    Sets the list of predefined predicates available in Bousi-Prolog.
 *    This list will be used by shell's autocomplete feature.
 */
foreign_t pl_set_system_predicate_list(term_t tList) {
	term_t tTail, tHead;
	char *sTempName, *sName;
	int nTotal;

	/* checks that arguments have the expected types */
	if(!PL_is_list(tList)) {
		PL_fail;
	}

	/* creates the dynamic array where the predicate names will be stored */
	if(bPredsInitialized) {
		destroy_array_and_contents(&aPredicates);
	}
	create_array(&aPredicates, 1, 500, STRING_ARRAY);
	bPredsInitialized = TRUE;

	/* reads and copies the predicate names to the dynamic array */
	nTotal = 0;
	tTail = PL_copy_term_ref(tList);
	tHead = PL_new_term_ref();
	while(PL_get_list(tTail, tHead, tTail)) {
        if ( !PL_get_atom_chars(tHead, &sTempName) ) {
            printf("ERROR (module shell: pl_set_system_predicate_list): line 286. \n");
            PL_fail;
        };
		sName = (char*)strdup(sTempName);
		array_set_string(&aPredicates, 0, nTotal++, sName);
	}

	/* saves the number of system predicates */
	nSystemPredicates = nTotal;

	/* adds a NULL string to mark the end of the array */
	array_set_string(&aPredicates, 0, nTotal++, NULL);

	PL_succeed;
}


/** ext_set_program_predicate_list(+List)
 *
 *    Sets the list of predicates defined in the currently loaded
 *    program. This list will be used by shell's autocomplete feature.
 */
foreign_t pl_set_program_predicate_list(term_t tList) {
	term_t tTail, tHead;
	char *sTempName, *sName;
	int nTotal;
	int i;

	/* creates the dynamic array where the predicate names will be
	   stored, if it hasn't been initialized yet                   */
	if(!bPredsInitialized) {
		create_array(&aPredicates, 1, 500, STRING_ARRAY);
		bPredsInitialized = TRUE;
		nSystemPredicates = 0;
	}

	/* frees the memory allocated by the previous call to this function */
	i = 0;
	while(array_get_string(&aPredicates, 0, nSystemPredicates + i) != NULL) {
		free(array_get_string(&aPredicates, 0, nSystemPredicates + i));
		i++;
	}

	/* reads and copies the predicate names to the dynamic array
	   (after the predefined/system predicates)                  */
	nTotal = nSystemPredicates;
	tTail = PL_copy_term_ref(tList);
	tHead = PL_new_term_ref();
	while(PL_get_list(tTail, tHead, tTail)) {
        if ( !PL_get_atom_chars(tHead, &sTempName) ) {
            printf("ERROR (module shell: pl_set_program_predicate_list): line 336. \n");
            PL_fail;
        };
		sName = (char*)strdup(sTempName);
		array_set_string(&aPredicates, 0, nTotal++, sName);
	}

	/* adds a NULL string to mark the end of the array */
	array_set_string(&aPredicates, 0, nTotal++, NULL);

	PL_succeed;
}



/***********************************************************************
 * Private functions
 ***********************************************************************/

/* Following functions are based on the GNU Readline Library examples */


/* Attempts to complete the contents of a word with the name of a
   Bousi-Prolog command if word is at the start of the line or begins
   with 'hp', the name of a file in current directory if word begins
   with 'ld', or the name of a predicate otherwise.                   */
static char** shell_completion(const char *sText, int nStart, int nEnd) {
	return NULL;
}


/* Tries to complete a Bousi-Prolog shell command. */
static char* command_generator(const char *sText, int nState) {
	return NULL;
}


/* Tries to complete the name of a system or user-defined predicate. */
static char* predicate_generator(const char *sText, int nState) {
	return NULL;
}

