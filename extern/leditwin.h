#ifndef _LEDITWIN_H
#define _LEDITWIN_H

/*
 * The implementation of this module contains modified versions of
 * several functions that belong to libedit and linenoise libraries.
 *
 * ----------------------------------------------------------------------
 *  Original linenoise copyright and disclaimer
 * ----------------------------------------------------------------------
 *
 * https://github.com/antirez/linenoise
 *
 * Copyright (c) 2010, Salvatore Sanfilippo <antirez at gmail dot com>
 * Copyright (c) 2010, Pieter Noordhuis <pcnoordhuis at gmail dot com>
 *
 * All rights reserved.
 * 
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions are
 * met:
 * 
 *  *  Redistributions of source code must retain the above copyright
 *     notice, this list of conditions and the following disclaimer.
 *
 *  *  Redistributions in binary form must reproduce the above copyright
 *     notice, this list of conditions and the following disclaimer in the
 *     documentation and/or other materials provided with the distribution.
 * 
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
 * "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
 * LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
 * A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
 * HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
 * SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
 * LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
 * DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
 * THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
 * (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
 * OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 *
 * ----------------------------------------------------------------------
 *  Original libedit copyright and disclaimer
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

/* Macros that return the handle of the standard input and output
   streams of a Windows console application                       */
#define STDIN  GetStdHandle(STD_INPUT_HANDLE)
#define STDOUT GetStdHandle(STD_OUTPUT_HANDLE)

/* Maximum length of lines read from standard input */
#define LINENOISE_MAX_LINE 4096

/* Maximum number of entries in history table */
#define MAX_HISTORY_LEN 1000

/* Command identifiers */
#define COMMAND_NONE              0
#define COMMAND_UNKNOWN           1
#define COMMAND_ENTER             2
#define COMMAND_UP_HISTORY        3
#define COMMAND_DOWN_HISTORY      4
#define COMMAND_LEFT              5
#define COMMAND_RIGHT             6
#define COMMAND_BACKSPACE         7
#define COMMAND_DELETE            8
#define COMMAND_LINE_START        9
#define COMMAND_LINE_END          10
#define COMMAND_DELETE_FROM_START 11
#define COMMAND_DELETE_TO_END     12
#define COMMAND_SWAP              13

/* Internal function prototypes */
char *readline(const char *prompt);
int read_history(const char *filename);
int write_history(const char *filename);
void clear_history(void);
int add_history(const char *line);
int history_truncate_file(const char *filename, int len);
char **history_tokenize(const char *str);

/* Not-used variables */
typedef char **CPPFunction(const char *, int, int);
extern CPPFunction *rl_attempted_completion_function;
extern char *rl_line_buffer;
extern char rl_completion_append_character;

/* Not-implemented functions */
int rl_bind_key(int c, int (*func)(int, int));
int rl_complete(int ignore, int invoking_key);
char **rl_completion_matches(const char *text, void *entry_func);
char *rl_filename_completion_function(const char *text, int state);

#endif

