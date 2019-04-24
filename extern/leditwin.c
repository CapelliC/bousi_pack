/***********************************************************************
 * Editline library replacement for partial Windows compatibility
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/types.h>
#include <windows.h>
#include <wincon.h>
#include "bool.h"
#include "leditwin.h"

/***********************************************************************/

/*
 * This module contains modified versions of several functions that
 * belong to editline and linenoise libraries.
 *
 * ----------------------------------------------------------------------
 *  Original linenoise's copyright and disclaimer
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
static void initialize_module(void);
static int readline_internal(char *buf, size_t buflen, const char *prompt);
static void refresh_line(const char *prompt, char *buffer, size_t length,
                         size_t position, size_t columns);
static int write_string(const char *buffer, unsigned int length);
static void free_history(void);
static int read_char(char *dest, int *keycode);
static int command_from_key(char ascii, int keycode);

/* History table */
static char **history = NULL;

/* Number of entries in history table */
static int history_len = 0;

/* TRUE if this module has already been initialized */       
static bool initialized = FALSE;

/* Number of character columns of the console */
static size_t columns = 0;

/* TRUE if this application is being executed on a Windows command-line
   console, FALSE otherwise (e.g. if it's being used from Eclipse)      */
static bool windows_console = FALSE;

/* Not used */
CPPFunction *rl_attempted_completion_function;
char *rl_line_buffer;
char rl_completion_append_character;



/***********************************************************************
 * Internal functions
 ***********************************************************************/


/* Reads a single line from standard input and returns it without the
   trailing newline. Returned string must be freed when not needed.   */
char *readline(const char *prompt) {
	char buffer[LINENOISE_MAX_LINE + 1];
	char *line;
	int count;

    /* initializes this module if it hasn't been done yet */
	if(!initialized) {
		initialize_module();
		initialized = TRUE;
	}

	/* reads a line from standard input */	
	count = readline_internal(buffer, LINENOISE_MAX_LINE, prompt);

	/* copies the line to a newly allocated buffer */	
	line = NULL;
	if(count != -1) {
		write_string("\n", 1);
		buffer[count] = '\0';
		line = strdup(buffer);
	}

	fflush(stdout);

	return line;
}


/* Loads the specified file and adds its lines to the current history.
   If the file does not exist 0 is returned and no operation is
   performed, if the file exists and can be loaded successfully 0 is
   returned, otherwise -1 is returned.                                 */
int read_history(const char *filename) {
	char buf[LINENOISE_MAX_LINE];
	FILE *fp;
	char *p;
	
	/* opens file */
	fp = fopen(filename, "r");
	if(fp == NULL) {
		return -1;
	}
	
	/* reads lines and adds them to history */
	while(fgets(buf, LINENOISE_MAX_LINE, fp) != NULL) {
		p = strchr(buf, '\r');
		if(!p) {
			p = strchr(buf, '\n');
		}
		if(p) {
			*p = '\0';
		}
		add_history(buf);
	}
	
	/* closes file */
	fclose(fp);
	
	return 0;
}


/* Saves the current history in the specified file. On success 0
   is returned, otherwise -1 is returned.                        */
int write_history(const char *filename) {
	FILE *fp;
	int i;
	
	/* opens file */
	fp = fopen(filename, "w");
	if(fp == NULL) {
		return -1;
	}
	
	/* writes lines from history */
	for(i = 0; i < history_len; i++) {
		fprintf(fp, "%s\n", history[i]);
	}
	
	/* closes file */
	fclose(fp);
	
	return 0;
}


/* Deletes all entries of the current history. */
void clear_history(void) {
	free_history();
}


/* Adds a new entry to the current history. On success 0 is returned,
   otherwise -1 is returned.                                          */
int add_history(const char *line) {
	char *linecopy;
	
	/* allocates memory for the history if it hasn't been done before */
	if(history == NULL) {
		history = malloc(sizeof(char*) * MAX_HISTORY_LEN);
		if(history == NULL) {
			return -1;
		}
		memset(history, 0, (sizeof(char*) * MAX_HISTORY_LEN));
	}
	
	/* makes a copy of the string */
	linecopy = strdup(line);
	if(!linecopy) {
		return -1;
	}
	
	/* removes the last string from history if max size has been reached */
	if(history_len == MAX_HISTORY_LEN) {
		free(history[0]);
		memmove(history, history + 1, sizeof(char*) * (MAX_HISTORY_LEN - 1));
		history_len--;
	}
	
	/* adds the string to the table */
	history[history_len] = linecopy;
	history_len++;
	
	return 0;
}


/* Truncates the specified history file, keeping only a certain number of
   lines. On success 0 is returned, otherwise -1 is returned.             */
int history_truncate_file(const char *filename, int lines) {
	char **current_history, **new;
	int current_len;
	bool err;
	
	err = FALSE;
	if(lines < 1) {
		lines = 1;
	}
	if(lines > MAX_HISTORY_LEN) {
		lines = MAX_HISTORY_LEN;
	}
	
	/* stores current history */
	current_history = history;
	current_len = history_len;
	history = NULL;
	history_len = 0;

	/* loads history to be truncated */
	if(read_history(filename) == -1) {
		err = TRUE;
	}

	/* truncates number of lines (if needed) */
	if(!err && history && lines < history_len) {
		new = malloc(sizeof(char*) * MAX_HISTORY_LEN);
		if(new == NULL) {
			err = TRUE;
		} else {
			memcpy(new, history + (history_len - lines), sizeof(char*) * lines);
			free(history);
			history = new;
			history_len = lines;
		}
	}

	/* saves truncated history */
	if(!err && history) {
		write_history(filename);
	}
	
	/* restores previous history */
	free_history();
	history = current_history;
	history_len = current_len;

	return (err ? -1 : 0);
}


/* Splits a string into individual tokens the same way a shell would do it. */
char **history_tokenize(const char *str) {
	char **result, *temp;
	char delim;
	size_t len;
	int size, result_idx, start;
	int i;

	size = 1;
	result_idx = 0;
	result = NULL;
	delim = '\0';

	for(i = 0; str[i]; i++) {
		while(isspace((unsigned char) str[i])) {
			i++;
		}
		start = i;
		for(; str[i]; i++) {
			if(str[i] == '\\') {
				if(str[i + 1] != '\0') {
					i++;
				}
			} else if(str[i] == delim) {
				delim = '\0';
			} else if (!delim && (isspace((unsigned char) str[i])
			                      || strchr("()<>;&|$", str[i]))) {
				break;
			} else if (!delim && strchr("'`\"", str[i])) {
				delim = str[i];
			}
		}

		if(result_idx + 2 >= size) {
			size <<= 1;
			result = realloc(result, sizeof(char*) * size);
		}
		len = i - start;
		temp = malloc(len + 1);
		(void)strncpy(temp, &str[start], len);
		temp[len] = '\0';
		result[result_idx++] = temp;
		result[result_idx] = NULL;
		
		if(!str[i]) {
			break;
		}
	}

	return result;
}


/* Not implemented. */
int rl_bind_key(int c, int (*func)(int, int)) {
	return 0;
}


/* Not implemented. */
int rl_complete(int ignore, int invoking_key) {
	return 0;
}


/* Not implemented. */
char **rl_completion_matches(const char *text, void *entry_func) {
	return NULL;
}


/* Not implemented. */
char *rl_filename_completion_function(const char *text, int state) {
	return NULL; 
}



/***********************************************************************
 * Private functions
 ***********************************************************************/


/* Initializes this module and retrieves some information about the
   console in which this application is being executed.             */
static void initialize_module(void) {
	CONSOLE_SCREEN_BUFFER_INFO consoleInfo;

	/* gets the size of the console (in character columns) */
	if(GetConsoleScreenBufferInfo(STDOUT, &consoleInfo)) {
		columns = consoleInfo.dwSize.X;
		windows_console = TRUE;
	} else {
		/* the above function will fail if this program isn't being
		   executed on a Windows command-line console, but on another
		   kind of console (such as the Console view of Eclipse); in
		   that case, no more Windows-specific functions must be
		   invoked, and the standard C functions for I/O must be used
		   instead for maximum compatiblity                           */
		columns = 80;
		windows_console = FALSE;
	}

	/* sets the 'free_history' function to be executed on exit */
	atexit(free_history);
}


/* Prints a prompt in console, reads a line from standard input and
   returns its length and contents (in first argument). This is the
   main function of this module.                                    */
static int readline_internal(char *buf, size_t buflen, const char *prompt) {
	size_t pos, len;
	char ascii;
	int history_index, keycode, command, aux;
	bool refresh, enter, err;
	
	pos = len = 0;
	history_index = 0;
	
	/* initializes buffer to make sure there is always space for
	   the last NULL-character                                   */
	buf[0] = '\0';
	buflen--;

	/* sets an empty string as the latest history entry, which
	   is used as the buffer for the current line              */
	if(add_history("") == -1) {
		return -1;
	}

	/* writes prompt */    
	if(write_string(prompt, strlen(prompt)) == -1) {
		return -1;
	}

	/* main loop */
	enter = FALSE;
	err = FALSE;
	refresh = FALSE;
	while(!enter && !err) {
		
		/* reads a single character from standard input */
		if(read_char(&ascii, &keycode) <= 0) {
			err = TRUE;
		}
	  
		/* scans character read */
		command = command_from_key(ascii, keycode);
		switch(command) {
        	
			case COMMAND_ENTER:
				/* removes the current history entry */
				history_len--;
				free(history[history_len]);
				/* exits loop */
				enter = TRUE;
				break;

			case COMMAND_UP_HISTORY:
			case COMMAND_DOWN_HISTORY:
				if(history_len > 1) {
					/* updates the current history entry */
					free(history[history_len - 1 - history_index]);
					history[history_len - 1 - history_index] = strdup(buf);
					/* gets the index of the new entry */
					history_index += (command == COMMAND_UP_HISTORY) ? 1 : -1;
					if(history_index < 0) {
						history_index = 0;
					} else if(history_index >= history_len) {
						history_index = history_len - 1;
					} else {
						/* copies the new entry to output buffer and prints it */
						strncpy(buf, history[history_len - 1 - history_index], buflen);
						buf[buflen] = '\0';
						len = pos = strlen(buf);
						refresh = TRUE;
					}
				}
				break;
		
			case COMMAND_LEFT:
				if(pos > 0) {
					/* moves cursor to the left */
					pos--;
					refresh = TRUE;
				}
				break;
     
			case COMMAND_RIGHT:
				if(pos != len) {
					/* moves cursor to the right */
					pos++;
					refresh = TRUE;
				}
				break;
		
			case COMMAND_BACKSPACE:
				if(pos > 0 && len > 0) {
					/* delete previous character */
					memmove(buf + pos - 1, buf + pos, len - pos);
					pos--;
					len--;
					buf[len] = '\0';
					refresh = TRUE;
				}
				break;
		
			case COMMAND_DELETE:
				if(len > 1 && pos < len) {
					/* deletes current character */
					memmove(buf + pos, buf + pos + 1, len - pos);
					len--;
					buf[len] = '\0';
					refresh = TRUE;
				} else if(len == 1 && pos == 0) {
					/* deletes the only character of the string */
					buf[0] = '\0';
					pos = len = 0;
					refresh = TRUE;
				}
				break;
  	
			case COMMAND_LINE_START:
				/* moves cursor to the beginning of the line */
				pos = 0;
				refresh = TRUE;
				break;
  	
			case COMMAND_LINE_END: 
				/* moves cursor to the end of the line */
				pos = len;
				refresh = TRUE;
				break;

			case COMMAND_DELETE_FROM_START:
				/* deletes string from the beginning to current position */
				memmove(buf, buf + pos, len - pos);
				buf[len - pos] = '\0';
				len -= pos;
				pos = 0;
				refresh = TRUE;
				break;

			case COMMAND_DELETE_TO_END:
				/* deletes string from current position to the end */
				buf[pos] = '\0';
				len = pos;
				refresh = TRUE;
				break;

			case COMMAND_SWAP:
				/* swaps current and previous characters */
				if(pos > 0 && pos < len) {
					aux = buf[pos - 1];
					buf[pos - 1] = buf[pos];
					buf[pos] = aux;
					if(pos != len - 1) {
						pos++;
					}
					refresh = TRUE;
				}
				break;

			case COMMAND_NONE:
				/* adds character to current line */
				if(len < buflen) {
					if(len == pos) {
						buf[pos] = ascii;
						pos++;
						len++;
						buf[len] = '\0';
						if(strlen(prompt) + len < columns) {
							if(write_string(&ascii, 1) == -1) {
								err = TRUE;
							}
						} else {
							refresh = TRUE;
						}
					} else {
						memmove(buf + pos + 1, buf + pos, len - pos);
						buf[pos] = ascii;
						len++;
						pos++;
						buf[len] = '\0';
						refresh = TRUE;
					}
				}
				break;
				
			default:
				/* ignores character */
				break;
				
		}
		
		/* refreshes current line (if needed) */
		if(refresh) {
			refresh_line(prompt, buf, len, pos, columns);
			refresh = FALSE;
		}
		
	}
	
	return len;
}


/* Clears the current line and prints a new one. */
static void refresh_line(const char *prompt, char *buffer, size_t length,
                         size_t position, size_t columns) {
	char aux[LINENOISE_MAX_LINE];
	CONSOLE_SCREEN_BUFFER_INFO consoleInfo;
	COORD cursor;
	size_t bufOffset, bufLength;
	size_t realPosition;
	int cursorY, out;
	
	if(windows_console) {
		
		/* gets current cursor position (windows consoles only) */
		GetConsoleScreenBufferInfo(STDOUT, &consoleInfo);
		cursorY = consoleInfo.dwCursorPosition.Y;

		/* moves cursor to left edge */
		cursor.X = 0;
		cursor.Y = cursorY;
		SetConsoleCursorPosition(STDOUT, cursor);
	
		/* writes the prompt */
		WriteConsole(STDOUT, prompt, strlen(prompt), &out, NULL);

		/* writes the command stored in the buffer */
		if(strlen(prompt) + length <= columns - 1) {
			
			/* the full command fits in a single row */
			realPosition = position;
			WriteConsole(STDOUT, buffer, length, &out, NULL);
			
		} else {
			
			/* the full command doesn't fit in a single row */
			if(position >= length - (columns / 2) + 3) {
				
				/* just the ending fragment of the command will be displayed */
				realPosition = position - (length - (columns - strlen(prompt) - 4) - 3);
				bufOffset = length - (columns - strlen(prompt) - 4);
				bufLength = columns - strlen(prompt) - 4;
				WriteConsole(STDOUT, "...", 3, &out, NULL);
				WriteConsole(STDOUT, buffer + bufOffset, bufLength, &out, NULL);
				
			} else if(position <= (columns / 2) - 3) {
	
				/* just the starting fragment of the command will be displayed */
				realPosition = position;
				bufOffset = 0;
				bufLength = columns - strlen(prompt) - 4;
				WriteConsole(STDOUT, buffer + bufOffset, bufLength, &out, NULL);
				WriteConsole(STDOUT, "...", 3, &out, NULL);
				
			} else {
				
				/* a middle fragment of the command will be displayed */
				realPosition = (columns - strlen(prompt) - 1) / 2;
				bufOffset = (position - realPosition + 3);
				bufLength = columns - strlen(prompt) - 7;
				WriteConsole(STDOUT, "...", 3, &out, NULL);
				WriteConsole(STDOUT, buffer + bufOffset, bufLength, &out, NULL);
				WriteConsole(STDOUT, "...", 3, &out, NULL);
				
			}
			
		}

		/* erases the remaining line (windows consoles only) */	
		if(strlen(prompt) + length <= columns - 1) {
			memset(aux, ' ', LINENOISE_MAX_LINE);
			GetConsoleScreenBufferInfo(STDOUT, &consoleInfo);
			cursor = consoleInfo.dwCursorPosition;
			WriteConsoleOutputCharacter(STDOUT, aux, columns - (strlen(prompt) + position),
			                            cursor, &out);
		}
		
		/* moves cursor to new current position (row & column) */
		cursor.X = strlen(prompt) + realPosition;
		cursor.Y = cursorY;
		SetConsoleCursorPosition(STDOUT, cursor);
		
	}
}


/* Frees all memory used by the history table. */
static void free_history(void) {
	int i;
	
	if(history != NULL) {
		for(i = 0; i < history_len; i++) {
			if(history[i]) {
				free(history[i]);
			}
		}
		free(history);
		history = NULL;
		history_len = 0;
	}
}


/* Writes a string in standard output and returns the number of characters
   that were actually written.                                             */
static int write_string(const char *buffer, unsigned int length) {
	char* aux;
	int out;
	
	if(windows_console) {
		WriteConsole(STDOUT, buffer, length, &out, NULL);
	} else {
		aux = (char*)malloc(sizeof(length + 1));
		strncpy(aux, buffer, length);
		aux[length] = '\0';
		printf("%s", aux);
		free(aux);
		fflush(stdout);
	}
	return out;
}


/* Reads and returns a character from standard input. If a control key is
   pressed, 0 is returned in dest and a VK_ macro is returned in keycode. */
static int read_char(char *dest, int *keycode) {
	INPUT_RECORD event;
	int num, code;
	bool read;
	
	/* reads a single character from the console input
	   (all events except key pressed events are ignored) */
	read = FALSE;
	do {
		if(windows_console) {
			ReadConsoleInput(STDIN, &event, 1, &num);
			read = (num == 1 && event.EventType == KEY_EVENT
			        && event.Event.KeyEvent.bKeyDown);
		} else {
			code = getchar();
			num = 1;
			read = TRUE;
		}
	} while(!read);
	
	/* returns the character and the keycode */
	if(windows_console) {
		*dest = event.Event.KeyEvent.uChar.AsciiChar;
		*keycode = event.Event.KeyEvent.wVirtualKeyCode;
	} else {
		*dest = code;
		*keycode = -1;		
	}

	return num;
}


/* Returns the command corresponding to a character and/or a keycode. */
static int command_from_key(char ascii, int keycode) {
	int command;
	
	command = COMMAND_NONE;
	
	if((ascii == 0 && keycode == VK_UP) ||
	   (ascii == 16)) {
		/* up / ctrl+p */
		command = COMMAND_UP_HISTORY;
	
	} else if((ascii == 0 && keycode == VK_DOWN) ||
	          (ascii == 14)) {
		/* down / ctrl+n */
		command = COMMAND_DOWN_HISTORY;
	
	} else if((ascii == 0 && keycode == VK_LEFT) ||
	          (ascii == 2)) {
		/* left / ctrl+b */
		command = COMMAND_LEFT;
	
	} else if((ascii == 0 && keycode == VK_RIGHT) ||
	          (ascii == 6)) {
		/* right / ctrl+f */
		command = COMMAND_RIGHT;
	
	} else if((ascii == 127) ||
	          (ascii == 8)) {
		/* backspace / ctrl+h */
		command = COMMAND_BACKSPACE;
	
	} else if((ascii == 13) ||
	          (ascii == 10)) {
		/* enter / ctrl+m */
		command = COMMAND_ENTER;
	
	} else if((ascii == 0 && keycode == VK_DELETE) ||
	          (ascii == 4)) {
		/* delete / ctrl+d */
		command = COMMAND_DELETE;
	
	} else if((ascii == 0 && keycode == VK_HOME) ||
	          (ascii == 1)) {
		/* home / ctrl+a */
		command = COMMAND_LINE_START;
	
	} else if((ascii == 0 && keycode == VK_END) ||
	          (ascii == 5)) {
		/* end / ctrl+e */
		command = COMMAND_LINE_END;
	
	} else if(ascii == 21) {
		/* ctrl+u */
		command = COMMAND_DELETE_FROM_START;
	
	} else if(ascii == 11) {
		/* ctrl+k */
		command = COMMAND_DELETE_TO_END;
	
	} else if(ascii == 20) {
		/* ctrl+t */
		command = COMMAND_SWAP;
	
	} else if(ascii <= 27) {
		/* unknown or unused control key */
		command = COMMAND_UNKNOWN;
	
	}
	
	return command;
}

