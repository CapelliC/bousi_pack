%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% On-line help for Bousi-Prolog shell commands

:- module(bplHelp, [
		bpl_help/0,             %
		command_help/1          % +Topic
   ]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- set_prolog_flag(double_quotes, codes).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Predicates for displaying Bousi-Prolog on-line help
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%% bpl_help
%
%     Shows a list with all available commands in Bousi-Prolog shell.
%

bpl_help :-
	write('----------------------------------------------------------------------------\n'),
	write('Available commands:\n'),
	write('ld ... (load) reads a source code file containing a program or an ontology,\n'),
	write('       or prints the currently loaded files\n'),
	write('sv ... (solve) executes a query [\'sv\' can be omitted]\n'),
	write('lc ... (lambda-cut) prints or sets the lower bound for the approximation\n'),
	write('       degree in weak unifications\n'),
	write('fl ... (filtering) prints or sets the state of filtering\n'),
	write('pwd .. (print working directory) shows the path of the working directory\n'),
	write('cd ... (change directory) changes the working directory\n'),
	write('ls ... (list) lists the contents of the working directory\n'),
	write('hp ... (help) shows this help text or information about a specific command\n'),
	write('sh ... (shell) starts a new interactive shell process\n'),
	write('ts ... (test) tests the system\n'),
	write('bk ... (break) breaks to the underlying SWI-Prolog\n'),
	write('qt ... (quit) quits the system\n'),
	write('----------------------------------------------------------------------------\n'),
	write('--          Enter \'hp <command>\' to get more help on a <command>          --\n'),
	write('----------------------------------------------------------------------------\n').


%% command_help(+Command)
%
%     Displays help for a specific Bousi-Prolog command.
%

command_help(ld) :-
	write('----------------------------------------------------------------------------\n'),
	write('LOAD\n'),
	nl,
	write('Reads, compiles and loads a Bousi~Prolog program or ontology into the\n'),
	write('database. The Bousi~Prolog system can have both a program and an ontology\n'),
	write('loaded at the same time, but the program must always be loaded first and\n'),
	write('the ontology is not allowed to contain clauses.\n'),
	nl,
	write('The default file extensions are \'.bpl\' for programs and \'.ont\' for onto-\n'),
	write('logies. If you want to load a file with a different extension, the full\n'),
	write('filename must be provided.\n'),
	nl,
	write('When used without an argument, this command shows the full path of the\n'),
	write('program and ontology which are currently loaded (if any).\n'),
	nl,
	write('Every Bousi~Prolog source code file is translated into an intermediate\n'),
	write('Prolog representation of the original file called \'TPL code\'. This code is\n'),
	write('stored in files which have the same name as the original BPL files but with\n'),
	write('the \'.tpl\' extension. TPL files are used when BPL files haven\'t been modi-\n'),
	write('fied since their corresponding TPL code was created. You can force the\n'),
	write('rebuilding of the TPL files using the \'-f\' option.\n'),
	nl,
	write('Syntax:\n'),
	write('ld .............. shows the currently loaded program and ontology\n'),
	write('ld <file> ....... loads a Bousi~Prolog program <file>\n'),
	write('ld -f <file> .... rebuilds and loads a Bousi~Prolog program <file>\n'),
	write('ld -o <file> .... loads a Bousi~Prolog ontology <file>\n'),
	write('ld -fo <file> ... rebuilds and loads a Bousi~Prolog ontology <file>\n'),
	write('----------------------------------------------------------------------------\n').

command_help(sv) :-
	write('----------------------------------------------------------------------------\n'),
	write('SOLVE\n'),
	nl,
	write('Executes a query about the currently loaded program using WSLD resolution.\n'),
	write('Bousi~Prolog queries can contain any statement that is valid in the body of\n'),
	write('a clause, including weak unifications and term comparisons. Queries can also\n'),
	write('be executed if no program is loaded.\n'),
	nl,
	write('When at least one solution is found for a query, the system shows its appro-\n'),
	write('ximation degree and the resulting variable values (if any). You can then\n'),
	write('type a semicolon (;) to look for more solutions or press RETURN to finish\n'),
	write('the current query and return to the Bousi~Prolog shell.\n'),
	nl,
	write('The \'sv\' command is the default Bousi~Prolog shell command. This means that\n'),
	write('you can also enter queries without preceding them with \'sv\'.\n'),
	nl,
	write('Syntax:\n'),
	write('sv <query> ...... solves <query> using WSLD resolution\n'),
	write('<query> ......... identical to \'sv <query>\'\n'),
	write('----------------------------------------------------------------------------\n').

command_help(lc) :-
	write('----------------------------------------------------------------------------\n'),
	write('LAMBDA-CUT\n'),
	nl,
	write('Prints or sets the lower bound allowed for the approximation degree in weak\n'),
	write('unifications, which is known as the \'lambda-cut\'. When used to set a new\n'),
	write('lambda-cut value, this command overrides the lambda_cut/1 directive that may\n'),
	write('be present in the already loaded program.\n'),
	nl,
	write('The lambda-cut can be used to limit the expansion of the search space in a\n'),
	write('WSLD resolution. When the lambda-cut value is set to a degree greater than\n'),
	write('zero, the weak unification process will fail every time the computed approxi-\n'),
	write('mation degree goes below the stored lambda-cut value. Therefore, the computa-\n'),
	write('tion will also fail and all branches starting from that choice point will be\n'),
	write('discarded.\n'),
	nl,
	write('Syntax:\n'),
	write('lc .............. prints the current lambda-cut value\n'),
	write('lc <degree> ..... sets <degree> as the new lambda-cut value, which must be\n'),
	write('                  a real number between 0 and 1 (inclusive)\n'),
	write('----------------------------------------------------------------------------\n').

command_help(fl) :-
	write('----------------------------------------------------------------------------\n'),
	write('FILTERING\n'),
	nl,
	write('Prints or sets whether filtering is enabled or not. When used to set a new\n'),
	write('filtering value, this command overrides the filtering/1 directive that may\n'),
	write('be present in the already loaded program.\n'),
	nl,
	write('The filtering is enabled by default. When enabled, it removes the proximity\n'),
	write('equations that are below the lambda-cut. As a consequence, translated program\n'),
	write('rules (in the .tpl file) which are known to not be selected for a lambda-cut\n'),
	write('less than the current one are also discarded.\n'),
	nl,
	write('Syntax:\n'),
	write('fl .............. prints whether filtering is enabled or not\n'),
	write('fl <boolean> .... sets <boolean> as the new value, which must be\n'),
	write('                  a Boolean (either \'true\' or \'false\')\n'),
	write('----------------------------------------------------------------------------\n').

command_help(hp) :-
	write('----------------------------------------------------------------------------\n'),
	write('HELP\n'),
	nl,
	write('Gives a summary of the Bousi~Prolog shell commands or detailed information\n'),
	write('about a specific command.\n'),
	nl,
	write('Syntax:\n'),
	write('hp .............. shows a summary of the shell commands\n'),
	write('hp <command> .... shows information about the given <command>\n'),
	write('----------------------------------------------------------------------------\n').

command_help(ls) :-
	write('----------------------------------------------------------------------------\n'),
	write('LIST\n'),
	nl,
	write('Lists the files and folders of the current working directory.\n'),
	nl,
	write('Syntax:\n'),
	write('ls .............. list the contents of the working directory\n'),
	write('----------------------------------------------------------------------------\n').

command_help(pwd) :- 
	write('----------------------------------------------------------------------------\n'),
	write('PRINT WORKING DIRECTORY\n'),
	nl,
	write('Shows the absolute path of the current working directory.\n'),
	nl,
	write('Syntax:\n'),
	write('pwd ............. shows the full path of the working directory\n'),
	write('----------------------------------------------------------------------------\n').

command_help(cd) :- 
	write('----------------------------------------------------------------------------\n'),
	write('CHANGE DIRECTORY\n'),
	nl,
	write('Changes the current working directory.\n'),
	nl,
	write('Syntax:\n'),
	write('cd <path> ....... changes the working directory to <path>, which can be\n'),
	write('                  either an absolute or a relative path\n'),
	write('----------------------------------------------------------------------------\n').

command_help(sh) :- 
	write('----------------------------------------------------------------------------\n'),
	write('SHELL\n'),
	nl,
	write('Starts a new interactive command-line shell without ending the Bousi~Prolog\n'),
	write('system. The default shell is \'/bin/sh\' on Unix/Linux and \'cmd.exe\' on,\n'),
	write('Windows, but you can override it entering a different path in the environ-\n'),
	write('ment variables SHELL (on Unix/Linux) and COMSPEC (on Windows).\n'),
	nl,
	write('Syntax:\n'),
	write('sh .............. starts a new interactive shell process\n'),
	write('----------------------------------------------------------------------------\n').

command_help(ts) :-
	write('----------------------------------------------------------------------------\n'),
	write('TEST\n'),
	nl,
	write('Test the system with a bundle of test programs.\n'),
	nl,
	write('Syntax:\n'),
	write('ts .............. tests the system\n'),
	write('----------------------------------------------------------------------------\n').

command_help(bk) :-
	write('----------------------------------------------------------------------------\n'),
	write('BREAK\n'),
	nl,
	write('Leaves the Bousi~Prolog system and enter underlying Prolog. Use ?- main. to\n'),
	write('restart the system.\n'),
	nl,
	write('Syntax:\n'),
	write('bk .............. breaks to the underlying SWI-Prolog\n'),
	write('----------------------------------------------------------------------------\n').

command_help(qt) :-
	write('----------------------------------------------------------------------------\n'),
	write('QUIT\n'),
	nl,
	write('Quits the Bousi~Prolog system immediately. Always use this command instead\n'),
	write('of the predefined predicate halt/0 when you want to end the application.\n'),
	nl,
	write('Syntax:\n'),
	write('qt .............. quits the system\n'),
	write('----------------------------------------------------------------------------\n').

