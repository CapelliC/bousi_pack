%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Bousi-Prolog foreign library interface

:- module(foreign, [
		load_foreign_extension/0
   ]).

:- initialization use_foreign_library(foreign(bousi_support)).

% :- use_module(library(shlib)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- set_prolog_flag(double_quotes, codes).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Foreign library loader
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

load_foreign_extension :- !, true.

%% load_foreign_extension
%
%     Loads Bousi-Prolog foreign library into memory.
%

load_foreign_extension :- 
	% Retrieves the path of the foreign library for this OS
	current_prolog_flag(executable, BPLExecutable),
	foreign_library_name(LibraryName),
  path_separator(Separator),
	% Loads the foreign library (if it exists)	
	( file_directory_name(BPLExecutable, BPLPath)
   ;
	  findall(I, sub_atom(BPLExecutable, I, 1, _, Separator), Is), % Find the last separator
	  max_list(Is, M), 
	  %M1 is M+1, 
	  sub_atom(BPLExecutable, 0, M, _, BPLPath)
	),
  concat_atom([BPLPath, Separator, LibraryName], LibraryPath),
	exists_file(LibraryPath),
	load_foreign_library(LibraryPath)
	;
	(
	  LibraryPath=LibraryPath,
		writef('ERROR: \'%w\' library not found.', [LibraryPath]), nl,
		write('If your Bousi-Prolog distribution includes the source code, \c
		       run \'make\' before starting Bousi-Prolog.'), nl,
		halt
	).
	
%% path_separator(?Separator)
%
%     Unifies Separator with the path separator of the OS file system
%

path_separator('\\') :-
	current_prolog_flag(windows, true), 
	!.

path_separator('/').


%% foreign_library_name(?Library)
%
%     Unifies Library with the name of the foreign library written in
%     C, which is 'extern.so' on Unix/Linux or 'extern.dll' on Windows.
%

foreign_library_name('extern.dll') :-
	current_prolog_flag(windows, true), 
	!.

foreign_library_name('extern.so').


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Foreign predicates documentation
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%% ext_closure(+InputEquations, +Closure, +TNorm, +RelationName, -OutputEquations)
%
%    Computes the reflexive, symmetric and/or transitive closure of
%     the fuzzy relation defined by a set of equations and returns the
%     list of equations of the resulting fuzzy relation. Input
%     equations must be like "rel(a, b, 0.5)", where "rel" can be any
%     functor; output equations will be similar but replacing "rel"
%     with RelationName atom.
%     Closure must be a combination of one or more of these flags:
%      * 1: Reflexive
%      * 2: Symmetric
%      * 4: Transitive
%     TNorm must be one of these values:
%      * 1: Minimum
%      * 2: Gödel
%      * 3: Lukasiewicz
%


%% ext_translate_fuzzysets(+Domain, +Subsets, +NewSubsets, +RelationName, -Equations)
%
%     Computes the similarity degree between the fuzzy subsets of the
%     NewSubsets list and the fuzzy subsets of the Subsets list, and
%     returns a list of equations that represent a fuzzy relation with
%     the subsets in NewSubsets.
%     Domain must be a list with four items: [Name, Min, Max,
%     MeasureUnit]. Output equations will be like "rel(a, b, 0.5)",
%     where "rel" is RelationName atom, and "a"/"b" are subsets' names.
%     Finally, these are the syntax of the valid fuzzy subsets:
%      * name(A, B, C, D): trapezoidal subset.
%      * name(A, B, C): triangular subset.
%      * name(point(X)): domain point.
%      * name(about(X)): fuzzy domain point.
%      * name(between(X, Y)): domain range.
%      * name(about(X, Y)): fuzzy domain range.
%      * name(modifier(Subset)): modifier subset (valid modifiers are
%        "very", "extremely", "more_or_less", and "somewhat").
%


%% ext_tokenize(+StringAtom, -Tokens)
%
%     Scans the Bousi-Prolog program or query contained in StringAtom,
%     performs a lexical analysis and returns a list with all the tokens
%     found. Each token will be defined by a compound term of arity 2
%     with this syntax: "<type>(<text>, [<line>, <column>])" (e.g.
%    "comma(',', [3, 5])" or "name('aaa bbb', [10, 15])").
%


%% ext_read_shell_line(+Prompt, -String, -Arguments)
% 
%     Reads a line from standard input using editline library, which
%     allows command-line editing and history features. The line entered
%     by the user is returned as an atom in String. A list with all the
%     arguments found in the line (using space-bar characters as
%     delimiters) is returned in Arguments.
%


%% ext_load_shell_history(+File)
%
%     Loads command history used by editline library from File. If File
%     doesn't exist, it'll be created.
%


%% ext_save_shell_history(+File, +MaxCommands)
%
%     Saves command history used by editline library in File, storing
%     no more commands than the specified in MaxCommands. If File
%     already exists, it'll be overwritten.
%


%% ext_set_system_predicate_list(+List)
%
%     Sets the list of predefined predicates available in Bousi-Prolog.
%     This list will be used by shell's autocomplete feature.
%


%% ext_set_program_predicate_list(+List)
%
%     Sets the list of predicates defined in the currently loaded
%     program. This list will be used by shell's autocomplete feature.
%

