%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Bousi-Prolog command-line shell

:- module(bplShell, [
		start_bpl_shell/0       %
   ]).

:- use_module(bplHelp).
:- use_module(translator).
:- use_module(evaluator).
:- use_module(flags).
:- use_module(foreign).
:- use_module(utilities).

:- use_module(library(lists)).
:- use_module(library(system)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Setting Prolog system flags
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- set_prolog_flag(double_quotes, codes).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Command-line shell
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%% start_bpl_shell
%
%     Initializes the environment and launches Bousi-Prolog
%     command-line shell.
%

start_bpl_shell :-
%	load_history,
	set_system_predicates,
	flags:reset_bpl_flags,
	evaluator:load_tpl(''),
	bpl_shell_loop.


	
%% bpl_shell_loop
%
%     Command-line shell main loop. This predicate implements a failure-
%     driven loop that reads a command from standard input, tries to
%     execute it and then repeats the same process indefinitely.
%

bpl_shell_loop :-
	repeat,
  	bpl_shell,
 	!.



%% bpl_shell
%
%     Helper predicate called by bpl_shell_loop/0 which reads a single
%     command for input, executes it and then fails (note that
%     bpl_shell_loop/0 implements a failure-driven loop).
%
%     @see bpl_shell_loop/0
%

bpl_shell :-
	bpl_prompt(Prompt),
	read_shell_line(Prompt, String, Arguments),
	non_void_input(String),
	catch((
		% Converts the read line into an executable term
		translate_command(String, Arguments, Command),
		% Executes the command or query
		Command,
		(Command \== true -> nl ; true)
	% (catcher)
	), Error, (
		( Error = translate_error(ErrorMessage), !,
		  % Invalid command or query
		  writeln(ErrorMessage), nl
		;
			% Exception thrown by SWI-Prolog
			print_message(error, Error), nl
		)
	)),
	% This forced fail makes Prolog to go back to the repeat/0
	% predicate of bpl_shell_loop/0, thus repeating this predicate
	% again without nesting calls
	!,
	flags:get_bpl_flag(continue('no')).


			
%% non_void_input
%
%     Checks that the input does not correspond to a void input
%     as a simple Intro. (We may accept comments as void inputs 
%     in the future)
%

non_void_input('') :-
  !,
  nl,
  fail.
non_void_input(_).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Predicates for reading and translating Bousi-Prolog commands
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%% read_shell_line(+Prompt, ?String, ?Arguments)
%
%     Displays Prompt to user, reads a line from standard input and
%     returns the text of the full line in String and a list with the
%     substrings of the line delimited by whitespace in Arguments.
%     
%

read_shell_line(Prompt, String, Arguments) :-
  pl_read_shell_line(Prompt, String, ArgumentsWithQuotes),
% 	foreign:ext_read_shell_line(Prompt, String, ArgumentsWithQuotes),
	utilities:remove_quotes(ArgumentsWithQuotes, Arguments).

	
pl_read_shell_line(Prompt, String, ArgumentsWithQuotes) :-
  flush_output,
  write(Prompt),
  flush_output,
  current_input(In),
  read_line_to_codes(In, StringCodes),
  flush_output,
  atom_codes(String, StringCodes),
  split_string(StringCodes, " ()", " ()", StrArgumentsWithQuotes),
  maplist([Cs, As] >> atom_codes(As, Cs), StrArgumentsWithQuotes, ArgumentsWithQuotes).
  

%% translate_command(+String, +Arguments, -Command)
%
%     Takes a line read from Bousi-Prolog shell and returns it as an
%     executable term in Command. String must contain the full line
%     read from shell, whereas Arguments must be a list with the
%     substrings of the line delimited by whitespace.
%
%     @throws translate_error(Message) String doesn't contain a Bousi-
%      Prolog shell command nor a query.
%

translate_command(_String, [], true).
	% String contains an empty command

translate_command(_String, Arguments, Command) :-
	% Checks if string contains a Bousi-Prolog shell command
	phrase(command(Command), Arguments).

translate_command(String, Arguments, Command) :-
	% Checks if string contains the special solve command
	Arguments = [sv|_],
	% Extracts query from string and parses it
	sub_atom(String, CommandStart, _, _, 'sv'),
	QueryStart is CommandStart + 2,
	sub_atom(String, QueryStart, _, 0, QueryString),
	translator:translate_query(QueryString, Query, Bindings, Degree),
	Command = sv(Query, Bindings, Degree).

translate_command(_String, Arguments, _Command) :-
	% Checks if string contains an invalid Bousi-Prolog shell command
	Arguments = [Name|_],
	command_arguments(Name, _, _),
	throw(translate_error('Wrong number of arguments.')).

translate_command(_String, Arguments, _Command) :-
	% Checks if string contains the solve command with an invalid query
	Arguments = [sv|_],
	throw(translate_error('Syntax error in query.')).

translate_command(String, _Arguments, Command) :-
	% Checks if string contains a query
	translator:translate_query(String, Query, Bindings, Degree),
	Command = sv(Query, Bindings, Degree).

translate_command(_String, _Arguments, _Command) :-
	% String contains an unknown command or bad query
	throw(translate_error('Syntax error in command or query.')).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% DCG (Definite Clause Grammar) rules for parsing shell commands
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%% command(-Command)
%
%     DCG rule which parses a full Bousi-Prolog shell command and
%     returns it as an executable term, using command name as functor
%     and parameters and/or options as arguments.
%
%     For example, 'qt' is translated into 'qt'; 'hp pwd' is returned
%     as 'hp(pwd)'; and 'ld -f test' is translated into 'ld(test,
%     [f])'.
%

command(Command) -->
	% Command with no arguments
	[Name],
	{
		command_arguments(Name, 0, no),
		Command =.. [Name]
	}.

command(Command) -->
	% Command with a list of options and one or more arguments
	[Name], option_lists(Options), arguments(Args),
	{
		length(Args, ArgsCount),
		command_arguments(Name, ArgsCount, yes),
		append(Args, [Options], ArgsAndOptions),
		Command =.. [Name|ArgsAndOptions]
	}.

command(Command) -->
	% Command with one or more arguments
	[Name], arguments(Args),
	{
		length(Args, NumberArgs),
		command_arguments(Name, NumberArgs, no),
		Command =.. [Name|Args]
	}.


%% option_lists(-OptionList)
%
%     DCG rule which parses zero, one or several option lists preceded
%     by a dash '-' and returns a list with all the characters found in
%     the options lists.
%

option_lists(OptionList) -->
	[Options],
	{
		% Checks that Options starts with '-' and
		% gets characters of option list
		atom_chars(Options, Chars), Chars = ['-'|OptionChars]
	},
	option_lists(MoreOptions),
	{
		% Merges option lists
		append(OptionChars, MoreOptions, OptionList)
	}.

option_lists([]) -->
	% No more option lists
	[].


%% arguments(-ArgList)
%
%     DCG rule which parses zero, one or several command arguments and
%     returns them in a list.
%

arguments(ArgList) -->
	[Arg], arguments(MoreArgs),
	{
		ArgList = [Arg|MoreArgs]
	}.

arguments([]) -->
	[].



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Predicates for loading program files
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%% load_file(+Filename, +OverwriteTPL, +ResetFlags)
%
%     Loads a Bousi-Prolog program file into the Prolog database. If
%     the associated TPL file exists and is newer than the BPL file,
%     the TPL will be loaded, unless OverwriteTPL is set to 'yes'.
%     Flags will be reset if ResetFlags is set to 'yes'.
%

%% load_file(+BPLFilename, +TPLFilename, +FullBPLFilename, +FullTPLFilename, +FullTPLSFilename, +OverwriteTPL, +ResetFlags)
%
%     Internal predicate called by load_file/2, which takes the full
%     and base filenames of both the program file and the destination
%     TPL file.
%
%     @see load_file/2
%

load_file(Filename, OverwriteTPL, ResetFlags) :-
	% Backups the system flags so they can be restored if loading fails
	flags:backup_bpl_flags,
	% Gets the default BPL and TPL filenames
	get_bpl_tpl_tpls_filenames(Filename, Directory, _BaseFilename,
	                      BPLFilename, TPLFilename, TPLSFilename),
	concat_atom([Directory, '/', BPLFilename], FullBPLFilename),
	concat_atom([Directory, '/', TPLFilename], FullTPLFilename),
	concat_atom([Directory, '/', TPLSFilename], FullTPLSFilename),
	% Calls the internal predicate
	load_file(BPLFilename, TPLFilename,
	          FullBPLFilename, FullTPLFilename, FullTPLSFilename,
	          OverwriteTPL, ResetFlags).

load_file(BPLFilename, TPLFilename,
          FullBPLFilename, FullTPLFilename, FullTPLSFilename,
	      _OverwriteTPL, ResetFlags) :-
	exists_file(FullBPLFilename),
	not(exists_file(FullTPLFilename)),
	% BPL file exists but TPL doesn't, so BPL file must be translated
	load_bpl(BPLFilename, TPLFilename,
             FullBPLFilename, FullTPLFilename, FullTPLSFilename, 
             ResetFlags).

load_file(BPLFilename, TPLFilename,
          FullBPLFilename, FullTPLFilename, FullTPLSFilename,
          yes, ResetFlags) :-
	exists_file(FullBPLFilename),
	exists_file(FullTPLFilename),
	% BPL and TPL files exist, but TPL file must be overwritten
	delete_file(FullTPLFilename),
	writef('\'%w\' exists and will be overwritten.\n', [TPLFilename]), 
	load_file(BPLFilename, TPLFilename,
              FullBPLFilename, FullTPLFilename, FullTPLSFilename,
	            no, ResetFlags).

load_file(BPLFilename, TPLFilename,
          FullBPLFilename, FullTPLFilename, FullTPLSFilename,
          no, ResetFlags) :-
	exists_file(FullBPLFilename),
	exists_file(FullTPLFilename),
	% BPL and TPL files exist, and TPL can be loaded if it's newer than BPL
	(utilities:file_is_newer(FullTPLFilename, FullBPLFilename) ->
		% TPL file is newer, so it might be loaded without recompiling
		(safe_tpl(FullTPLSFilename) ->
		  % TPL file is safe to be loaded without recompiling (flags match)
  		writef('\'%w\' already exists and is being loaded...\n', [TPLFilename]),
  		load_tpl(FullBPLFilename, FullTPLFilename, ResetFlags)
  	 ;
  		% TPL file is not safe and must be overwritten
  		delete_file(FullTPLFilename), 
		  writef('\'%w\' must be reloaded.\n', [TPLFilename]),
  		load_bpl(BPLFilename, TPLFilename,
                   FullBPLFilename, FullTPLFilename, FullTPLSFilename, 
                   ResetFlags)
		)
	;
		% TPL file is older and must be overwritten
		delete_file(FullTPLFilename), 
		writef('\'%w\' is older and will be overwritten.\n', [TPLFilename]),
		load_bpl(BPLFilename, TPLFilename,
                 FullBPLFilename, FullTPLFilename, FullTPLSFilename, 
                 ResetFlags)
	).

load_file(BPLFilename, TPLFilename,
          FullBPLFilename, FullTPLFilename, _FullTPLSFilename,
	      _OverwriteTPL, ResetFlags) :-
	not(exists_file(FullBPLFilename)),
	exists_file(FullTPLFilename),
	% TPL file exists but BPL doesn't, so TPL file must be loaded
	% (a warning is shown because the original file doesn't exist)
	writef('WARNING: \'%w\' does not exist.\n', [BPLFilename]),
	writef('\'%w\' already exists and is being loaded...\n', [TPLFilename]),
	load_tpl(FullBPLFilename, FullTPLFilename, ResetFlags).

load_file(BPLFilename, _TPLFilename,
          FullBPLFilename, FullTPLFilename, _FullTPLSFilename, _OverwriteTPL, _ResetFlags) :-
	not(exists_file(FullBPLFilename)),
	not(exists_file(FullTPLFilename)),
	% None of BPL and TPL files exist
	writef('ERROR: \'%w\' does not exist.\n', [BPLFilename]).


%% load_bpl(+BPLFilename, +TPLFilename, +FullBPLFilename,
%           +FullTPLFilename, +FullTPLSFilename, +ResetFlags)
%
%     Parses and translates a BPL source-code file, converts it into an
%     intermediate TPL file, and then loads the latter file.
%

load_bpl(BPLFilename, TPLFilename,
         FullBPLFilename, FullTPLFilename, FullTPLSFilename, ResetFlags) :-
	(ResetFlags==yes -> flags:reset_bpl_flags; true),
	retractall(evaluator:sim(_, _, _)), % WARNING: If translate_program fails, previous sim is lost
 	retractall(evaluator:sim(_, _, _, _)),
	writef('Parsing and translating \'%w\'...\n', [BPLFilename]),
	translator:translate_program(FullBPLFilename, '', FullTPLFilename, FullTPLSFilename),
	writef('\'%w\' is being loaded...\n', [TPLFilename]),
	load_tpl(FullBPLFilename, FullTPLFilename, ResetFlags).

load_bpl(_BPLFilename, _TPLFilename,
         _FullBPLFilename, _FullTPLFilename, _FullTPLSFilename, _ResetFlags) :-
	% This is only executed if translate_program/3 fails
	flags:restore_bpl_flags,
	write('Program not loaded.\n').


%% load_tpl(+FullBPLFilename, +FullTPLFilename, +ResetFlags)
%
%     Loads an intermediate TPL file into the 'evaluator' module and
%     saves the path of the new program loaded.
%

load_tpl(FullBPLFilename, FullTPLFilename, ResetFlags) :-
	(ResetFlags==yes -> flags:reset_bpl_flags; true),
	evaluator:load_tpl(FullTPLFilename),
	set_files_loaded(FullBPLFilename, ''),
	flags:get_bpl_flag(program_prefix(ProgramPrefix)),
	set_program_predicates(ProgramPrefix),
	write('Program loaded!\n').

load_tpl(_FullBPLFilename, _FullTPLFilename, _ResetFlags) :-
	% This is only executed if evaluator:load_tpl/1 fails
	flags:restore_bpl_flags,
	write('Program not loaded.\n').


%% safe_tpl(+FullTPLSFilename)
%
%     Checks whether it is safe to use an existing TPL file, 
%     i.e., the current lambda-cut and filtering must match
%     with the values for which the TPL was generated (kept
%     in the TPLS file, which holds all bpl flags at compilation-time)
%

safe_tpl(FullTPLSFilename) :-
  exists_file(FullTPLSFilename),
	evaluator:current_file(OldTPLFile),
	(OldTPLFile \== '' ->
	  concat_atom([OldTPLFile,'s'],OldTPLSFile),
		unload_file(OldTPLSFile)
	;
		true
	),
	consult(FullTPLSFilename),
	tpl_flags(Flags),
	flags:get_bpl_flag(lambda_cut(LambdaValue)),
	memberchk(lambda_cut(LambdaValue),Flags),
	flags:get_bpl_flag(filtering(Filtering)),
	memberchk(filtering(Filtering),Flags).
  



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Predicates for loading ontology files
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%% load_ontology(+Filename, +OverwriteTPL, +ResetFlags)
%
%     Loads a Bousi-Prolog ontology file into the Prolog database. If
%     no Bousi-Prolog program has been loaded before, an error is shown.
%     If the associated TPL file exists and is newer than both the
%     ontology file and the currently loaded program, the TPL will be
%     loaded, unless OverwriteTPL is set to 'yes'.
%     Flags will be reset if ResetFlags is set to 'yes'.
%

%% load_ontology(+BPLFilename, +ONTFilename, +TPLFilename, +FullBPLFilename, +FullONTFilename, +FullTPLFilename, +FullTPLSFilename, +OverwriteTPL, +ResetFlags)
%
%     Internal predicate called by load_ontology/2, which takes the
%     full and base filenames of the currently loaded program, the
%     ontology file and the destination TPL file.
%
%     @see load_ontology/2
%

load_ontology(Filename, OverwriteTPL, ResetFlags) :-
	% Checks that a programs has been loaded before
	last_program_loaded(ProgramPath, _OntologyPath),
	ProgramPath \== '',
	% Backups the system flags so they can be restored if loading fails
	flags:backup_bpl_flags,
	% Gets the default BPL, TPL and ONT filenames
	get_bpl_tpl_tpls_filenames(ProgramPath, Directory, BaseFilename,
	                      BPLFilename, _UnusedTPLFilename, _UnusedTPLSFilename),
	get_ont_filename(Filename, ONTDirectory, BaseONTFilename, ONTFilename),
	get_ontology_tpl_tpls_filename(BaseFilename, BaseONTFilename, TPLFilename, TPLSFilename),
	concat_atom([Directory, '/', BPLFilename], FullBPLFilename),
	concat_atom([ONTDirectory, '/', ONTFilename], FullONTFilename),
	concat_atom([Directory, '/', TPLFilename], FullTPLFilename),
	concat_atom([Directory, '/', TPLSFilename], FullTPLSFilename),
	% Calls the internal predicate
	load_ontology(BPLFilename, ONTFilename, TPLFilename,
                FullBPLFilename, FullONTFilename, 
                FullTPLFilename, FullTPLSFilename, 
                OverwriteTPL, ResetFlags).

load_ontology(_Filename, _OverwriteTPL, _ResetFlags) :-
	last_program_loaded('', _),
	write('ERROR: A program must be loaded before loading an ontology.\n').

load_ontology(BPLFilename, ONTFilename, TPLFilename,
              FullBPLFilename, FullONTFilename, FullTPLFilename, FullTPLSFilename,
	          _OverwriteTPL, ResetFlags) :-
	exists_file(FullBPLFilename),
	exists_file(FullONTFilename),
	not(exists_file(FullTPLFilename)),
	% BPL and ontology files exist but TPL doesn't, so BPL + ontology must
	% be translated
	load_bpl_with_ontology(BPLFilename, ONTFilename, TPLFilename,
                           FullBPLFilename, FullONTFilename, FullTPLFilename, FullTPLSFilename, ResetFlags).

load_ontology(BPLFilename, ONTFilename, TPLFilename,
              FullBPLFilename, FullONTFilename, FullTPLFilename, FullTPLSFilename,
	          yes, ResetFlags) :-
	exists_file(FullBPLFilename),
	exists_file(FullONTFilename),
	exists_file(FullTPLFilename),
	% BPL, TPL and ontology files exist, but TPL file must be overwritten
	delete_file(FullTPLFilename),
	writef('\'%w\' exists and will be overwritten.\n', [TPLFilename]),
	load_bpl_with_ontology(BPLFilename, ONTFilename, TPLFilename,
                           FullBPLFilename, FullONTFilename, FullTPLFilename, FullTPLSFilename, ResetFlags).

load_ontology(BPLFilename, ONTFilename, TPLFilename,
              FullBPLFilename, FullONTFilename, FullTPLFilename, FullTPLSFilename,
	          no, ResetFlags) :-
	exists_file(FullBPLFilename),
	exists_file(FullONTFilename),
	exists_file(FullTPLFilename),
	% BPL, TPL and ontology files exist, and TPL can be loaded if it's newer
	% than both BPL and ontology
	((utilities:file_is_newer(FullTPLFilename, FullBPLFilename),
	  utilities:file_is_newer(FullTPLFilename, FullONTFilename)) ->
		% TPL file is newer than ontology and BPL, so it ,ight be loaded
		% without recompiling
		(safe_tpl(FullTPLSFilename) ->
		  % TPL file is safe to be loaded without recompiling (flags match)
  		writef('\'%w\' already exists and is being loaded...\n', [TPLFilename]),
	  	load_tpl_with_ontology(FullBPLFilename, FullONTFilename, FullTPLFilename, 
                   ResetFlags)
	  ;
  		% TPL file is not safe and must be overwritten
  		delete_file(FullTPLFilename),
  		writef('\'%w\' must be reloaded.\n', [TPLFilename]),
  		load_bpl_with_ontology(BPLFilename, ONTFilename, TPLFilename,
  		                       FullBPLFilename, FullONTFilename, FullTPLFilename, FullTPLSFilename, ResetFlags)
	  )
	;
		% TPL file is older than ontology or BPL and must be overwritten
		delete_file(FullTPLFilename),
		writef('\'%w\' is older and will be overwritten.\n', [TPLFilename]),
		load_bpl_with_ontology(BPLFilename, ONTFilename, TPLFilename,
		                       FullBPLFilename, FullONTFilename, FullTPLFilename, FullTPLSFilename, ResetFlags)
	).	

load_ontology(_BPLFilename, ONTFilename, _TPLFilename,
              FullBPLFilename, FullONTFilename, _FullTPLFilename, _FullTPLSFilename,
	          _OverwriteTPL, _ResetFlags) :-
	exists_file(FullBPLFilename),
	not(exists_file(FullONTFilename)),
	% Original BPL file exists but ontology doesn't
	writef('ERROR: \'%w\' ontology do not exist.\n', [ONTFilename]).

load_ontology(_BPLFilename, _ONTFilename, _TPLFilename,
              FullBPLFilename, _FullONTFilename, _FullTPLFilename, _FullTPLSFilename,
	          _OverwriteTPL, _ResetFlags) :-
	not(exists_file(FullBPLFilename)),
	% Original BPL file doesn't exist
	write('ERROR: Program\'s original BPL file is needed to load an ontology.\n').


%% load_bpl_with_ontology(+BPLFilename, +ONTFilename, +TPLFilename, +FullBPLFilename, +FullONTFilename, +FullTPLFilename, +FullTPLSFilename, +ResetFlags)
%
%     Parses and translates a BPL program together with a BPL ontology,
%     converts them into a single intermediate TPL file, and then loads
%     the latter file.
%

load_bpl_with_ontology(BPLFilename, ONTFilename, TPLFilename,
                       FullBPLFilename, FullONTFilename, FullTPLFilename, FullTPLSFilename, ResetFlags) :-
	(ResetFlags==yes -> flags:reset_bpl_flags; true),
	writef('Parsing and translating \'%w\' using ontology \'%w\'...\n',
	       [BPLFilename, ONTFilename]),
	translator:translate_program(FullBPLFilename, FullONTFilename, FullTPLFilename, FullTPLSFilename),
	writef('\'%w\' is being loaded...\n', [TPLFilename]),
	load_tpl_with_ontology(FullBPLFilename, FullONTFilename, FullTPLFilename, ResetFlags).

load_bpl_with_ontology(_BPLFilename, _ONTFilename, _TPLFilename,
                       _FullBPLFilename, _FullONTFilename, _FullTPLFilename, _FullTPLSFilename, _ResetFlags) :-
	% This is only executed if translate_program/3 fails
	flags:restore_bpl_flags,
	write('Ontology not loaded.\n').


%% load_tpl_with_ontology(+FullBPLFilename, +FullONTFilename, 
%%                        +FullTPLFilename, +ResetFlags)
%
%     Loads an intermediate TPL file compiled from a BPL and an
%     ontology into the 'evaluator' module and saves the path of the
%     new program and ontology loaded.
%

load_tpl_with_ontology(FullBPLFilename, FullONTFilename, FullTPLFilename, ResetFlags) :-
	(ResetFlags==yes -> flags:reset_bpl_flags; true),
	evaluator:load_tpl(FullTPLFilename),
	set_files_loaded(FullBPLFilename, FullONTFilename),
	flags:get_bpl_flag(program_prefix(ProgramPrefix)),
	set_program_predicates(ProgramPrefix),
	write('Ontology loaded!\n').

load_tpl_with_ontology(_FullBPLFilename, _FullONTFilename, _FullTPLFilename, _ResetFlags) :-
	% This is only executed if evaluator:load_tpl/1 fails
	flags:restore_bpl_flags,
	write('Ontology not loaded.\n').



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Predicates for handling filenames
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%% get_bpl_tpl_tpls_filenames(+Filename, -Directory, -BaseFilename, -BPLFilename, -TPLFilename)
%
%     Splits a Filename into its absolute path and its base (relative)
%     filename, and then adds the '.bpl' and '.tpl' extension to the
%     base filename.
%
%     For example, given Filename = '/example/file.bpl', this predicate
%     will return Directory = '/example', BaseFilename = 'file',
%     BPLFilename = 'file.bpl' and TPLFilename = 'file.tpl'.
%

get_bpl_tpl_tpls_filenames(Filename, Directory, BaseFilename, BPLFilename, TPLFilename, TPLSFilename) :-
	% Converts filename into an absolute path and extracts
	% its relative filename and directory 
	absolute_file_name(Filename, AbsoluteFilename),
	file_base_name(AbsoluteFilename, RelativeFilename),
	file_directory_name(AbsoluteFilename, Directory),
	% Checks filename extension
	file_name_extension(FilenameNoExtension, Extension, RelativeFilename),
	downcase_atom(Extension, LowerCaseExtension),
	((LowerCaseExtension == 'bpl' ; LowerCaseExtension == 'tpl' ;
	  LowerCaseExtension == '') ->
		% Base filename from "file.bpl" or "file." is "file"
		BaseFilename = FilenameNoExtension
	;
		% Base filename from "file.new" is "file.new"
		BaseFilename = RelativeFilename
	),
	% Builds BPL and TPL filenames
	concat_atom([BaseFilename, '.', bpl], BPLFilenameAux),
	concat_atom([Directory, '/', BPLFilenameAux], FullBPLFilenameAux),
	concat_atom([Directory, '/', BaseFilename], FullBaseFilaname),
	((exists_file(FullBPLFilenameAux) ; not(exists_file(FullBaseFilaname))) ->
		BPLFilename = BPLFilenameAux
	;
		BPLFilename = BaseFilename
	),
	concat_atom([BaseFilename, '.', tpl], TPLFilename),
	concat_atom([TPLFilename, 's'], TPLSFilename).


%% get_ont_filename(+Filename, -Directory, -BaseFilename, -ONTFilename)
%
%     Splits a Filename into its absolute path and its base (relative)
%     filename, and then adds the '.ont' extension to the base
%     filename.
%
%     For example, given Filename = '/example/ontology.ont', this
%     predicate will return Directory = '/example', BaseFilename =
%     'ontology', ONTFilename = 'ontology.ont'.
%

get_ont_filename(Filename, Directory, BaseFilename, ONTFilename) :-
	% Converts filename into an absolute path and extracts
	% relative filename and directory 
	absolute_file_name(Filename, AbsoluteFilename),
	file_base_name(AbsoluteFilename, RelativeFilename),
	file_directory_name(AbsoluteFilename, Directory),
	% Checks filename extension
	file_name_extension(FilenameNoExtension, Extension, RelativeFilename),
	downcase_atom(Extension, LowerCaseExtension),
	((LowerCaseExtension == 'ont' ; LowerCaseExtension == '') ->
		% Base filename from "file.ont" or "file." is "file"
		BaseFilename = FilenameNoExtension
	;
		% Base filename from "file.new" is "file.new"
		BaseFilename = RelativeFilename
	),
	% Builds ONT filename
	concat_atom([BaseFilename, '.', ont], ONTFilenameAux),
	concat_atom([Directory, '/', ONTFilenameAux], FullONTFilenameAux),
	concat_atom([Directory, '/', BaseFilename], FullBaseFilename),
	((exists_file(FullONTFilenameAux) ; not(exists_file(FullBaseFilename))) ->
		ONTFilename = ONTFilenameAux
	;
		ONTFilename = BaseFilename
	).


%% get_ontology_tpl_tpls_filename(+ProgramBaseFilename, +OntologyBaseFilename, -TPLFilename, -TPLSFilename)
%
%     Concatenates the base filenames of a program and an ontology and
%     then adds the '.tpl' and '.tpls' extensions to the resulting filenames.
%
%     For example, given ProgramBaseFilename = 'file' and
%     OntologyBaseFilename = 'ontology', this predicate will return
%     TPLFilename  = 'file-ontology.tpl'.
%     TPLSFilename = 'file-ontology.tpls'.
%

get_ontology_tpl_tpls_filename(ProgramBaseFilename, OntologyBaseFilename, TPLFilename, TPLSFilename) :-
	concat_atom([ProgramBaseFilename, '-', OntologyBaseFilename, '.', tpl], TPLFilename),
	concat_atom([TPLFilename, 's'], TPLSFilename).

	

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Predicates for solving a query
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%% sv(+Query, +Bindings, ?Degree)
%
%     Tries to solve the specified Bousi-Prolog Query and, if an answer
%     is found, shows the approximation degree of the result and the
%     list of variable bindings. Afterwards, user can ask for more
%     answers.
%

%% sv_aux(+Query, +Bindings, ?Degree)
%
%     Internal predicate used by sv/3.
%

sv(Query, Bindings, Degree) :-
	sv_aux(Query, Bindings, Degree),
	% This cut is used to discard all remaining choice points of the
	% query, since it's not going to be solved again; in other words,
	% this cut "frees" all memory used by the query
	!.

sv_aux(Query, [], Degree) :- 
	% Ground query (i.e., query has no free variables)
	evaluator:solve_goal(Query),
	writeln('Yes'),
	writef('With approximation degree: %w ', [Degree]),
	sv_actions.

sv_aux(Query, Bindings, Degree) :- 
	% Non ground query (i.e., query has one or more free variables)
	Bindings \== [],
	evaluator:solve_goal(Query),
	print_answer(Bindings), nl,
	writef('With approximation degree: %w ', [Degree]),
	sv_actions.

sv_aux(_Query, _Bindings, _Degree) :-
	% Query couldn't be solved or there're no more answers
	writeln('No answers').


%% sv_actions
%
%     Prompts the user for an action after solving a query. User can
%     ask for more answers or finish the query.
%

sv_actions :-
	flush_output,
	get_single_char(Code),
	char_code(Char, Code),
	(member(Char, [';', 'n', 'r', ' ', '\t']) ->
		% Tries to solve the query again
		% If in host_safe mode (for the web interface) do not echo the ';' 
		% because the web server already does it.
		% This is because get_single_char/1 does not output the user input 
		% character in the console application, by contrast with work like 
		% read_line_to_codes, which does
    (flags:get_bpl_flag(host_safe('no')) -> writeln(';') ; true),
		fail
	;
	(member(Char, ['c', 'a', '\r', '\n']) ->
		% Finishes query
		writeln('.'),
		writeln('Yes')
	;
	(member(Char, ['h', '?']) ->
		% Shows available commands
		nl, nl,
		writeln('Available actions:'),
		writeln(' ; (n, r, space, tab): redo'),
		writeln(' c (a, return): exit'),
		writeln(' h (?): help'), nl,
		write('Action? '), sv_actions
	;
		% Invalid action
		nl,
		write('Unknown action: '), write(Char), writeln(' (h for help)'),
		write('Action? '), sv_actions
	))).


%% print_answer(+Bindings)
%
%     Prints the contents of a list of variable bindings that
%     represents the answer of a query. After writing the last binding,
%     a newline is not added in case user wants to ask for more
%     answers.
%

print_answer([]).

print_answer([Name = Value]) :-
	write(Name), write(' = '), write(Value).

print_answer([Name = Value|MoreBindings]) :-
	MoreBindings \== [],
	print_answer([Name = Value]), nl,
	print_answer(MoreBindings).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Shell command predicates
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%% hp
%
%     Shows some general help about Bousi-Prolog.
%

hp :-
	bplHelp:bpl_help.


%% hp(+Topic)
%
%     Shows some help about a specific topic or command.
%

hp(Topic) :-
	bplHelp:command_help(Topic).

hp(_Topic) :-
	writeln('ERROR: Unknown topic.').


%% qt
%
%     Stops Bousi-Prolog execution.
%

qt :-
	writeln('Exiting the system...'), nl,
%	save_history,
	halt.


%% bk
%
%     Breaks Bousi-Prolog execution.
%

bk :-
  check_safe_execution,
  !,
  flags:set_bpl_flag(continue('no')),
	writeln('Exiting to Prolog... Type ''exit.'' to return to Bousi~Prolog.'), nl.
%	save_history.
bk.


%% ts
%
%     Tests Bousi-Prolog test programs.
%

ts :-
  check_safe_execution,
  !,
	writeln('Testing...'), nl,
	test:main_test.
ts.


%% ls
%
%     Lists the contents of current working directory, using the ls/1
%     SWI-Prolog predefined predicate.
%

ls :-
  check_safe_execution,
  !,
	shell:ls.
ls.


%% pwd
%
%     Prints the full path of the current working directory.
%

pwd :-
  check_safe_execution,
  !,
	working_directory(CurrentWorkingDir, CurrentWorkingDir),
	writeln('Current working directory is:'),
	writeln(CurrentWorkingDir).
pwd.


%% cd(+Directory)
%
%     Changes current working directory to Directory, which can be an
%     absolute or a relative path.
%

cd(Directory) :- 	
  check_safe_execution,
  !,
	(exists_directory(Directory)
	 ->
  	working_directory(_OldDirectory, Directory),
  	working_directory(NewWorkingDir, NewWorkingDir),
  	writeln('New working directory is:'),
  	writeln(NewWorkingDir)
	 ;
  	writef('ERROR: Directory \'%w\' does not exist.\n', [Directory])).
cd(_Directory).


%% sh
%
%     Starts an interactive command-line shell. On Unix/Linux, default
%     shell is /bin/sh, but can be overriden with $SHELL environment
%     variable; on Windows, default shell is cmd.exe, but can be
%     overriden with %COMSPEC% environment variable.
%

sh :-
  check_safe_execution,
  !,
	(current_prolog_flag(windows, true)
	 ->
  	writeln('Starting a Windows shell...'),
  	writeln('Type \'exit\' to return to Bousi~Prolog.'),
  	(getenv('COMSPEC', Shell), ! ; Shell = 'cmd.exe'),
  	% shell/2 predicate works well on Windows Vista and 7, but hangs on
  	% XP, that's because we use win_shell/2 instead, the only difference
  	% being that win_shell/2 doesn't wait until task ends
  	win_shell(open, Shell)
   ;
  	writeln('Starting a Unix shell...'),
  	writeln('Type \'exit\' to return to Bousi~Prolog.'), nl,
  	(getenv('SHELL', Shell), ! ; Shell = '/bin/sh'),
  	shell(Shell, _)
	).
sh.


%% lc
%
%     Shows current lambda-cut value, i.e., the lower bound allowed for
%     the resulting approximation degree of a weak unification.
%

lc :-
	flags:get_bpl_flag(lambda_cut(LambdaValue)),
	writef('Current lambda-cut value is: %w\n', [LambdaValue]).


%% lc(+Lambda)
%
%     Changes current lambda-cut value, i.e., the lower bound allowed
%     for the resulting approximation degree of a weak unification.
%

lc(Lambda) :-
	catch((
		% Converts Lambda atom into a number
		atom_chars(Lambda, LambdaChars),
		number_chars(LambdaValue, LambdaChars),
		LambdaValue >= 0, LambdaValue =< 1, !,
		% Changes lambda-cut value
		flags:remove_bpl_flag(lambda_cut(OldLambda)),
		flags:add_bpl_flag(lambda_cut(LambdaValue)),
		reload_on_lambda_change(OldLambda, LambdaValue),
		writef('New lambda-cut value is: %w\n', [LambdaValue])
	% (catcher)
	), error(syntax_error(illegal_number), _), (
		% Lambda-cut is not a number
		fail
	)).

lc(Lambda) :-
	writef('ERROR: \'%w\' is a wrong lower bound for the approximation \c
	        degree.\n', [Lambda]),
	writeln('Lambda-cut value must be a number in range [0.0, 1.0].').

	
% reload_on_lambda_change(+OldLambda, +NewLambda)
%   Reload the loaded program, if any, on lambda change.

% If filtering is not enabled, do not reload
reload_on_lambda_change(_OldLambda, _NewLambda) :-
	flags:get_bpl_flag(filtering(false)),
	!.

% Otherwise, test if needed on flag change
reload_on_lambda_change(OldLambda, NewLambda) :-
  reload_on_flag_change(OldLambda, NewLambda).

  
% reload_on_flag_change(+OldFlag, +NewFlag)
%   Reload the loaded program, if any, on flag change.

% No change
reload_on_flag_change(Flag, Flag) :-
  !.

% No program loaded. An ontology requires a program previously loaded
reload_on_flag_change(_OldFlag, _NewFlag) :-
	last_program_loaded('', _), !,
  !.

% Otherwise, recompiling is necessary without resetting flags
reload_on_flag_change(_OldFlag, _NewFlag) :-
  reload([r]).
  
% Reload resetting flags
reload :-
  reload([]).

% Either for a program:
reload(Options) :-
	last_program_loaded(Program, ''),
	ld(Program, [f | Options]), % f: force, r: reload (don't reset flags)
	!.
	
% Or an ontology:
reload(Options) :-
	last_program_loaded(_Program, Ontology),
	ld(Ontology, [f, o | Options]), % f: force, o: ontology, r: reload (don't reset flags)
	!.
	

% reload_on_extra_equations(+Equations, +Added)
%   Reload the current program (and ontology) if new equations
%     have been added.

% No added equations
reload_on_extra_equations(_Equations, no) :-
  !.

% Equations added: reload with new equations
reload_on_extra_equations(Equations, yes) :-
	last_program_loaded(Program, _Ontology),
	Program\=='',
	!,
	% Save current program file
	atom_concat(Program, bak, ProgramBak),
	copy_file(Program, ProgramBak),
	% Add Equations to program
	append(Program),
	nl,
	findall(_, (member(sim(X,Y,D), Equations), writef('%w~%w=%w.\n',[X,Y,D])), _),
	told,
	% Reload the extended program
	reload,
	% Restore original program file
	copy_file(ProgramBak, Program),
	delete_file(ProgramBak).

% No program already loaded. Don't reload. 
%   This typically will raise an exception on solving the query
reload_on_extra_equations(_Equations, yes).


%% ld
%
%     Shows the name of the program and the ontology that are currently
%     loaded into memory.
%

ld :-
	last_program_loaded('', _), !,
	writeln('No program loaded.').

ld :-
	last_program_loaded(Program, ''), !,
	writeln('Current loaded program is:'),
	writeln(Program).

ld :-
	last_program_loaded(Program, Ontology),
	writeln('Current loaded program is:'),
	writeln(Program),
	writeln('Current loaded ontology is:'),
	writeln(Ontology).


%% ld(+Filename)
%
%     Loads a Bousi-Prolog source code file into memory. If TPL file
%     exists and is newer than BPL file, TPL will be loaded.
%     Flags will be reset.
%

ld(Filename) :-
	load_file(Filename, no, yes).


%% ld(+Filename, +Options)
%
%     Loads a Bousi-Prolog source code file into memory as a program,
%     or as an ontology if 'o' option is specified. If TPL file exists
%     and is newer than source code file, TPL will be loaded, unless
%     the 'f' option is specified. Flags will be reset unless the 'r' 
%     option is specified (recompiling).
%

ld(Filename, Options) :-
	is_list(Options),
	subset(Options, [f, o, r]),
	(member(f, Options) ->
		OverwriteTPL = yes
	;
		OverwriteTPL = no
	),
	(member(r, Options) ->
		ResetFlags = no
	;
		ResetFlags = yes
	),
	(member(o, Options) ->
		load_ontology(Filename, OverwriteTPL, ResetFlags)
	;
		load_file(Filename, OverwriteTPL, ResetFlags)
	).

ld(_Filename, Options) :-
	member(Option, Options),
	not(member(Option, [f, o, r])),
	writef('ERROR: Unknown option: \'%w\'.\n', [Option]).

	
	
%% fl
%
%     Shows whether filtering is enabled.
%

fl :-
	flags:get_bpl_flag(filtering(Boolean)),
	writef('Filtering enabled: %w\n', [Boolean]).


%% fl(+Boolean)
%
%     Changes current filtering behaviour: if Boolean=true, then
%     apply filtering; otherwise, do not.
%

fl(Boolean) :-
	% Checks valid value
	memberchk(Boolean, [true, false]),
	!,
	% Changes filtering value
	flags:remove_bpl_flag(filtering(OldBoolean)),
	flags:add_bpl_flag(filtering(Boolean)),
  reload_on_flag_change(OldBoolean, Boolean),
	writef('New filtering is: %w\n', [Boolean]).

fl(Boolean) :-
	writef('ERROR: \'%w\' is a wrong value. Use either \c
	        \'true\' or \'false\'.\n', [Boolean]).

%% check_safe_execution
%
%     Succeeds if host safe mode is disabled, fails and informs otherwise.
%

check_safe_execution :-
	flags:get_bpl_flag(host_safe('no')),
	!.
check_safe_execution :-
	write('ERROR: This command cannot be executed in online mode. Use the desktop application instead.'), 
  nl,
	nl,
	fail.




%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Predicates for reading and writing command history
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%% load_history
%
%     Reads the command history used by editline library from the
%     '.bpl_history' file located in the user home directory.
%

% load_history :-
% 	history_filename(File),
% 	utilities:home_directory(HomeDirectory),
% 	concat_atom([HomeDirectory, '/', File], HistoryFile),
% 	(exists_file(HistoryFile) ->
% 		true
% 	;
% 		% Creates a new, empty history file
% 		telling(CurrentOutput),
% 		tell(HistoryFile), told,
% 		tell(CurrentOutput)
% 	),
% 	foreign:ext_load_shell_history(HistoryFile).


%% save_history
%
%     Writes the command history used by editline library in the
%     '.bpl_history' file located in the user home directory.
%

% save_history :-
% 	history_filename(File),
% 	max_history_commands(MaxCommands),
% 	utilities:home_directory(HomeDirectory),
% 	concat_atom([HomeDirectory, '/', File], HistoryFile),
% 	foreign:ext_save_shell_history(HistoryFile, MaxCommands).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Predicates for managing lists of predicate names
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%% set_system_predicates
%
%     Retrieves the list of the names of the predefined predicates
%     in SWI-Prolog and sets them to be used by the shell's
%     autocomplete feature.
%

set_system_predicates :-
  true.
% 	utilities:get_predicates(PredicateNames),
% 	foreign:ext_set_system_predicate_list(PredicateNames).


%% set_program_predicates(+Prefix)
%
%     Retrieves the list of the names of the predicates defined in the
%     currently loaded program and sets them to be used by the shell's
%     autocomplete feature. Prefix represents the prefix associated to
%     the loaded program.
%

set_program_predicates(Prefix) :-
	utilities:get_predicates_modules([evaluator], PredicateNames),
	atom_concat(Prefix, '_', FullPrefix),
	utilities:remove_prefixes(PredicateNames, _RealNames, FullPrefix).
%	foreign:ext_set_program_predicate_list(RealNames).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Miscellaneous predicates
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%% set_files_loaded(+File, +Ontology)
%
%     Sets File and Ontology as the new paths of the Bousi-Prolog
%     program and ontology that are currently loaded into memory.
%     Besides, updates the program_prefix flag from File.
%

set_files_loaded(File, Ontology) :-
	last_program_loaded(OldFile, OldOntology),
	retract(last_program_loaded(OldFile, OldOntology)),
	assert(last_program_loaded(File, Ontology)),
	utilities:simplify_filename(File, SimplifiedFilename),
	flags:remove_bpl_flag(program_prefix(_OldProgramPrefix)),
	flags:add_bpl_flag(program_prefix(SimplifiedFilename)).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Constant predicates
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%% bpl_prompt(?Prompt)
%
%     Unifies Prompt with the prompt that will be shown to the user.
%

bpl_prompt('BPL> ').


%% history_filename(?Filename)
%
%     Unifies Filename with the name of the file where the Bousi-Prolog
%     command history will be saved.
%

% history_filename('.bpl_history').


%% max_history_commands(?Max)
%
%     Unifies Max with the maximum number of commands that may be saved
%     in the Bousi-Prolog command history.
%

% max_history_commands(100).


%% command_arguments(?Command, ?NArgs, ?Options)
%
%     For each Bousi-Prolog shell command (except sv), unifies Command
%     with the command name, NArgs with the number of required
%     arguments, and Options with 'yes' if the command can take a list
%     of options or 'no' otherwise.
%

command_arguments(hp, 0, no).
command_arguments(lc, 0, no).
command_arguments(fl, 0, no).
command_arguments(ld, 0, no).
command_arguments(ls, 0, no).
command_arguments(pwd, 0, no).
command_arguments(ts, 0, no).
command_arguments(bk, 0, no).
command_arguments(qt, 0, no).
command_arguments(sh, 0, no).
command_arguments(cd, 1, no).
command_arguments(hp, 1, no).
command_arguments(lc, 1, no).
command_arguments(fl, 1, no).
command_arguments(ld, 1, no).
command_arguments(ld, 1, yes).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Dynamic predicates
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%% last_program_loaded(?Program, ?Ontology)                  is dynamic
%
%     Dynamic predicate which stores the full paths of the Bousi-Prolog
%     Program and Ontology that are currently loaded into memory.
%     Program and Ontology can be '' (an empty string) if no program
%     has been loaded yet, whereas Ontology can also be '' if just a
%     program is currently loaded.
%

:- dynamic last_program_loaded/2.

last_program_loaded('', '').

