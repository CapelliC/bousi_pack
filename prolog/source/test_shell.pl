%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Test module for the Bousi-Prolog shell

:- module(test_shell, [
		run_all_shell_tests/2   % -Passed, -Failed
	]).

:- use_module(library(readutil)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Main predicates for testing Bousi-Prolog shell
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%% run_all_shell_tests(-Passed, -Failed)
%
%     Runs a set of tests for each of the available commands in the
%     Bousi-Prolog shell (except 'ls', 'sh' and 'qt).
%
%     The total number of tests passed and failed will be returned in
%     Passed and Failed, respectively.
%

run_all_shell_tests(Passed, Failed) :-
	write('------------------------------------------------------------------------------'), nl,
	write('Running Bousi-Prolog shell tests'), nl,
	write('------------------------------------------------------------------------------'), nl,
	assert(passed_tests(0)),
	assert(failed_tests(0)),
	run_other_tests,
	run_hp_tests,
	run_cd_tests,
	run_pwd_tests,
	run_ld_tests,
	run_lc_tests,
	run_sv_tests,
%	run_history_test,
	passed_tests(Passed),
	failed_tests(Failed),
	retract(passed_tests(Passed)),
	retract(failed_tests(Failed)).


%% run_other_tests
%
%     Runs other Bousi-Prolog shell tests.
%
%     @see run_command_test/3
%

run_other_tests :-
	% > Empty command
	run_command_test('', [],
	                 []),
	run_command_test('  ', [],
	                 []),
	% > Unknown command
	run_command_test('foo foo', ['foo foo'],
	                 ['Syntax error']).


%% run_hp_tests
%
%     Runs some tests for the help command ('hp').
%
%     @see run_command_test/3
%

run_hp_tests :-
	% > General help
	run_command_test('hp', ['hp'],
	                 ['ld', 'sv', 'lc', 'pwd', 'ls', 'cd', 'hp', 'qt', 'sh']),
	% > Command help
	run_command_test('hp ld', ['hp', 'ld'],
	                 ['LOAD']),
	run_command_test('hp sv', ['hp', 'sv'],
	                 ['SOLVE']),
	run_command_test('hp lc', ['hp', 'lc'],
	                 ['LAMBDA-CUT']),
	run_command_test('hp pwd', ['hp', 'pwd'],
	                 ['PRINT WORKING DIRECTORY']),
	run_command_test('hp ls', ['hp', 'ls'],
	                 ['LIST']),
	run_command_test('hp cd', ['hp', 'cd'],
	                 ['CHANGE DIR']),
	run_command_test('hp hp', ['hp', 'hp'],
	                 ['HELP']),
	run_command_test('hp qt', ['hp', 'qt'],
	                 ['QUIT']),
	run_command_test('hp sh', ['hp', 'sh'],
	                 ['SHELL']),
	% > Error cases
	run_command_test('hp foo', ['hp', 'foo'],
	                 ['ERROR']),
	run_command_test('hp foo foo', ['hp', 'foo', 'foo'],
	                 ['Wrong number of arguments']).


%% run_cd_tests
%
%     Runs some tests for the change directory command ('cd').
%
%     @see run_command_test/3
%

run_cd_tests :-
	% Creates two temporary directories
	working_directory(WorkingDir, WorkingDir),
	tmp_file('test', TempDir),
	make_directory(TempDir),
	add_path_to_files(['1 $ 3'], [SpacesTempDir], TempDir),
	make_directory(SpacesTempDir),
	% > Change to an absolute path
	atom_concat('cd ', TempDir, CDCommandTempDir),
	run_command_test(CDCommandTempDir, ['cd', TempDir],
	                 [TempDir]),
	% > Change to a relative path with spaces and non-alphanumeric characters
	run_command_test('cd \'1 $ 3\'', ['cd', '1 $ 3'],
	                 [SpacesTempDir]),
	% > Error cases
	run_command_test('cd $foo$', ['cd', '$foo$'],
	                 ['ERROR']),
	run_command_test('cd $foo$ $foo$', ['cd', '$foo$', '$foo$'],
	                 ['Wrong number of arguments']),
	% Restores the original working directory
	atom_concat('cd ', WorkingDir, CDCommandWorkingDir),
	run_command_test(CDCommandWorkingDir, ['cd', WorkingDir],
	                 [WorkingDir]),
	% Deletes the temporary directories
	delete_directory(SpacesTempDir),
	delete_directory(TempDir).


%% run_pwd_tests
%
%     Runs some tests for the print working directory command ('pwd').
%
%     @see run_command_test/3
%

run_pwd_tests :-
	% Creates a temporary directory
	working_directory(WorkingDir, WorkingDir),
	tmp_file('test', TempDir),
	make_directory(TempDir),
	% > Print working directory
	run_command_test('pwd', ['pwd'],
	                 [WorkingDir]),
	% > Change directory and print working directory again
	atom_concat('cd ', TempDir, CDCommandTempDir),
	run_command_test(CDCommandTempDir, ['cd', TempDir],
	                 [TempDir]),
	run_command_test('pwd', ['pwd'],
	                 [TempDir]),
	% > Error cases
	run_command_test('pwd foo', ['pwd', 'foo'],
	                 ['Wrong number of arguments']),
	% Restores the original working directory
	atom_concat('cd ', WorkingDir, CDCommandWorkingDir),
	run_command_test(CDCommandWorkingDir, ['cd', WorkingDir],
	                 [WorkingDir]),
	% Deletes the temporary directory
	delete_directory(TempDir).


%% run_ld_tests
%
%     Runs some tests for the load command ('ld'). In order for these
%     tests to work, this predicate must be invoked before any program
%     is loaded.
%
%     @see run_command_test/3
%

run_ld_tests :-
	% Creates a temporary folder and several temporary files
	working_directory(WorkingDir, WorkingDir),
	tmp_file('test', TempDir),
	make_directory(TempDir),
	add_path_to_files(['program.bpl', 'program.tpl', '1 prog PROG $',
	                   'ontology.ont', 'program-ontology.tpl', 'ONT ont 2 $',
	                   '1 prog PROG $.tpl', '1 prog PROG $-ONT ont 2 $.tpl',
	                   'ontology.ont.tpl'],
	                  [BPLProgramPath, TPLProgramPath, SpacesProgramPath,
	                   ONTOntologyPath, TPLOntologyPath, SpacesOntologyPath,
	                   TPLSpacesProgramPath, TPLSpacesOntologyPath,
	                   TPLONTOntologyPath],
	                  TempDir),
	tell(BPLProgramPath),
	write('a.'), nl,
	told,
	tell(ONTOntologyPath),
	write('a ~ b = 0.5.'), nl,
	told,
	tell(SpacesProgramPath),
	write('a.'), nl,
	told,
	tell(SpacesOntologyPath),
	write('a ~ b = 0.5.'), nl,
	told,
	% Changes current working directory
	atom_concat('cd ', TempDir, CDCommandTempDir),
	run_command_test(CDCommandTempDir, ['cd', TempDir],
	                 [TempDir]),
	% > Show loaded program when no program is loaded
	run_command_test('ld', ['ld'],
	                 ['No program loaded']),
	% > Load an ontology when no program is loaded
	run_command_test('ld -o ontology.ont', ['ld', '-o', 'ontology.ont'],
	                 ['ERROR']),
	% > Load a program: TPL file must be created because it doesn't exist
	run_command_test('ld program.bpl', ['ld', 'program.bpl'],
	                 ['Parsing and translating', 'Program loaded!']),
	run_command_test('sv \\+(a)', ['sv', '\\+', '(', 'a', ')'],
	                 ['No answers']),
	% > Load a program: TPL file exists and can be loaded
	run_command_test('ld program.bpl', ['ld', 'program.bpl'],
	                 ['\'program.tpl\' already exists', 'Program loaded!']),
	% > Load a program with forced rebuild
	run_command_test('ld -f program.bpl', ['ld', '-f', 'program.bpl'],
	                 ['Parsing and translating', 'Program loaded!']),
	% > Load a program: BPL file is newer than TPL so a rebuild is needed
	sleep(1),
	tell(BPLProgramPath),
	write('b.'),
	told,
	run_command_test('ld program.bpl', ['ld', 'program.bpl'],
	                 ['Parsing and translating', 'Program loaded!']),
	% > Show loaded program
	run_command_test('ld', ['ld'],
	                 [BPLProgramPath]),
	% > Load an ontology: TPL file must be created because it doesn't exist
	run_command_test('ld -o ontology.ont', ['ld', '-o', 'ontology.ont'],
	                 ['Parsing and translating', 'Ontology loaded!']),
	run_command_test('sv \\+(\\+(a))', ['sv','\\+','(','\\+','(','a',')',')'],
	                 ['No answers']),
	% > Load an ontology: TPL file exists and can be loaded
	run_command_test('ld -o ontology.ont', ['ld', '-o', 'ontology.ont'],
	                 ['\'program-ontology.tpl\' already exists', 'Ontology loaded!']),
	% > Load an ontology with forced rebuild
	run_command_test('ld -fo ontology.ont', ['ld', '-fo', 'ontology.ont'],
	                 ['Parsing and translating', 'Ontology loaded!']),
	run_command_test('ld -f -o ontology.ont', ['ld', '-f', '-o', 'ontology.ont'],
	                 ['Parsing and translating', 'Ontology loaded!']),
	run_command_test('ld -o -f ontology.ont', ['ld', '-o', '-f', 'ontology.ont'],
	                 ['Parsing and translating', 'Ontology loaded!']),
	% > Load an ontology: program's BPL file is newer than TPL so a
	%   rebuild is needed
	sleep(1),
	tell(BPLProgramPath),
	write('a.'),
	told,
	run_command_test('ld -o ontology.ont', ['ld', '-o', 'ontology.ont'],
	                 ['Parsing and translating', 'Ontology loaded!']),
	% > Load an ontology: ontology's BPL file is newer than TPL so a
	%   rebuild is needed
	sleep(1),
	tell(ONTOntologyPath),
	write('a ~ b = 0.8.'),
	told,
	run_command_test('ld -o ontology.ont', ['ld', '-o', 'ontology.ont'],
	                 ['Parsing and translating', 'Ontology loaded!']),
	% > Show loaded program and ontology
	run_command_test('ld', ['ld'],
	                 [BPLProgramPath, ONTOntologyPath]),
	% > Load an ontology without the original program's BPL file
	delete_file(BPLProgramPath),
	run_command_test('ld -o ontology.ont', ['ld', '-o', 'ontology.ont'],
	                 ['ERROR']),
	% > Load a program without the original BPL file
	run_command_test('ld program.bpl', ['ld', 'program.bpl'],
	                 ['WARNING']),
	% > Load a file with spaces, with non-alphanumeric characters and
	%   without extension
	run_command_test('ld \'1 prog PROG $\'', ['ld', '1 prog PROG $'],
	                 ['Program loaded!']),
	run_command_test('ld -o \"ONT ont 2 $\"', ['ld', '-o', 'ONT ont 2 $'],
	                 ['Ontology loaded!']),
	% > Load a file in another directory with an include/1 directive
	sleep(1),
	tell(BPLProgramPath),
	write(':- include(\'1 prog PROG $\').'),
	write('a.'), nl,
	told,
	tell(ONTOntologyPath),
	write('a ~ b = 0.5.'),
	told,
	atom_concat('cd ', WorkingDir, CDCommandWorkingDir),
	run_command_test(CDCommandWorkingDir, ['cd', WorkingDir],
	                 [WorkingDir]),
	atom_concat('ld ', BPLProgramPath, LDCommandBPL),
	run_command_test(LDCommandBPL, ['ld', BPLProgramPath],
	                 ['Program loaded!']),
	atom_concat('ld -o ', ONTOntologyPath, LDCommandONT),
	run_command_test(LDCommandONT, ['ld', '-o', ONTOntologyPath],
	                 ['Ontology loaded!']),
	% > Load a program without rules (warning)
	atom_concat('ld ', ONTOntologyPath, LDCommandONT2),
	run_command_test(LDCommandONT2, ['ld', ONTOntologyPath],
	                 ['WARNING', 'Program loaded!']),
	% > Load an ontology with rules (error)
	atom_concat('ld -o ', BPLProgramPath, LDCommandBPL2),
	run_command_test(LDCommandBPL2, ['ld', '-o', BPLProgramPath],
	                 ['ERROR', 'Ontology not loaded.']),
	% > Error cases
	run_command_test('ld $foo$', ['ld', '$foo$'],
	                 ['ERROR']),
	run_command_test('ld -f $foo$', ['ld', '-f', '$foo$'],
	                 ['ERROR']),
	run_command_test('ld -o $foo$', ['ld', '-o', '$foo$'],
	                 ['ERROR']),
	run_command_test('ld -x $foo$', ['ld', '-x', '$foo$'],
	                 ['ERROR']),
	run_command_test('ld $foo$ $foo$', ['ld', '$foo$', '$foo$'],
	                 ['Wrong number of arguments']),
	% Deletes the temporary files and folders
	delete_file(BPLProgramPath),
	delete_file(TPLProgramPath),
	delete_file(ONTOntologyPath),
	delete_file(TPLOntologyPath),
	delete_file(TPLONTOntologyPath),
	delete_file(SpacesProgramPath),
	delete_file(TPLSpacesProgramPath),
	delete_file(SpacesOntologyPath),
	delete_file(TPLSpacesOntologyPath),
	concat_atom(['rm ',TempDir,'/*.tpls'], Cmd),
	shell(Cmd),
	delete_directory(TempDir).


%% run_lc_tests
%
%     Runs some tests for the lambda cut command ('lc').
%
%     @see run_command_test/3
%

run_lc_tests :-
	% Creates a temporary folder and a temporary file
	working_directory(WorkingDir, WorkingDir),
	tmp_file('test', TempDir),
	make_directory(TempDir),
	add_path_to_files(['lambda.bpl', 'lambda.tpl', 'lambda.tpls'],
	                  [BPLProgramPath, TPLProgramPath, TPLSProgramPath],
	                  TempDir),
	tell(BPLProgramPath),
	write(':- transitivity(yes).'), nl,
	write('a ~ b = 0.8.'), nl,
	write('b ~ c = 0.3.'), nl,
	write('a.'), nl,
	told,
	% > Show lambda cut of current loaded file, which must be 0
	atom_concat('ld ', BPLProgramPath, LDCommandBPL),
	run_command_test(LDCommandBPL, ['ld', BPLProgramPath],
	                 ['Program loaded!']),
	run_command_test('lc', ['lc'],
	                 ['Current lambda-cut value is: 0']),
	run_command_test('sv \\+(\\+(c))', ['sv','\\+','(','\\+','(','c',')',')'],
	                 ['No answers']),
	% > Change lambda cut
	run_command_test('lc 0.5', ['lc', '0.5'],
	                 ['New lambda-cut value is: 0.5']),
	run_command_test('sv c', ['sv', 'c'],
%	                 ['No answers']),
                   ['\'<meta-call>\'/1: Undefined procedure: c/0']),
	% > Show new lambda cut
	run_command_test('lc', ['lc'],
	                 ['Current lambda-cut value is: 0.5']),
	% > Load file again and show new lambda cut, which must be 0
	run_command_test(LDCommandBPL, ['ld', BPLProgramPath],
	                 ['Program loaded!']),
	run_command_test('lc', ['lc'],
	                 ['Current lambda-cut value is: 0']),
	% > Error cases
	run_command_test('lc -0.5', ['lc', '-0.5'],
	                 ['ERROR']),
	run_command_test('lc 1.5', ['lc', '1.5'],
	                 ['ERROR']),
	run_command_test('lc foo', ['lc', 'foo'],
	                 ['ERROR']),
	run_command_test('lc foo foo', ['lc', 'foo', 'foo'],
	                 ['Wrong number of arguments']),
	% Deletes the temporary files and folders
	delete_file(BPLProgramPath),
	delete_file(TPLProgramPath),
	delete_file(TPLSProgramPath),
	delete_directory(TempDir).


%% run_sv_tests
%
%     Runs some tests for the solve command ('sv').
%
%     @see run_command_test/3
%

run_sv_tests :-
	% Creates a temporary folder and a temporary file
	working_directory(WorkingDir, WorkingDir),
	tmp_file('test', TempDir),
	make_directory(TempDir),
	add_path_to_files(['solve.bpl', 'solve.tpl', 'solve.tpls'],
	                  [BPLProgramPath, TPLProgramPath, TPLSProgramPath],
	                  TempDir),
	tell(BPLProgramPath),
	write('a ~ b = 0.5.'), nl,
	write('a(0).'), nl,
	write('a(1).'), nl,
	write('v(1, 2, 3).'), nl,
	write('x :- fail.'), nl,
	told,
	% > Load a file with some sample predicates
	atom_concat('ld ', BPLProgramPath, LDCommandBPL),
	run_command_test(LDCommandBPL, ['ld', BPLProgramPath],
	                 ['Program loaded!']),
	% > Solve a query with no solutions (using / without using the sv
	%   command, and ending / without ending in a dot)
	run_command_test('sv x', ['sv', 'x'],
	                 ['No answers']),
	run_command_test('sv x.', ['sv', 'x.'],
	                 ['No answers']),
	run_command_test('x', ['x'],
	                 ['No answers']),
	run_command_test('x.', ['x.'],
	                 ['No answers']),
	run_command_test('sv a(foo)', ['sv', 'a', '(', 'foo', ')'],
	                 ['No answers']),
	run_command_test('\\+(a(0))', ['\\+', '(', 'a', '(', '0', ')', ')'],
	                 ['No answers']),
	run_command_test('(10 is 9 + 2 ; fail)', ['(','10','is','9','+','2',';','fail',')'],
	                 ['No answers']),
  open('test/shell_inputs', read, Stream),
  set_stream(Stream, alias(user_input)),
	% > Solve a ground query
	write('[Press RETURN]'), nl,
	run_command_test('sv a(0)', ['sv', 'a', '(', '0', ')'],
	                 ['Yes', 'With approximation degree: 1']),
	write('[Press RETURN]'), nl,
	run_command_test('b(0).', ['b', '(', '0', ')', '.'],
	                 ['Yes', 'With approximation degree: 0.5']),
	% > Solve a ground query with several subgoals
	write('[Press RETURN]'), nl,
	run_command_test('sv b(0) ; b(1).', ['sv','b','(','0',')',';','b','(','1',')','.'],
	                 ['Yes', 'With approximation degree: 0.5']),
	write('[Press RETURN]'), nl,
	run_command_test('a(0) ; a(1)', ['a','(','0',')',';','a','(','1',')'],
	                 ['Yes', 'With approximation degree: 1']),
	% > Solve a non-ground query
	write('[Press RETURN]'), nl,
	run_command_test('v(A, B, C)', ['v', '(', 'A,', 'B,', 'C', ')'],
	                 ['Yes', 'A = 1', 'B = 2', 'C = 3',
	                  'With approximation degree: 1']),
	write('[Press RETURN]'), nl,
	run_command_test('(X is 9 + 2 ; fail)', ['(','X','is','9','+','2',';','fail',')'],
	                 ['Yes', 'X = 11', 'With approximation degree: 1']),
	% > Solve a non-ground query with several solutions
	write('[Press \';\' twice]'), nl,
	run_command_test('sv a(X)', ['sv', 'a', '(', 'X', ')'],
	                 ['X = 0', 'X = 1', 'With approximation degree: 1']),
	write('[Press \';\' twice]'), nl,
	run_command_test('sv b(X)', ['sv', 'b', '(', 'X', ')'],
	                 ['X = 0', 'X = 1', 'With approximation degree: 0.5']),
	close(Stream),
	% > Error cases
	run_command_test('sv undefined(1, 2)', ['sv', 'undefined', '(', '1', ',', '2', ')'],
	                 ['\'<meta-call>\'/1: Undefined procedure: undefined/2']),
	run_command_test('call(undefined(_))', ['call', '(', 'undefined', '(', '_', ')', ')'],
	                 ['bpl_call/1: Undefined procedure: undefined/1']),
	run_command_test('sv a(.', ['sv', 'a', '(', '.'],
	                 ['Syntax error in query']),
	run_command_test('a(', ['a', '('],
	                 ['Syntax error in command or query']),
	% Deletes the temporary files and folders
	delete_file(BPLProgramPath),
	delete_file(TPLProgramPath),
	delete_file(TPLSProgramPath),
	delete_directory(TempDir).


%% run_history_test
%
%     Runs a test for the shell's history file.
%

% run_history_test :-
% 	% Creates a temporary folder and a temporary file
% 	working_directory(WorkingDir, WorkingDir),
% 	tmp_file('test', TempDir),
% 	make_directory(TempDir),
% 	add_path_to_files(['history.bpl', 'history.tpl', 'history.tpls'],
% 	                  [BPLProgramPath, TPLProgramPath, TPLSProgramPath],
% 	                  TempDir),
% 	tell(BPLProgramPath),
% 	write('x :- fail.'), nl,
% 	told,
% 	% Loads the temporary file
% 	bplShell:ld(BPLProgramPath, [f]),
% 	(
% 		% Backups the current history file and creates a new one
% 		utilities:home_directory(HomeDirectory),
% 		bplShell:history_filename(File),
% 		add_path_to_files([File, '.bpl_history_backup'],
% 		                  [HistoryFile, HistoryFileBackup], HomeDirectory),
% 		rename_file(HistoryFile, HistoryFileBackup),
% 		tell(HistoryFile),
% 		told,
% 		% Asks the user to enter 'x', a query that will fail
% 		bplShell:load_history,
% 		write('[Press \'x\' and then ENTER]'), nl,
% 		ignore(bplShell:bpl_shell),
% 		bplShell:save_history,
% 		% Reads the contents of the history file
% 		read_file_to_codes(HistoryFile, Codes, []),
% 		% Restores the original history file
% 		delete_file(HistoryFile),
% 		rename_file(HistoryFileBackup, HistoryFile),
% 		bplShell:load_history,
% 		% Checks if the shell saved an 'x' in the history file
% 		atom_codes('x', [ExpectedCode]),
% 		Codes = [ExpectedCode|_],
% 		% Test passed
% 		write('Testing shell history... OK'), nl,
% 		passed_tests(Passed),
% 		NewPassed is Passed + 1,
% 		retract(passed_tests(Passed)),
% 		assert(passed_tests(NewPassed))
% 	;
% 		% Test failed
% 		write('Testing shell history... failed'), nl,
% 		failed_tests(Failed),
% 		NewFailed is Failed + 1,
% 		retract(failed_tests(Failed)),
% 		assert(failed_tests(NewFailed)),
% 		write('Press any key to continue '),
% 		get_single_char(_),
% 		nl
% 	),
% 	% Deletes the temporary files and folders
% 	delete_file(BPLProgramPath),
% 	delete_file(TPLProgramPath),
% 	delete_file(TPLSProgramPath),
% 	delete_directory(TempDir).


%% run_command_test(+String, +Arguments, +ExpectedMessages)
%
%     Sends the command represented by String and Arguments to the
%     Bousi-Prolog shell, and checks whether its execution generated
%     all the messages in the ExpectedMessages list. This test will
%     pass if the output contains lines starting with each of the
%     strings in the ExpectedMessages list.
%
%     The dynamic predicates passed_tests/1 and failed_tests/1 will be
%     modified after executing this test depending on its result.
%

run_command_test(String, Arguments, ExpectedMessages) :-
	writef('Testing command \'%w\'... ', [String]),
	% Executes the shell command using exactly the same calls as in
	% bplShell:bpl_shell/0, and saves its output in a temporary file
	tmp_file('test', CommandOutputFile),
	tell(CommandOutputFile),
	catch((
		bplShell:translate_command(String, Arguments, Command),
		bplShell:Command,
		(Command \== true -> nl ; true)
	), Error, (
		(Error = translate_error(ErrorMessage) ->
			% Invalid command or query
			write(ErrorMessage), nl, nl
		;
			% Exception thrown by SWI-Prolog
			message_to_string(Error, Message),
			write(Message), nl
		)
	)),
	told,
	!,
	(
		% Checks if the shell command wrote the expected messages
		% in the standard output
		check_output(CommandOutputFile, ExpectedMessages),
		% Test passed
		write('OK'), nl,
		passed_tests(Passed),
		NewPassed is Passed + 1,
		retract(passed_tests(Passed)),
		assert(passed_tests(NewPassed))
	;
		% Test failed
		write('failed'), nl,
		failed_tests(Failed),
		NewFailed is Failed + 1,
		retract(failed_tests(Failed)),
		assert(failed_tests(NewFailed)),
		% Shows the results of the test
		read_file_to_codes(CommandOutputFile, OutputCodes, []),
		atom_codes(OutputString, OutputCodes),
		writef('> Output text .......\n%w', [OutputString]), nl,
		writef('> Expected messages . %w', [ExpectedMessages]), nl,
		write('Press any key to continue '),
		get_single_char(_),
		nl
	),
	delete_file(CommandOutputFile),
	!.



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Helper predicates
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%% check_output(+File, +Texts)
%
%     Succeeds if File contains lines starting with each of the
%     strings in the Texts lists.
%

check_output(_File, []).

check_output(File, [Text|MoreTexts]) :-
	open(File, read, Stream),
	search_text_in_stream(Stream, Text),
	close(Stream),
	check_output(File, MoreTexts).


%% search_text_in_stream(+Stream, +Text)
%
%     Succeeds if a line starting with Text can be found in the
%     specified Stream. Stream will be automatically closed if the
%     predicate fails, but will remain open if it succeeds.
%

search_text_in_stream(Stream, _Text) :-
	% Checks if EOF has been reached
	at_end_of_stream(Stream), !,
	% Text wasn't found in Stream
	close(Stream),
	fail.

search_text_in_stream(Stream, Text) :-
	% Gets the next line from the stream
	read_line_to_codes(Stream, Codes),
	atom_codes(String, Codes),
	% Checks if the line starts with Text
	(sub_atom(String, 0, _, _, Text) ->
		% Text was found in this line, so the predicate must succeed
		!
	;
		% Text wasn't found in this line, so we must keep searching
		search_text_in_stream(Stream, Text)
	).


%% add_path_to_files(+Files, -FullPaths, +BasePath)
%
%     Concatenates BasePath with each of the filenames in Files and
%     returns the resulting paths in FullPaths.
%

add_path_to_files([], [], _BasePath).

add_path_to_files([File|MoreFiles], [FullPath|MoreFullPaths], BasePath) :-
	(concat_atom([_, '/'], BasePath) ->
		concat_atom([BasePath, File], FullPath)
	;
		concat_atom([BasePath, '/', File], FullPath)
	),
	add_path_to_files(MoreFiles, MoreFullPaths, BasePath).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Dynamic predicates
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%% passed_tests(?Number)                                     is dynamic
%
%     Contains the number of tests passed.
%

:- dynamic passed_tests/1.


%% failed_tests(?Number)                                     is dynamic
%
%     Contains the number of tests failed.
%

:- dynamic failed_tests/1.

