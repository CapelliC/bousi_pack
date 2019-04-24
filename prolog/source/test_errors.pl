%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Test module for error and warning messages

:- module(test_errors, [
		run_all_error_and_warning_tests/3 % +BasePath, -Passed, -Failed
	]).

:- use_module(library(readutil)).
:- use_module(library(lists)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Main predicates for testing error and warning messages
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%% run_all_error_and_warning_tests(+BasePath, -Passed, -Failed)
%
%     Loads all the files for testing error and warning messages (which
%     must be under the BasePath directory) and runs all their tests
%     using run_error_test_files/6 and run_warning_test_files/5.
%
%     The total number of tests passed and failed will be returned in
%     Passed and Failed, respectively.
%
%     @see run_error_test_files/6
%     @see run_warning_test_files/5
%

run_all_error_and_warning_tests(BasePath, Passed, Failed) :-
	% Error tests
	error_test_files(ErrorFiles, ErrorLinesFiles),
	add_path_to_files(ErrorFiles, FullErrorFiles, BasePath),
	add_path_to_files(ErrorLinesFiles, FullErrorLinesFiles, BasePath),
	run_error_test_files(FullErrorFiles, FullErrorLinesFiles, 0, PassedAux, 0, FailedAux),
	% Warning tests
	warning_test_files(WarningFiles),
	add_path_to_files(WarningFiles, FullWarningFiles, BasePath),
	run_warning_test_files(FullWarningFiles, PassedAux, Passed, FailedAux, Failed).


%% run_error_test_files(+Files, +LinesFiles, +Passed, -FinalPassed, +Failed, -FinalFailed)
%
%     Loads each of the Prolog or Bousi-Prolog source code files of the
%     Files list, and verifies that these files have exactly the same
%     errors that are indicated in the files of the LinesFiles (by means
%     of the error_in_line/1 predicate).
%
%     Passed/FinalPassed and Failed/FinalFailed are two accumulator
%     pairs: FinalPassed will be unified with Passed plus the number of
%     tests passed, whereas FinalFailed will be unified with Failed plus
%     the number of tests failed.
%
%     @see compare_lines/7
%

run_error_test_files([], [], FinalPassed, FinalPassed, FinalFailed, FinalFailed).

run_error_test_files([File|MoreFiles], [LinesFile|MoreLinesFiles], Passed, FinalPassed, Failed, FinalFailed) :-
	file_base_name(File, BaseFile),
	file_base_name(LinesFile, BaseLinesFile),
	write('------------------------------------------------------------------------------'), nl,
	writef('Running error test file %w', [BaseFile]), nl,
	write('------------------------------------------------------------------------------'), nl,
	(
		% Tries to load the file with errors
		tmp_file('test', ErrorOutputFile),
		catch((
			tell(ErrorOutputFile),
			bplShell:ld(File, [f]),
			bplShell:last_program_loaded(_LoadedBaseFile, ''),
			told,
			!
		% (catcher)
		), _Error2, (
			fail
		)),
		% Loads the file that can be used to get the lines
		% where the errors are located
		catch((
			bplShell:ld(LinesFile, [f]),
			bplShell:last_program_loaded(LoadedBaseLinesFile, ''),
			file_base_name(LoadedBaseLinesFile, BaseLinesFile),
			!
		% (catcher)
		), _Error1, (
			fail
		)),
		% Gets the expected and the actual lists of lines with errors
		findall(Line, evaluator:solve_goal(bpl_call(error_in_line(Line))), ExpectedLines),
		get_line_numbers(ErrorOutputFile, File, 'ERROR', Lines),
		% Compares both lists of line numbers
		sort(ExpectedLines, SortedExpectedLines),
		sort(Lines, SortedLines),
		compare_lines(SortedExpectedLines, SortedLines, error,
		              Passed, NewPassed, Failed, NewFailed)
	;
		% One of the files couldn't be loaded
		told,
		writef('Test files %w or %w couldn\'t be loaded.', [BaseFile, BaseLinesFile]), nl,
		write('Press any key to continue '),
		get_single_char(_),
		nl,
		NewPassed is Passed,
		NewFailed is Failed + 1
	),
	!,
	% Proceeds to the next file
	run_error_test_files(MoreFiles, MoreLinesFiles,
	                     NewPassed, FinalPassed, NewFailed, FinalFailed).


%% run_warning_test_files(+Files, +Passed, -FinalPassed, +Failed, -FinalFailed)
%
%     Loads each of the Prolog or Bousi-Prolog source code files of the
%     Files list, and verifies that these files have exactly the same
%     warnings that are indicated in them (by means of the
%     warning_in_line/1 predicate).
%
%     Passed/FinalPassed and Failed/FinalFailed are two accumulator
%     pairs: FinalPassed will be unified with Passed plus the number of
%     tests passed, whereas FinalFailed will be unified with Failed plus
%     the number of tests failed.
%
%     @see compare_lines/7
%

run_warning_test_files([], FinalPassed, FinalPassed, FinalFailed, FinalFailed).

run_warning_test_files([File|MoreFiles], Passed, FinalPassed, Failed, FinalFailed) :-
	file_base_name(File, BaseFile),
	write('------------------------------------------------------------------------------'), nl,
	writef('Running warning test file %w', [BaseFile]), nl,
	write('------------------------------------------------------------------------------'), nl,
	(
		% Loads the file with warnings, which is the same file that
		% can be used to get the lines where the warnings are located
		tmp_file('test', ErrorOutputFile),
		catch((
			tell(ErrorOutputFile),
			bplShell:ld(File, [f]),
			bplShell:last_program_loaded(LoadedFile, ''),
			file_base_name(LoadedFile, BaseFile),
			told,
			!
		% (catcher)
		), _Error, (
			told,
			fail
		)),
		% Gets the expected and the actual lists of lines with warnings
		findall(Line, evaluator:solve_goal(bpl_call(warning_in_line(Line))), ExpectedLines),
		get_line_numbers(ErrorOutputFile, File, 'WARNING', Lines),
		% Compares both lists of line numbers
		sort(ExpectedLines, SortedExpectedLines),
		sort(Lines, SortedLines),
		compare_lines(SortedExpectedLines, SortedLines, warning,
		              Passed, NewPassed, Failed, NewFailed)
	;
		% File couldn't be loaded
		told,
		writef('Test file %w couldn\'t be loaded.', [BaseFile]), nl,
		write('Press any key to continue '),
		get_single_char(_),
		nl,
		NewPassed is Passed,
		NewFailed is Failed + 1
	),
	!,
	% Proceeds to the next file
	run_warning_test_files(MoreFiles, NewPassed, FinalPassed, NewFailed, FinalFailed).


%% compare_lines(+ExpectedLines, +Lines, +MessageType, +Passed, -FinalPassed, +Failed, -FinalFailed)
%
%     Compares the line numbers of the ExpectedLines with the line
%     numbers of the Lines lists. Each comparison is a test, which will
%     pass only if the line numbers are exactly the same and are located
%     in the same position in both lists. An extra test will be used to
%     check if one of the lists has more line numbers than the other one.
%
%     Passed/FinalPassed and Failed/FinalFailed are two accumulator
%     pairs: FinalPassed will be unified with Passed plus the number of
%     matched lines, whereas FinalFailed will be unified with Failed plus
%     the number of unmatched lines.
%

compare_lines([], [], MessageType,
              Passed, FinalPassed, Failed, FinalFailed) :-
	% Both error lists are empty, so the last test passed
	writef('Testing that there\'re no more %ws... OK', [MessageType]), nl,
	FinalPassed is Passed + 1,
	FinalFailed is Failed.

compare_lines([ExpectedLine|MoreExpectedLines], [], MessageType,
              Passed, FinalPassed, Failed, FinalFailed) :-
	% Error list is empty but expected error list isn't, so the last test failed
	writef('Testing %w in line %w... failed', [MessageType, ExpectedLine]), nl,
	length(MoreExpectedLines, UntestedLines),
	FinalPassed is Passed,
	FinalFailed is Failed + UntestedLines + 1,
	% Shows the results of the test
	writef('> Next %w was in line ......... (none)', [MessageType]), nl,
	writef('> Remaining expected %w lines . %w', [MessageType, MoreExpectedLines]), nl,
	writef('> Remaining %w lines .......... (none)', [MessageType]), nl,
	write('Press any key to continue '),
	get_single_char(_),
	nl.

compare_lines([], [Line|MoreLines], MessageType,
              Passed, FinalPassed, Failed, FinalFailed) :-
	% Expected error list is empty but error list isn't, so the last test failed
	writef('Testing that there\'re no more %ws... failed', [MessageType]), nl,
	FinalPassed is Passed,
	FinalFailed is Failed + 1,
	% Shows the results of the test
	writef('> Next %w was in line ......... %w', [MessageType, Line]), nl,
	writef('> Remaining expected %w lines . (none)', [MessageType]), nl,
	writef('> Remaining %w lines .......... %w', [MessageType, MoreLines]), nl,
	write('Press any key to continue '),
	get_single_char(_),
	nl.

compare_lines([ExpectedLine|MoreExpectedLines], [Line|MoreLines], MessageType,
	          Passed, FinalPassed, Failed, FinalFailed) :-
	writef('Testing %w in line %w... ', [MessageType, ExpectedLine]),
	(
		ExpectedLine == Line,
		% Test passed
		write('OK'), nl,
		NewPassed is Passed + 1,
		NewFailed is Failed,
		% Compares the remaining lines
		compare_lines(MoreExpectedLines, MoreLines, MessageType,
			          NewPassed, FinalPassed, NewFailed, FinalFailed)
	;
		% Test failed
		write('failed'), nl,
% 		length(MoreExpectedLines, UntestedLines), % WARNING: What are these three lines for?
% 		NewPassed is Passed, 
% 		NewFailed is Failed + UntestedLines + 1,
		% Shows the results of the test
		writef('> Next %w was in line ......... %w', [MessageType, Line]), nl,
		writef('> Remaining expected %w lines . %w', [MessageType, MoreExpectedLines]), nl,
		writef('> Remaining %w lines .......... %w', [MessageType, MoreLines]), nl,
		write('Press any key to continue '),
		get_single_char(_),
		nl,
		% When an error is found the comparison is stopped
		FinalPassed is Passed,
		FinalFailed is Failed
	).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Helper predicates
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%% get_line_numbers(+File, +SourceFile, +MessageType, -Lines)
%
%     Reads the specified File and looks for lines which start with
%     the following substring: "<MessageType>: <SourceFile>:<Line>".
%     Lines will be unified with a list containing all the Line numbers
%     from these messages.
%

get_line_numbers(File, SourceFile, MessageType, Lines) :-
	open(File, read, Stream),
	search_messages_in_stream(Stream, SourceFile, MessageType, Lines),
	close(Stream).


%% search_messages_in_stream(+Stream, +SourceFile, +MessageType, -Lines)
%
%     Scans all the lines from the specified Stream and looks for lines
%     which start with the following substring: "<MessageType>:
%     <SourceFile>:<Line>". Lines will be unified with a list containing
%     all the Line numbers from these messages.
%

search_messages_in_stream(Stream, _SourceFile, _MessageType, []) :-
	% Checks if EOF has been reached
	at_end_of_stream(Stream), !.

search_messages_in_stream(Stream, SourceFile, MessageType, Lines) :-
	% Gets the next line from the stream
	read_line_to_codes(Stream, Codes),
	atom_codes(String, Codes),
	% Checks if the line contains an error or warning message; these
	% messages must be like "ERROR: path/to/file:line:column: message text"
	concat_atom([MessageType, ': ', SourceFile, ':'], Substring),
	(sub_atom(String, 0, MessageIdx, _, Substring) ->
		% Extracts the line number from the error or warning message
		sub_atom(String, MessageIdx, NumberLen, _, LineNumberAsAtom),
		NumberIdx is MessageIdx + NumberLen,
		sub_atom(String, NumberIdx, 1, _, ':'),
		!,
		% Adds the line number to the list that will be returned
		atom_number(LineNumberAsAtom, LineNumber),
		Lines = [LineNumber|MoreLines]
	;
		% No error or warning message was found in this line
		Lines = MoreLines
	),
	% Goes on scanning the stream
	search_messages_in_stream(Stream, SourceFile, MessageType, MoreLines).


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
% Constant predicates
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%% error_test_files(?Files, ?LinesFiles)
%
%     Returns the list of Prolog or Bousi-Prolog source code files that
%     are used by this module to generate error messages, and the list
%     of files that can be used to get the lines where the errors are
%     located.
%

error_test_files(['errors_directives.bpl'], ['errors_directives_lines.bpl']).
% error_test_files(['errors_directives.bpl', 'errors_other.bpl',
%                   'errors_comments.bpl', 'errors_bpl.bpl'],
%                  ['errors_directives_lines.bpl', 'errors_other_lines.bpl',
%                   'errors_comments_lines.bpl', 'errors_bpl_lines.bpl']).


%% warning_test_files(?Files)
%
%     Returns the list of Prolog or Bousi-Prolog source code files that
%     are used by this module to generate warning messages. As files
%     with warnings can be loaded into the database, these files also
%     contain the lines where the warnings are located.
%

warning_test_files(['errors_warnings.bpl']).

