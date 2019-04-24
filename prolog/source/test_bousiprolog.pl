%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Test module for Bousi-Prolog predicates

:- module(test_bousiprolog, [
		run_all_bousiprolog_tests/3 % +BasePath, -Passed, -Failed
	]).

:- use_module(library(lists)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Main predicates for testing Prolog predicates
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%% run_all_bousiprolog_tests(+BasePath, -Passed, -Failed)
%
%     Loads all the files for testing Bousi-Prolog predicates (which
%     must be under the BasePath directory) and runs all their tests
%     using run_bousiprolog_test_files/5.
%
%     The total number of tests passed and failed will be returned in
%     Passed and Failed, respectively.
%
%     @see run_bousiprolog_test_files/5
%

run_all_bousiprolog_tests(BasePath, Passed, Failed) :-
	bousiprolog_test_files(Files),
	add_path_to_files(Files, FullFiles, BasePath),
	run_bousiprolog_test_files(FullFiles, 0, Passed, 0, Failed).


%% run_bousiprolog_test_files(+Files, +Passed, -FinalPassed, +Failed, -FinalFailed)
%
%     Loads each of the Bousi-Prolog source code files of the Files list,
%     and runs all their test predicates using run_bousiprolog_tests/5.
%
%     Passed/FinalPassed and Failed/FinalFailed are two accumulator
%     pairs: FinalPassed will be unified with Passed plus the number of
%     tests passed, whereas FinalFailed will be unified with Failed plus
%     the number of tests failed.
%
%     @see run_bousiprolog_tests/5
%

run_bousiprolog_test_files([], Passed, Passed, Failed, Failed).

run_bousiprolog_test_files([File|MoreFiles], Passed, FinalPassed, Failed, FinalFailed) :-
	file_base_name(File, BaseFile),
	write('------------------------------------------------------------------------------'), nl,
	writef('Running Bousi-Prolog test file %w', [BaseFile]), nl,
	write('------------------------------------------------------------------------------'), nl,
	(
		% Loads the Bousi-Prolog file (each Bousi-Prolog file
		% contains a single test suite)
		catch((
			bplShell:ld(File, [f]),
			bplShell:last_program_loaded(LoadedFile, ''),
			file_base_name(LoadedFile, BaseFile),
			!
		% (catcher)
		), _Error1, (
			fail
		)),
		% Gets the name of the predicates that belong to the test
		% suite of the loaded Bousi-Prolog file
		evaluator:solve_goal(bpl_call(test_suite(Tests))),
		% Runs the tests of this file
		run_bousiprolog_tests(Tests, Passed, NewPassed, Failed, NewFailed),
		% Gets the additional queries that must be solved
		catch((
			(
				evaluator:solve_goal(bpl_call(additional_queries(AdQueries)))
			;
				AdQueries = []
			)
		% (catcher)
		), _Error2, (
			AdQueries = []
		)),
		(AdQueries \== [] ->
			write('Testing additional queries... '),
			(
				% Runs all the additional queries
				catch((
					forall(member(AdQuery, AdQueries), (
						translator:translate_query(AdQuery, AdExQuery, _Bindings, _Degree),
						evaluator:solve_goal(AdExQuery)
					))
				), _Error3, (
					fail
				)),
				!,
				% Additional queries passed
				write('OK'), nl
			;
				% Additional queries failed
				write('failed'), nl,
				write('Press any key to continue '),
				get_single_char(_),
				nl
			)
		;
			% No additional queries found
			true
		)
	;
		% Bousi-Prolog file couldn't be loaded
		writef('Test file %w couldn\'t be loaded.', [BaseFile]), nl,
		write('Press any key to continue '),
		get_single_char(_),
		nl,
		NewPassed is Passed,
		NewFailed is Failed + 1
	),
	!,
	% Proceeds to the next file
	run_bousiprolog_test_files(MoreFiles, NewPassed, FinalPassed, NewFailed, FinalFailed).


%% run_bousiprolog_tests(+Tests, +Passed, -FinalPassed, +Failed, -FinalFailed)
%
%     Runs each of the predicates of the Tests list under Bousi-Prolog
%     and checks the approximation degree of the result. A test will pass
%     only if it succeeds and its approximation degree is exactly the same
%     as the expected one for that test.
%
%     Passed/FinalPassed and Failed/FinalFailed are two accumulator
%     pairs: FinalPassed will be unified with Passed plus the number of
%     tests passed, whereas FinalFailed will be unified with Failed plus
%     the number of tests failed.
%
%     @see execute_test/2
%

run_bousiprolog_tests([], Passed, Passed, Failed, Failed).

run_bousiprolog_tests([Test|MoreTests], Passed, FinalPassed, Failed, FinalFailed) :-
	writef('Testing %w... ', [Test]),
	% Translates the test name into an executable query
	translator:translate_query(Test, Query, _Bindings, Degree),
	% Executes the test under Bousi-Prolog
	execute_test(Query, Degree),
	% Gets the expected approximation degree of the solution
	% (if no expected degree is specified, 1 is assummed)
	catch((
		(
			evaluator:solve_goal(bpl_call(approximation_degree(Test, ExpectedDegree)))
		;
			ExpectedDegree is 1
		)
	% (catcher)
	), _Error, (
		ExpectedDegree is 1
	)),
	(
		% Compares the approximation degree of the solution with the
		% expected approximation degree (if the latter is a free variable,
		% any approximation degree less than 1 will be considered OK)
		(
			var(ExpectedDegree), Degree < 1
		;
			nonvar(ExpectedDegree), Degree =:= ExpectedDegree
		),
		% Test passed
		writef('OK with %w', [Degree]), nl,
		NewPassed is Passed + 1,
		NewFailed is Failed
	;
		% Test failed
		write('failed'), nl,
		NewPassed is Passed,
		NewFailed is Failed + 1,
		% Shows the results of the test
		writef('> Approximation degree . %w', [Degree]), nl,
		(var(ExpectedDegree) ->
			write('> Expected degree ...... < 1'), nl
		;
			writef('> Expected degree ...... %w', [ExpectedDegree]), nl
		)
		,
		write('Press any key to continue '),
		get_single_char(_),
		nl
	),
	!,
	% Proceeds to the next test
	run_bousiprolog_tests(MoreTests, NewPassed, FinalPassed, NewFailed, FinalFailed).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Helper predicates
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%% execute_test(+Goal, -Degree)
%
%     Executes Goal under the evaluator module. Degree must be the
%     variable where the approximation degree of the result will be
%     stored after executing Goal. If Goal fails or throws an unhandled
%     exception Degree will be unified with 0.
%

execute_test(Goal, Degree) :-
	catch((
		(evaluator:solve_goal(Goal) ->
			true
		;
			Degree = 0
		)
	% (catcher)
	), _Error, (
		Degree = 0
	)).


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


%% bousiprolog_test_files(?Files)
%
%     Returns the list of Bousi-Prolog source code files that contain
%     the test suites used by this module.
%

bousiprolog_test_files(['bpl_relations_1.bpl', 'bpl_relations_2.bpl',
                        'bpl_relations_3.bpl', 'bpl_fuzzysets.bpl',
                        'bpl_lambdacut.bpl', 'bpl_tnorms.bpl',
                        'bpl_wsld.bpl', 'bpl_unif_algorithm_a1.bpl',
                        'bpl_unif_algorithm_a2.bpl', 'bpl_unif_algorithm_a3.bpl']).

