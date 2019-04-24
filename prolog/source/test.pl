%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Bousi-Prolog system test launcher

:- module(test, [
		main_test/0             %
   ]).

:- use_module('bousi').
:- use_module('foreign').
:- use_module('flags').
:- use_module('evaluator').

:- use_module(test_prolog).
:- use_module(test_bousiprolog).
:- use_module(test_shell).
:- use_module(test_errors).

:- use_module(library(test_cover)).
:- use_module(library(lists)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Main predicate for launching the Bousi-Prolog system tests
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%% main_test
%
%     Initial predicate of the Bousi-Prolog system test launcher. This
%     predicate initializes the Bousi-Prolog system and runs all the
%     tests designed for it. After all the tests have been completed, a
%     report with the number of tests passed and the code coverage of
%     each module is displayed.
%

main_test :-
%	test:run_tests.
	show_coverage(test:run_tests).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Predicate used to run all the tests
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%% run_tests
%
%     Initializes the Bousi-Prolog system, runs all the tests designed
%     for it and then displays a report with the total number of test
%     passed.
%

run_tests :-
	% Initializes the Bousi-Prolog system
	foreign:load_foreign_extension,
%	bplShell:load_history,
	bplShell:set_system_predicates,
	flags:reset_bpl_flags,
	evaluator:load_tpl(''),
	% Gets the full path where the test files are located
	working_directory(WorkingDir, WorkingDir),
	(concat_atom([_, '/'], WorkingDir) ->
		concat_atom([WorkingDir, 'test'], BasePath)
	;
		concat_atom([WorkingDir, '/', 'test'], BasePath)
	),
	% Remove compiled .tpl files
	concat_atom([BasePath, '/', '*.tpl'], WildCard),
	expand_file_name(WildCard, FilesToDelete),
	maplist(rm, FilesToDelete),
	% Runs all the tests from the four categories
	run_all_shell_tests(ShellPassed, ShellFailed),
	run_all_bousiprolog_tests(BasePath, BousiPassed, BousiFailed),
	run_all_error_and_warning_tests(BasePath, ErrorPassed, ErrorFailed),
 	run_all_prolog_tests(BasePath, PrologPassed, PrologFailed),
 	TotalPassed is ShellPassed+BousiPassed+ErrorPassed+PrologPassed,
 	TotalFailed is ShellFailed+BousiFailed+ErrorFailed+PrologFailed,
	% Writes the test report
	nl,
	write('================================'), nl,
	write('          Test Report           '), nl,
	write('================================'), nl,
	write('Test suites      Passed   Failed'), nl,
	write('================================'), nl,
	writef('Shell             %4r     %4r', [ShellPassed, ShellFailed]), nl,
	writef('Bousi-Prolog      %4r     %4r', [BousiPassed, BousiFailed]), nl,
	writef('Error/Warnings    %4r     %4r', [ErrorPassed, ErrorFailed]), nl,
 	writef('Prolog            %4r     %4r', [PrologPassed, PrologFailed]), nl,
	write('================================'), nl,
	writef('Total             %4r     %4r', [TotalPassed, TotalFailed]), nl,
	true.

	