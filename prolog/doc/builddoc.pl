%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Bousi-Prolog documentation generator

:- module(builddoc, [
		create_documentation/1  % +LatexFile
   ]).

:- use_module(library(doc_latex)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Main predicate for building Bousi-Prolog documentation
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%% create_documentation(+LatexFile)
%
%     Initial predicate of the Bousi-Prolog documentation generator.
%     This predicate loads both the Bousi-Prolog system and the test
%     launcher into memory and writes the documentation of all their
%     modules (including this one) to a LaTeX file called LatexFile.
%

create_documentation(LatexFile) :-
	consult('../source/bousi'),
	consult('../test/test'),
	source_files(Filenames),
	doc_latex(Filenames, LatexFile, [stand_alone(true), public_only(false)]).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Constant predicates
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%% source_files(?Filenames)
%
%     Returns a list with the paths of the Prolog source code files
%     that make up the Bousi-Prolog system, the documentation generator
%     and the test launcher.
%

source_files([
              % Bousi-Prolog system
              '../source/bousi', '../source/bplHelp.pl',
              '../source/directives.pl', '../source/evaluator.pl',
              '../source/flags.pl', '../source/foreign.pl',
              '../source/parser.pl', '../source/translator.pl',
              '../source/utilities.pl',
              % Documentation generator
              'builddoc.pl',
              % Test launcher
              '../test/test.pl', '../test/test_prolog.pl',
              '../test/test_bousiprolog.pl', '../test/test_errors.pl',
              '../test/test_shell.pl'
             ]).

