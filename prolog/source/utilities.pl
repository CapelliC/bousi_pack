%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Utility predicates used by other modules

:- module(utilities, [
		% Predicates for processing and scanning terms
		process_term/6,         % +Term, -Result, +Scanners, +Testers,
		                        %   +InData, -OutData
		% Predicates for retrieving predicate names
		get_predicates/1,       % -PredicateList
		get_predicates_modules/2,% +ModuleList, -PredicateList
		% Predicates for simplifying a filename or an atom
		simplify_filename/2,    % +Path, -SimplifiedFilename
		simplify_atom/2,        % +Atom, -SimplifiedAtom)
		% Miscellaneous list-related predicates
		write_lines/3,          % +List, +Prefix, +Sufix
		extract_terms/4,        % +Prefix, +Arity, +List, -Items
		ascending_numbers/1,    % +List
		remove_prefixes/3,      % +List, -Result, +Prefix
		remove_program_prefix/2,% +Atom, -Result
		% Miscellaneous string-related predicates
		remove_quotes/2,        % +Strings, ?FixedStrings
		is_quoted/2,            % +String, ?QuoteChar
		% Miscellaneous predicates for interacting with files and the OS
		home_directory/1,       % ?HomeDir
		file_is_newer/2,        % +File1, +File2
		% Other miscellaneous predicates
		builtin/1,              % +Predicate
		atom_is_variable/1,     % +Atom
		% Bousi-Prolog specific predicates
		closure_properties/3,   % +Properties, ?Closure, ?TNorm
		relation_name/2,        % ?Symbol, ?Name
		relation_evaluator/2    % ?Relation, ?Evaluator
%		atoms_in_term/2         % +Term, -Atoms
   ]).

:- use_module(library(lists)).
:- use_module(library(readutil)).
:- use_module(library(shell)).
% Add all modules used by wn module. 
:- use_module(library(ordsets)).
:- use_module(library(lists)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- set_prolog_flag(double_quotes, codes).

checkwnenv(WNDB) :-
    (   getenv('WNDB', WNDB)
    ->  true
    ;  (current_prolog_flag(windows, true)
    %   Default directories:
    ->  WNDB = 'C:\\WordNet3.0'
    ;   WNDB = '/usr/local/WordNet-3.0'),
        setenv('WNDB', WNDB)
    ).
    
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Predicates for processing and scanning terms
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%% process_term(+Term, -Result, +Scanners, +Testers, +InData, -OutData)
%
%     Generic higher-order predicate that can be used to scan and/or
%     process a Term with some InData in order to get a Result term and
%     some OutData. This predicate uses two different lists of
%     predicates (which can be empty):
%
%      * Scanners: predicates with signature "(+Term, -Result, +InData,
%        -OutData)" that must not fail.
%      * Testers: unary predicates with signature "(+Term)" that may
%        optionally fail.
%
%     The behavior of the process_term/6 predicate is defined by means
%     of the following algorithm:
%
%      * 1: Call the Scanners with the initial Term and InData to get
%           a Result and some OutData.
%      * 2: If Result is not a compound term, return Result and
%           OutData.
%      * 3: If Result is a compound term:
%        * 3.1: Call the Testers with the Result.
%        * 3.2: If any of the Testers fail, return Result and OutData.
%        * 3.3: If all the Testers succeed:
%          * 3.3.1: Call this predicate recursively with each of the
%                   arguments of Result. OutData will be passed as
%                   InData on the first call to get a new OutData, then
%                   this OutData will be passed as InData on the second
%                   call and so on.
%          * 3.3.2: Return a new term with the Result functor and
%                   the arguments returned by the recursive calls, and
%                   also the OutData returned by the last recursive
%                   call.
%

%% process_term_aux(+Terms, -Results, +Scanners, +Testers, +InData, -OutData)
%
%     Internal predicate used by process_term/6 in which Terms and
%     Results are not single terms but lists.
%
%     @see process_term/4
%

process_term(Term, Result, Scanners, Testers, InData, OutData) :-
	process_term_aux([Term], [Result], Scanners, Testers, InData, OutData).

process_term_aux([], [], _Scanners, _Testers, OutData, OutData).

process_term_aux([Term|MoreTerms], [Result|MoreResults], Scanners, Testers, InData, OutData) :-
	execute_scanners(Term, FirstResult, Scanners, InData, FirstOutData),
	((compound(FirstResult), execute_testers(FirstResult, Testers)) ->
		FirstResult =.. [Functor|Args],
		process_term_aux(Args, ResultArgs, Scanners, Testers, FirstOutData, LastOutData),
		Result =.. [Functor|ResultArgs]
	;
		Result = FirstResult,
		LastOutData = FirstOutData
	),
	process_term_aux(MoreTerms, MoreResults, Scanners, Testers, LastOutData, OutData).


%% execute_testers(+Term, +Testers)
%
%     Applies Term to each of the predicates in the Testers list.
%

execute_testers(_Term, []).

execute_testers(Term, [Tester|MoreTesters]) :-
	apply(Tester, [Term]),
	execute_testers(Term, MoreTesters).


%% execute_scanners(+Term, -Result, +Scanners, +InData, -OutData)
%
%     Applies Term and InData to the first predicate of the Scanners
%     list in order to get a temporary Result and OutData. Then, these
%     Result and OutData are passed as Term and InData to the second
%     predicate of the Scanners list, and so on. In the last call, the
%     resulting values are unified with Result and OutData.
%

execute_scanners(Term, Term, [], OutData, OutData).

execute_scanners(Term, Result, [Scanner|MoreScanners], InData, OutData) :-
	apply(Scanner, [Term, NextTerm, InData, NextInData]),
	execute_scanners(NextTerm, Result, MoreScanners, NextInData, OutData).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Predicates for retrieving predicate names
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%% get_predicates(-PredicateList)                                is det
%
%     Unifies PredicateList with the list of the names of all the
%     predefined predicates in SWI-Prolog.
%

get_predicates(Predicates) :-
	% Gets the list of SWI-Prolog modules
	setof(Mod, current_module(Mod), Modules),
	subtract(Modules, [bousi, bplHelp, bplShell, directivesBpl, evaluator,
	                   flags, foreign, parser, translator, utilities],
	         PrologModules),
	% Retrieves the full list of predicate names
	get_predicates_modules(PrologModules, UnsortedPredicates),
	% Sorts predicate names and removes duplicates
	sort(UnsortedPredicates, Predicates).


%% get_predicates_modules(+ModuleList, -PredicateList)
%
%     Unifies PredicateList with the list of the names of the
%     predicates that are available in each of the modules of the
%     ModuleList.
%

get_predicates_modules([], []).

get_predicates_modules([Module|MoreModules], Predicates) :-
	setof(Pred, Arity ^ current_predicate(Module:Pred/Arity), ModulePreds), !,
	get_predicates_modules(MoreModules, MorePredicates),
	append(ModulePreds, MorePredicates, Predicates).

get_predicates_modules([_Module|MoreModules], MorePredicates) :-
	% This rule is executed only if the predicates of Module can't
	% be retrieved; in that case the module is ignored
	get_predicates_modules(MoreModules, MorePredicates).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Predicates for simplifying a filename or an atom
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%% simplify_filename(+Path, -SimplifiedFilename)
%
%     Extracts the filename part of Path, applies the simplify_atom/2
%     to it and unifies the resulting filename with SimplifiedFilename.
%
%     @see simplify_atom/2
%

simplify_filename(Path, SimplifiedFilename) :-
	file_base_name(Path, BaseFileWithExt),
	file_name_extension(BaseFile, _Extension, BaseFileWithExt),
	simplify_atom(BaseFile, SimplifiedFilename).


%% simplify_atom(+Atom, -SimplifiedAtom)
%
%     Replaces all the non-alphanumeric characters of Atom with
%     underscores, modifies its first character if it's not a lowercase
%     letter and returns the resulting atom in SimplifiedAtom.
%

simplify_atom(Atom, SimplifiedAtom) :-
	atom_chars(Atom, OriginalChars),
	simplify_chars(OriginalChars, SimplifiedChars),
	atom_chars(SimplifiedAtom, SimplifiedChars).


%% simplify_chars(+Chars, -SimplifiedChars)
%
%     Replaces all the non-alphanumeric characters of the Chars list
%     with underscores, changes the first character if it's not a
%     lowercase letter and returns the resulting character list in
%     SimplifiedChars.
%

%% simplify_chars_aux(+Chars, -SimplifiedChars, +FirstChar)
%
%     Internal predicate used by simplify_chars/2 which includes an
%     extra argument that indicates whether the next char in the Chars
%     list is going to be the first character of an atom.
%
%     @see simplify_chars/2
%

simplify_chars(OriginalChars, SimplifiedChars) :-
	simplify_chars_aux(OriginalChars, SimplifiedChars, yes).

simplify_chars_aux([], [], _First).

simplify_chars_aux([Char|MoreChars], [Char|MoreSimplifiedChars], _First) :-
	% Lowercase letters are always copied to the destination list
	char_type(Char, lower), !,
	simplify_chars_aux(MoreChars, MoreSimplifiedChars, no).

simplify_chars_aux([UpperChar|MoreChars], [LowerChar|MoreSimplifiedChars], yes) :-
	% Uppercase letters are replaced with their lowercase
	% counterparts if they're the first character of an atom
	char_type(UpperChar, upper(LowerChar)), !,
	simplify_chars_aux(MoreChars, MoreSimplifiedChars, no).

simplify_chars_aux([_Char|MoreChars], ['a'|MoreSimplifiedChars], yes) :-
	% Any character that isn't a letter is replaced with a lowercase letter
	% (in this case, 'a') when they're the first character of an atom
	simplify_chars_aux(MoreChars, MoreSimplifiedChars, no).

simplify_chars_aux([Char|MoreChars], [Char|MoreSimplifiedChars], no) :-
	% Uppercase letters and digits are allowed only if
	% they're not the first character of an atom
	(char_type(Char, upper) ; char_type(Char, digit)), !,
	simplify_chars_aux(MoreChars, MoreSimplifiedChars, no).

simplify_chars_aux([_Char|MoreChars], ['_'|MoreSimplifiedChars], no) :-
	% Any non-alphanumeric character is replaced with an underscore
	simplify_chars_aux(MoreChars, MoreSimplifiedChars, no).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Miscellaneous list-related predicates
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% write_lines(+List, +Prefix, +Sufix)
%
%     Writes a list of terms in current output stream. Prefix and Sufix
%     strings will be added before and after every term, respectively.
%

write_lines([], _Prefix, _Sufix).

write_lines([Item], Prefix, Sufix) :-
	write(Prefix), write(Item), write(Sufix).

write_lines([Item|List], Prefix, Sufix) :-
	List \== [],
	write_lines([Item], Prefix, Sufix), nl,
	write_lines(List, Prefix, Sufix).


%% extract_terms(+Prefix, +Arity, +List, -Items)
%
%     Extracts all the compound terms from List that match template
%     "Prefix/Arity" and returns them in the Items list.
%
%     For example, given Prefix = 'sim' and Arity = 3, this predicate
%     will only return "sim(_, _, _)" terms.
%
%     @compat iso
%

extract_terms(Prefix, Arity, List, Items) :-
	atom(Prefix), integer(Arity),
	functor(Template, Prefix, Arity),
	findall(Template, member(Template, List), Items).


%% ascending_numbers(+List)                                      is det
%
%      Succeeds only if all the items of List are integer numbers and
%      they're in ascending order too.
%
%      @compat iso
%

%% ascending_numbers(+List, +Highest)
%
%      Internal predicate used by ascending_numbers/1 that includes
%      the highest number found.
%
%      @compat iso
%      @see ascending_numbers/1
%

ascending_numbers([]).

ascending_numbers([First|Values]) :-
	ascending_numbers(Values, First).

ascending_numbers([], _Highest).

ascending_numbers([Value|MoreValues], Highest) :-
	integer(Value),
	Value >= Highest,
	ascending_numbers(MoreValues, Value).


%% remove_prefixes(+List, -Result, +Prefix)
%
%     Extracts all the atoms of List that begin with Prefix sub-atom,
%     removes Prefix from all those atoms and returns them in Result.
%
%     For example, given List = ['atom1', 'other', 'atom_ex'] and
%     Prefix = 'atom', this predicate will return Result = ['1', '_ex'].
%
%     @compat iso
%

remove_prefixes([], [], _Prefix).

remove_prefixes([Atom|MoreAtoms], [AtomNoPrefix|MoreAtomsNoPrefix], Prefix) :-
    sub_atom(Atom, 0, Len, _, Prefix), !,
	% Atom begins with Prefix
	sub_atom(Atom, Len, _, 0, AtomNoPrefix),
	remove_prefixes(MoreAtoms, MoreAtomsNoPrefix, Prefix).

remove_prefixes([_Atom|MoreAtoms], MoreAtomsNoPrefix, Prefix) :-
	% Atom doesn't begin with Prefix
	remove_prefixes(MoreAtoms, MoreAtomsNoPrefix, Prefix).

	
%% remove_program_prefix(+Atom, -Result)
%
%     Removes the current program prefix from Atom and returns in Result.
%     If Atom does not include the program prefix, just returns Atom.
%

remove_program_prefix(Atom, Result) :-
  parser:program_prefix(Prefix),
  atom_concat(Prefix, '_', PrefixUS),
  atom_concat(PrefixUS, Result, Atom),
  !.
  
remove_program_prefix(Atom, Atom).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Miscellaneous string-related predicates
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%% remove_quotes(+String, ?FixedString)
%
%     Removes the initial and final quote characters of String and
%     returns the result in FixedString. If String is a list, this
%     predicate will copy all their items to FixedString, removing
%     the initial and final quote characters of every quoted string.
%

remove_quotes(String, FixedString) :-
	atom(String),
	is_quoted(String, '\''),
	sub_atom(String, 1, _, 1, FixedString), !.

remove_quotes(String, FixedString) :-
	atom(String),
	is_quoted(String, '\"'),
	sub_atom(String, 1, _, 1, FixedString), !.

remove_quotes(String, String) :-
	atom(String).

remove_quotes([], []).

remove_quotes([String|MoreStrings], [FixedString|MoreFixedStrings]) :-
	remove_quotes(String, FixedString),
	remove_quotes(MoreStrings, MoreFixedStrings).


%% is_quoted(+String, ?QuoteChar)
%
%     Succeeds if String starts and ends with QuoteChar character.
%

is_quoted(String, QuoteChar) :-
	sub_atom(String, 0, 1, _, QuoteChar),
	sub_atom(String, _, 1, 0, QuoteChar).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Miscellaneous predicates for interacting with files and the OS
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%% home_directory(?HomeDir)
%
%     Unifies HomeDir with the path to the user's home directory, which
%     is taken from $HOME environment variable on Unix/Linux or from
%     %HOMEDRIVE% and %HOMEPATH% environment variables on Windows.
%

home_directory(HomeDir) :-
	% Windows home folder
	current_prolog_flag(windows, true), !,
	getenv('HOMEDRIVE', HomeDrive), getenv('HOMEPATH', HomePath),
	concat_atom([HomeDrive, HomePath], HomeDir).

home_directory(HomeDir) :-
	% Unix/Linux home folder
	getenv('HOME', HomeDir).


%% file_is_newer(+File1, +File2)
%
%     Succeeds only if File1 is newer than File2, i.e., File1 has been
%     modified after File2.
%

file_is_newer(File1, File2) :-
	time_file(File1, ModTime1),
	time_file(File2, ModTime2),
	ModTime1 >= ModTime2.



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Other miscellaneous predicates
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%% builtin(+Predicate)                                       is semidet
%
%     Succeeds only if Predicate is the head of a SWI-Prolog predefined
%     predicate or a predicate declared in any of the currently loaded
%     modules.
%

builtin(Predicate) :-
	functor(Predicate, Functor, Arity),
	not(number(Functor)),
	current_module(Module),
	not(member(Module, [test_prolog])), % Needed for running the tests
	current_predicate(Module:Functor/Arity), !.


%% atom_is_variable(+Atom)                                   is semidet
%
%     Succeeds if Atom is an atomic term which starts with an uppercase
%     letter or an underscore character (_).
%

atom_is_variable(Atom) :-
	atomic(Atom),
	atom_chars(Atom, [FirstChar|_]),
	(FirstChar == '_', ! ; char_type(FirstChar, upper)).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Bousi-Prolog specific predicates
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%% closure_properties(+Properties, ?Closure, ?TNorm)
%
%     Scans a list with the closure Properties of a fuzzy relation and
%     returns its Closure and TNorm numeric values, which can then be
%     used to invoke the ext_closure/5 foreign predicate.
%
%     Valid fuzzy relation properties are 'symmetric', 'reflexive' and
%     'transitive(TNorm)', where TNorm can be 'yes', 'no', 'min',
%     'product' or 'luka'.
%

closure_properties(Properties, Closure, TNorm) :-
	is_list(Properties), !,
	% Extracts closure properties and t-norm name from list; fuzzy
	% relation properties are specified by a number which is a
	% combination of one or more of these flags:
	%  1 - Reflexive
	%  2 - Symmetric
	%  4 - Transitive
	% These are three common fuzzy binary relations:
	%  3 - Proximity relation (reflexive and symmetric)
	%  5 - Partial order (reflexive and transitive)
	%  7 - Similarity relation (reflexive, symmetric and transitive)
	(member(transitive(TNormName), Properties) ->
		(TNormName \== no ->
			NTransitive is 0b100
		;
			NTransitive is 0
		)			
	;
		(member(transitive, Properties) ->
			NTransitive is 0b100
		;
			NTransitive is 0
		)
	),
	(member(symmetric, Properties) ->
		NSymmetric is 0b010
	;
		NSymmetric is 0
	),
	(member(reflexive, Properties) ->
		NReflexive is 0b001
	;
		NReflexive is 0
	),
	Closure is NTransitive + NSymmetric + NReflexive,
	% Sets default t-norm if it doesn't appear in properties list
	(var(TNormName) ->
		TNormName = yes
	;
		true
	),
	% Gets t-norm identifier
	(TNormName == product ->
		TNorm is 2
	;
	(TNormName == luka ->
		TNorm is 3
	;
	% TNormName == yes / no / min
		TNorm is 1
	)).


%% relation_name(?Symbol, ?Name)
%
%     Succeeds if Name is the internal name of the relation defined by
%     Symbol in BPL files.
%

relation_name('~', sim).
relation_name('<~', lEqThan).
relation_name('~>', gEqThan).
relation_name('~1~', frel1).
relation_name('~2~', frel2).
relation_name('~3~', frel3).


%% relation_evaluator(?Relation, ?Evaluator)
%
%     Succeeds if Evaluator is the name of the predicate that is used
%     internally to compare two terms using a certain Relation.
%

relation_evaluator(lEqThan, e_lEqThan).
relation_evaluator(gEqThan, e_gEqThan).
relation_evaluator(frel1, e_frel1).
relation_evaluator(frel2, e_frel2).
relation_evaluator(frel3, e_frel3).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% append_goals(+Goals1,+Goals2,-Goals) Appends the two input
%   goals, returning a concatenated goal and excluding
%   true goals
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

append_goals(true, (A,B), C) :-
  !,
  append_goals(A,B,C).
append_goals((A,B), true, C) :-
  !,
  append_goals(A,B,C).
append_goals(true, true, true) :-
  !.
append_goals(true, A, A) :-
  !.
append_goals(A,true, A) :-
  !.
append_goals((A,B), C, E) :-
  !, 
  append_goals(B, C, D),
  append_goals(A, D, E).
append_goals(A, (B,C), (A,D)) :-
  !,
  append_goals(B, C, D).
append_goals(A, B, (A,B)).

append_goals_list([A],A).
append_goals_list([A,B|Gs],G) :-
  append_goals(A,B,C),
  append_goals_list([C|Gs],G).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% atoms_in_term(+Term, -Functors) Returns all the atoms
%   in Term
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% atoms_in_term(Term, Functors) :-
%    atoms_in_term(Term, Functors, []).


% atoms_in_term(Var) -->
%   {
%     var(Var),
%     !
%   },
%   [].

% atoms_in_term(Number) -->
%   {
%     number(Number),
%     !
%   },
%   [].

% atoms_in_term(Atom) -->
%   {
%     atom(Atom),
%     !
%   },
%   [Atom].

% atoms_in_term([]) -->
%   !,
%   [].


% atoms_in_term(Term) --> 
%   {
%     Term =.. [_Functor|Terms]
%   },
%   atoms_in_term_list(Terms).

% atoms_in_term_list([]) -->
%   [].
% atoms_in_term_list([Term|Terms]) -->
%   atoms_in_term(Term),
%   atoms_in_term_list(Terms).
  

