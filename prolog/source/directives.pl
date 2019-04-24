%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Bousi-Prolog directive evaluator

:- module(directives, [
		directive/3,            % +Name, +Arguments, +Program, -Equations
		directive/2,            % +Name, +Arguments
		is_directive_valid/2    % +Name, +Arguments
   ]).

:- use_module(flags).
:- use_module(utilities).
%:- ensure_loaded(wn(wn_gen_prox_equations)).
%:- ensure_loaded(wn(wn_utilities)).

:- use_module(library(lists)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- set_prolog_flag(double_quotes, codes).

ensure_loaded_wn :-
  ensure_loaded(wn(wn_gen_prox_equations)),
  ensure_loaded(wn(wn_utilities)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Predicate for executing a BPL directive
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%% directive(+Name, +Arguments)
%
%     Evaluates and executes (at run-time) a Bousi-Prolog directive 
%     with the given Name and list of Arguments. Basically, this 
%     predicate will alter the BPL flags defined in the 'flags' module.
%

directive(Name, Arguments) :-
  directive(Name, Arguments, _).


%% directive(+Name, +Arguments, -Equations)
%
%     This call is performed at compile-time. Its third argument
%     contains the new equations that will replace the directive
%     in the compiled program

directive(lambda_cut, [Lambda], []) :-
	% Sets the lambda-cut value, i.e., the lower bound allowed
	% for the approximation degree of weak unifications
	flags:set_bpl_flag(lambda_cut(Lambda)).

directive(filtering, [Boolean], []) :-
	% Sets the filtering behavior, i.e., if Boolean=true, then
  % apply filtering; otherwise, do not.
	flags:set_bpl_flag(filtering(Boolean)).

directive(transitivity, [Type], []) :-
	% Sets properties for the relation used in weak unifications (by
	% default, this relation is always symmetric and reflexive)
	ClosureProperties = [symmetric, reflexive, transitive(Type)],
	flags:remove_bpl_flag(relation_properties(sim, _OldClosureProperties)),
	flags:add_bpl_flag(relation_properties(sim, ClosureProperties)).

directive(fuzzy_rel, [RelName, ClosureProperties], []) :-
	% Sets closure properties for "most/less general than"
	% relations or user defined fuzzy relations
	flags:remove_bpl_flag(relation_properties(RelName, _OldClosureProperties)),
	flags:add_bpl_flag(relation_properties(RelName, ClosureProperties)).

directive(domain, [Name, Min, Max, Unit], []) :-
	% Registers a new fuzzy domain
	flags:add_bpl_flag(fuzzy_domain(Name, [Min, Max, Unit])),
	flags:add_bpl_flag(fuzzy_subsets(Name, [])).

directive(fuzzy_set, [DomainName, Subsets], []) :-
	% Registers a new fuzzy set composed of one or more fuzzy subsets
	% (fuzzy subsets are added to the list of current fuzzy sets of
	% the specified domain)
	flags:get_bpl_flag(fuzzy_subsets(DomainName, CurrentSubsets)),
	append(CurrentSubsets, Subsets, NewSubsets),
	% Duplicates are removed before building the new list of subsets
	list_to_set(NewSubsets, NewSubsetsNoDuplicates),
	flags:remove_bpl_flag(fuzzy_subsets(DomainName, CurrentSubsets)),
	flags:add_bpl_flag(fuzzy_subsets(DomainName, NewSubsetsNoDuplicates)).

directive(weak_unification, [Type], []) :-
	% Sets the algorithm used for weak unifications: either a1, or a2 or a3
	% (by default, it is 'a1' - Sessa's incomplete algorithm for proximity relations)
	flags:set_bpl_flag(weak_unification(Type)).

directive(ext_block_equs, [Boolean], []) :-
	% Sets whether blocks are externally (true) or internally (false) computed 
	% for the weak unification algorithm 'a3'
	% (by default, it is true)
	flags:set_bpl_flag(ext_block_equs(Boolean)).

directive(fuzzy_logic, [TNorm], []) :-
	% Sets the current t-norm for resolution.  
	% (by default, it is min)
	retractall(evaluator:t_norm_current_op(_, _, _)),
	clause(evaluator:t_norm_op(TNorm, L, R, D), Body),
	assert((evaluator:t_norm_current_op(L, R, D) :- Body)),
	flags:set_bpl_flag(fuzzy_logic(TNorm)).

directive(wn_connect, [], []) :-
  ensure_loaded_wn.

directive(wn_connect, [QFolder], []) :-
	% Loads the WordNet interface  
	% Folder is the location of WordNet 3.0 installation
	utilities:remove_quotes(QFolder,Folder),
	setenv('WNDB', Folder),
  ensure_loaded_wn.

directive(wn_gen_prox_equations, [Measure, ListOfListOfWords], Equations) :-
	% Generate proximity equations between each pair of words in each 
	% list of ListOfListOfWords 
	is_list(ListOfListOfWords),
	!,
	wn_gen_prox_equations:wn_gen_prox_equations_list(ListOfListOfWords, Measure, Equations).
	
directive(wn_gen_prox_equations, [_Measure, _Auto], []).
	% Generate proximity equations between each pair of words in the program 
%	wn_gen_prox_equations:wn_auto_gen_prox_equations(Program, Equations).
	
	
% replaced_directive(?DirectiveName)
%
%     True if the directive DirectiveName is replaced during the compilation
%     For example, the directive wn_gen_prox_equations is replaced by all
%     the proximity equations that it automatically generates

replaced_directive(wn_gen_prox_equations).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Predicate for checking whether a BPL directive is correct or not
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%% is_directive_valid(+Name, +Arguments)
%
%     Checks if Name is the name of a Bousi-Prolog directive and, in
%     that case, checks also if its Arguments are correct. If Arguments
%     are invalid, a 'directive_error' exception with an error message
%     is thrown, whereas if Name doesn't represent a Bousi-Prolog
%     directive, this predicate fails.
%
%     @throws directive_error(Message) Name is the name of a
%      Bousi-Prolog directive but its Arguments aren't valid.
%

%
% :- lambda_cut(Lambda)
%

is_directive_valid(lambda_cut, [Lambda]) :-
	number(Lambda), Lambda >= 0, Lambda =< 1,
	% Directive is OK
	!.

is_directive_valid(lambda_cut, [_Lambda]) :-
	% Lambda-cut value is invalid
	!,
	throw_invalid_argument_error(lambda_cut, 1,
		'Lambda-cut value must be a number in range [0.0, 1.0].').

is_directive_valid(lambda_cut, Arguments) :-
	% Wrong number of arguments
	length(Arguments, Arity),
	throw_wrong_arity_error(lambda_cut, Arity, 1).

%
% :- filtering(Boolean)
%

is_directive_valid(filtering, [Boolean]) :-
	memberchk(Boolean,[true,false]),
	% Directive is OK
	!.

is_directive_valid(filtering, [_Boolean]) :-
	% Filtering value is invalid
	!,
	throw_invalid_argument_error(filtering, 1,
		'Filtering value must be either \'true\' or \'false\'.').

is_directive_valid(filtering, Arguments) :-
	% Wrong number of arguments
	length(Arguments, Arity),
	throw_wrong_arity_error(filtering, Arity, 1).

%
% :- transitivity(Type)
%

is_directive_valid(transitivity, [Type]) :-
	nonvar(Type),
	member(Type, [yes, no, min, product, luka, drastic, nilpotent, hamacher]),
	% Directive is OK
	!.

is_directive_valid(transitivity, [_Type]) :-
	% Transitivity type is invalid
	!,
	throw_invalid_argument_error(transitivity, 1,
		'Transitivity type must be in domain [yes, no, min, product, luka].').

is_directive_valid(transitivity, Arguments) :-
	% Wrong number of arguments
	length(Arguments, Arity),
	throw_wrong_arity_error(transitivity, Arity, 1).

%
% :- weak_unification(Type)
%

is_directive_valid(weak_unification, [Type]) :-
	nonvar(Type),
	member(Type, [a1, a2, a3]),
	% Directive is OK
	!.

is_directive_valid(weak_unification, [_Type]) :-
	% Weak_unification type is invalid
	!,
	throw_invalid_argument_error(weak_unification, 1,
		'Weak unification algorithm must be in domain [a1, a2, a3].').

is_directive_valid(weak_unification, Arguments) :-
	% Wrong number of arguments
	length(Arguments, Arity),
	throw_wrong_arity_error(weak_unification, Arity, 1).

%
% :- ext_block_equs(Type)
%

is_directive_valid(ext_block_equs, [Type]) :-
	nonvar(Type),
	member(Type, [true, false]),
	% Directive is OK
	!.

is_directive_valid(ext_block_equs, [_Type]) :-
	% ext_block_equs type is invalid
	!,
	throw_invalid_argument_error(ext_block_equs, 1,
		'External block equations processing must be in domain [true, false].').

is_directive_valid(ext_block_equs, Arguments) :-
	% Wrong number of arguments
	length(Arguments, Arity),
	throw_wrong_arity_error(ext_block_equs, Arity, 1).

%
% :- fuzzy_logic(TNorm)
%

is_directive_valid(fuzzy_logic, [TNorm]) :-
	nonvar(TNorm),
	member(TNorm, [min, product, luka, drastic, nilpotent, hamacher]),
	% Directive is OK
	!.

is_directive_valid(fuzzy_logic, [_TNorm]) :-
	% fuzzy_logic t-norm is invalid
	!,
	throw_invalid_argument_error(fuzzy_logic, 1,
		'Fuzzy logic t-norm must be in domain [min, product, luka, drastic, nilpotent, hamacher].').

is_directive_valid(fuzzy_logic, Arguments) :-
	% Wrong number of arguments
	length(Arguments, Arity),
	throw_wrong_arity_error(fuzzy_logic, Arity, 1).

%
% :- fuzzy_rel(Relation, Properties)
%

is_directive_valid(fuzzy_rel, [RelName, Properties]) :-
	nonvar(RelName), nonvar(Properties),
	RelName \== sim,
	utilities:relation_name(_RelSymbol, RelName),
	is_list(Properties),
	list_to_set(Properties, Properties),
	subset(Properties, [symmetric, reflexive, transitive, transitive(_)]),
	((RelName == lEqThan ; RelName == gEqThan) ->
		member(reflexive, Properties),
		(member(transitive, Properties) ; member(transitive(_), Properties))
	;
		true
	),
	(member(transitive(TNorm), Properties) ->
		member(TNorm, [yes, no, min, luka, product])
	;
		true
	),
	% Directive is OK
	!.

is_directive_valid(fuzzy_rel, [sim, _Properties]) :-
	% Similarity relation properties can't be changed
	!,
	throw_invalid_argument_error(fuzzy_rel, 2,
		'Similarity relation properties can\'t be changed with this directive. \c
		 Use \'transitivity/1\' to add transitivity to similarity relation.').

is_directive_valid(fuzzy_rel, [RelName, _Properties]) :-
	(RelName == lEqThan ; RelName == gEqThan),
	% "Less/Most general than" properties are partially fixed
	!,
	throw_invalid_argument_error(fuzzy_rel, 2,
		'"Less/Most general than" relations must be reflexive and transitive, \c
		 and transitive type must be in domain [yes, no, min, product, luka].').

is_directive_valid(fuzzy_rel, [RelName, _Properties]) :-
	nonvar(RelName),
	utilities:relation_name(_RelSymbol, RelName),
	% Closure properties list is invalid
	!,
	throw_invalid_argument_error(fuzzy_rel, 2,
		'Closure properties must be a subset of {symmetric, reflexive, \c
		 transitive, transitive(Type)}, and transitive type must be in domain \c
		 [yes, no, min, product, luka].').

is_directive_valid(fuzzy_rel, [_RelName, _Properties]) :-
	% Relation symbol is invalid
	!,
	throw_invalid_argument_error(fuzzy_rel, 2,
		'Relation symbol must be one of these: ~>, <~, ~1~, ~2~, ~3~.').

is_directive_valid(fuzzy_rel, Arguments) :-
	% Wrong number of arguments
	length(Arguments, Arity),
	throw_wrong_arity_error(fuzzy_rel, Arity, 2).

%
% :- domain(Name, Min, Max, Unit)
%

is_directive_valid(domain, [Name, Min, Max, Unit]) :-
	atomic(Name), nonvar(Name), not(number(Name)),
	not(utilities:atom_is_variable(Name)),
	atomic(Unit), nonvar(Unit), not(number(Unit)),
	not(utilities:atom_is_variable(Unit)),
	integer(Min), integer(Max),
	Min < Max,
 	not(flags:get_bpl_flag(fuzzy_domain(Name, [_, _, _]))),
	% Directive is OK
	!.

is_directive_valid(domain, [Name, Min, Max, Unit]) :-
	atomic(Name), nonvar(Name), not(number(Name)),
	not(utilities:atom_is_variable(Name)),
	atomic(Unit), nonvar(Unit), not(number(Unit)),
	not(utilities:atom_is_variable(Unit)),
	integer(Min), integer(Max),
	Min < Max,
	% Domain has already been defined
	!,
	swritef(Message, 'Fuzzy domain \'%w\' has already been defined.', [Name]),
	throw_invalid_argument_error(domain, 4, Message).

is_directive_valid(domain, [Name, Min, Max, Unit]) :-
	atomic(Name), nonvar(Name), not(number(Name)),
	not(utilities:atom_is_variable(Name)),
	atomic(Unit), nonvar(Unit), not(number(Unit)),
	not(utilities:atom_is_variable(Unit)),
	integer(Min), integer(Max),
	% Minimum and maximum values are swapped
	!,
	throw_invalid_argument_error(domain, 4,
		'Minimum value must be greater than maximum value.').

is_directive_valid(domain, [Name, _Min, _Max, Unit]) :-
	atomic(Name), nonvar(Name), not(number(Name)),
	not(utilities:atom_is_variable(Name)),
	atomic(Unit), nonvar(Unit), not(number(Unit)),
	not(utilities:atom_is_variable(Unit)),
	% Minimum and/or maximum values aren't integer numbers
	!,
	throw_invalid_argument_error(domain, 4,
		'Minimum and maximum values must be integers numbers.').

is_directive_valid(domain, [Name, _Min, _Max, _Unit]) :-
	(compound(Name) ; var(Name) ; number(Name)
	 ; utilities:atom_is_variable(Name)),
	% Domain name is a number, a free variable or a compound term
	!,
	throw_invalid_argument_error(domain, 4,
		'Domain name must be an atom.').

is_directive_valid(domain, [_Name, _Min, _Max, Unit]) :-
	(compound(Unit) ; var(Unit) ; number(Unit)
	 ; utilities:atom_is_variable(Unit)),
	% Domain measure unit is a number, a free variable or a compound term
	!,
	throw_invalid_argument_error(domain, 4,
		'Domain measure unit must be an atom.').

is_directive_valid(domain, Arguments) :-
	% Wrong number of arguments
	length(Arguments, Arity),
	throw_wrong_arity_error(domain, Arity, 4).

%
% :- fuzzy_set(DomainName, [SetName(V1, V2, V3, V4), SetName(V1, V2, V3), ...])
%

is_directive_valid(fuzzy_set, [DomainName, Subsets]) :-
	atomic(DomainName), nonvar(DomainName), not(number(DomainName)),
	not(utilities:atom_is_variable(DomainName)),
	flags:get_bpl_flag(fuzzy_domain(DomainName, _)),
	findall(Name, flags:get_bpl_flag(fuzzy_domain(Name, [_Min, _Max, _Unit])), Domains),
	get_subset_names(Domains, CurrentSubsetNames),
	check_fuzzy_subsets(Subsets, CurrentSubsetNames),
	% Directive is OK
	!.

is_directive_valid(fuzzy_set, [DomainName, _Subsets]) :-
	(compound(DomainName) ; var(DomainName) ; number(DomainName)
	 ; utilities:atom_is_variable(DomainName)),
	% Domain name is a number, a free variable or a compound term
	!,
	swritef(Message, 'Domain name must be an atom.', [DomainName]),
	throw_invalid_argument_error(fuzzy_set, 2, Message).

is_directive_valid(fuzzy_set, [DomainName, _Subsets]) :-
	% Domain hasn't been defined yet
	!,
	swritef(Message, 'Fuzzy domain \'%w\' is undefined.', [DomainName]),
	throw_invalid_argument_error(fuzzy_set, 2, Message).

is_directive_valid(fuzzy_set, Arguments) :-
	% Wrong number of arguments
	length(Arguments, Arity),
	throw_wrong_arity_error(fuzzy_set, Arity, 2).

is_directive_valid(wn_connect, []) :-
	(getenv('WNDB',_Folder)
	 ->
	  true
	 ;
	 	utilities:checkwnenv(Folder),
	  (exists_directory(Folder)
	    ->
	     true
	   ; 
  	  % The WNDB variable is not set
    	throw_invalid_argument_error(wn_connect, 0,
    		'WNDB environment variable is not set or is incorrect. Please either set it at the OS terminal or use this directive with an argument indicating the directory of the Wordnet database.')
    )
  ).

is_directive_valid(wn_connect, [QFolder]) :-
  utilities:remove_quotes(QFolder,Folder),
	(exists_directory(Folder)
	 ->
	  setenv('WNDB',QFolder)
	 ;
	  % The argument is not a valid directory
  	throw_invalid_argument_error(wn_connect, 1,
  		'Argument must be an existent directory.')).

is_directive_valid(wn_gen_prox_equations, [Measure, ListOfListOfWords]) :-
  is_list(ListOfListOfWords),
  (maplist(is_list,ListOfListOfWords)
   ->
    ensure_loaded_wn,
  	(\+ wn_measure(Measure)
  	 ->
  	  % The first argument is not a valid measure
    	throw_invalid_argument_error(wn_gen_prox_equations, 2,
    		'Argument must be a valid measure.')
     ;
  	  flatten(ListOfListOfWords, ListOfWords),
  	  check_wn_words(ListOfWords, WordNotFound),
  	  (var(WordNotFound)
    	 ->
    	  true
    	 ;
    	  % One word is not found
    	  atomic_list_concat(['Word not found in WordNet database: ', WordNotFound, '.'], ErrorMessage),
      	throw_invalid_argument_error(wn_gen_prox_equations, 2,
      		ErrorMessage)))
   ;
	  % The second argument is not a list of lists
  	throw_invalid_argument_error(wn_gen_prox_equations, 2,
  		'Second argument must be a list of lists.')
  ).
    	
    
%is_directive_valid(wn_gen_prox_equations, _).

is_directive_valid(wn_gen_prox_equations, [Measure, Auto]) :-
  ensure_loaded_wn,
	(\+ wn_measure(Measure)
	 ->
	  % The argument is not a valid measure
  	throw_invalid_argument_error(wn_gen_prox_equations, 1,
  		'Argument must be a valid measure.')
   ;
  	(\+ member(Auto, [auto, automatic, automatically])
  	 ->
  	  % The argument is not a valid switch
    	throw_invalid_argument_error(wn_gen_prox_equations, 1,
    		'Argument must be either ''auto'', or ''automatic'', or ''automatically''.')
     ;
      true)).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Additional predicate for checking fuzzy_set/2 directive
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%% check_fuzzy_subsets(+Subsets, +DefinedSubsets)
%
%     Checks if the list of fuzzy subsets of a certain domain is valid.
%     DefinedSets must be a list with all the fuzzy subsets that have
%     already been defined for all domains.
%

check_fuzzy_subsets([], _DefinedSubsets).

check_fuzzy_subsets([Subset|MoreSubsets], DefinedSubsets) :-
	compound(Subset),
	Subset =.. [SubsetName|Values],
	atomic(SubsetName), nonvar(SubsetName), not(number(SubsetName)),
	not((member(AnySubset, DefinedSubsets), AnySubset =.. [SubsetName|_])),
	(length(Values, 3) ; length(Values, 4)),
	ascending_numbers(Values),
	% This fuzzy subset is OK
	!,
	NewDefinedSubsets = [SubsetName|DefinedSubsets],
	check_fuzzy_subsets(MoreSubsets, NewDefinedSubsets).

check_fuzzy_subsets([Subset|_MoreSubsets], DefinedSubsets) :-
	compound(Subset),
	Subset =.. [SubsetName|Values],
	atomic(SubsetName), nonvar(SubsetName), not(number(SubsetName)),
	not((member(AnySubset, DefinedSubsets), AnySubset =.. [SubsetName|_])),
	(length(Values, 3) ; length(Values, 4)),
	% One of the values that define the fuzzy subset isn't a number,
	% or the numbers are not in ascending order
	!,
	throw_invalid_argument_error(fuzzy_set, 2,
		'The points that define a fuzzy subset must be integer numbers, \c
		 and they must be in ascending order.').

check_fuzzy_subsets([Subset|_MoreSubsets], DefinedSubsets) :-
	compound(Subset),
	Subset =.. [SubsetName|_Values],
	atomic(SubsetName), nonvar(SubsetName), not(number(SubsetName)),
	not((member(AnySubset, DefinedSubsets), AnySubset =.. [SubsetName|_])),
	% The fuzzy subset has been defined with an unexpected number of values
	!,
	throw_invalid_argument_error(fuzzy_set, 2,
		'Each linguistic term must be defined by a list with 3 integer \c
	     numbers for triangular subsets or 4 integer numbers for \c
	     trapezoidal subsets.').

check_fuzzy_subsets([Subset|_MoreSubsets], _DefinedSubsets) :-
	compound(Subset),
	Subset =.. [SubsetName|_Values],
	atomic(SubsetName), nonvar(SubsetName), not(number(SubsetName)),
	% Fuzzy subset has already been defined
	!,
	swritef(Message, 'Fuzzy subset \'%w\' is defined more than once.', [SubsetName]),
	throw_invalid_argument_error(fuzzy_set, 2, Message).

check_fuzzy_subsets([Subset|_MoreSubsets], _DefinedSubsets) :-
	compound(Subset),
	Subset =.. [SubsetName|_Values],
	(compound(SubsetName) ; var(SubsetName) ; number(SubsetName)),
	% Fuzzy subset name is a number, a free variable or a compound term
	!,
	swritef(Message, 'Fuzzy subset name must be an atom.', [SubsetName]),
	throw_invalid_argument_error(fuzzy_set, 2, Message).

check_fuzzy_subsets([_Subset|_MoreSubsets], _DefinedSubsets) :-
	% Fuzzy set definition isn't a compound term
	!,
	throw_invalid_argument_error(fuzzy_set, 2,
		'A fuzzy set must be defined using a compound term.').



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Predicates for throwing errors when BPL directives aren't valid
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%% throw_invalid_argument_error(+Name, +Arity, +Message)
%
%     Throws a directive_error/1 exception indicating that the
%     Name/Arity Bousi-Prolog directive specified isn't correct
%     because of the reason explained in Message string.
%
%     @throws directive_error(Message)
%

throw_invalid_argument_error(Name, Arity, Message) :-
	swritef(ErrorMessage, 'BPL directive \'%w/%w\' is invalid. %w',
		[Name, Arity, Message]),
	throw(directive_error(ErrorMessage)).


%% throw_wrong_arity_error(+Name, +Arity, +CorrectArity)
%
%      Throws a directive_error/1 exception indicating that Name/Arity
%      is not a valid Bousi-Prolog directive, but Name/CorrectArity is.
%
%      @throws directive_error(Message)
%

throw_wrong_arity_error(Name, Arity, CorrectArity) :-
	swritef(ErrorMessage, 'BPL directive \'%w/%w\' is invalid. Use \'%w/%w\' \c
		instead.', [Name, Arity, Name, CorrectArity]),
	throw(directive_error(ErrorMessage)).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Miscellaneous predicates
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%% get_subset_names(+DomainNames, -SubsetNames)
%
%     Returns a list with the names of all the subsets defined in each
%     of the domains of the DomainNames list.
%

get_subset_names(DomainNames, SubsetNames) :-
	get_subset_names_aux(DomainNames, [], SubsetNames),
	!.


%% get_subset_names_aux(+DomainNames, +SubsetNames, -FinalSubsetNames)
%
%     Internal predicate used by get_subset_names/2 which includes
%     an accumulator pair. FinalSubsetNames is unified with SubsetNames
%     when DomainNames list has no more items.
%
%     @see get_subset_names/2
%

get_subset_names_aux([], FinalSubsetNames, FinalSubsetNames).

get_subset_names_aux([Domain|MoreDomains], SubsetNames, FinalSubsetNames) :-
	(
		% Gets the names of the subsets of the given domain
		flags:get_bpl_flag(fuzzy_subsets(Domain, Subsets)),
		findall(SubsetName, (member(SubsetDefinition, Subsets),
		                     SubsetDefinition =.. [SubsetName|_]),
		       ThisSubsetNames)
	;
		% The given domain doesn't have any subset yet
		ThisSubsetNames = []
	),
	% Adds the subset names to the list and calls this predicate recursively
	append(SubsetNames, ThisSubsetNames, NextSubsetNames),
	get_subset_names_aux(MoreDomains, NextSubsetNames, FinalSubsetNames).

