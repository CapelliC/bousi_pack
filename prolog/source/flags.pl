%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% System flags handler

:- module(flags, [
		get_bpl_flag/1,         % ?Flag
		remove_bpl_flag/1,      % +Flag
		add_bpl_flag/1,         % +Flag
		reset_bpl_flags/0,      %
		backup_bpl_flags/0,     %
		restore_bpl_flags/0,    %
		current_bpl_flags/1     % -Flags
%		tpl_flags/1
   ]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- set_prolog_flag(double_quotes, codes).

%:- dynamic tpl_flags/1.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Predicates for handling system flags
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%% get_bpl_flag(?Flag)
%
%     Queries current system flags.
%

get_bpl_flag(Flag) :-
	bpl_flags(Flag).


%% set_bpl_flag(?Flag)
%
%     Set a system flag: retract it if it exists already, and 
%     assert the new value. Use this predicate if only one 
%     clause is expected for the flag. Otherwise, use the pair
%     remove_bpl_flag/1 and add_bpl_flag/1.
%

set_bpl_flag(Flag) :-
  Flag =.. [Name|Args],
  length(Args,L),
  length(OpenArgs,L),
  OpenFlag =.. [Name|OpenArgs],
	remove_bpl_flag(OpenFlag),
	add_bpl_flag(Flag).


% add_bpl_flag(+Flag)
%
%     Adds a new Flag to the current system flags. Does nothing if
%     Flag already belongs to the system flags.
%

add_bpl_flag(Flag) :-
	bpl_flags(Flag),
	% Flag already exists
	!.

add_bpl_flag(Flag) :-
	% Flag doesn't exist and must be added
	assert(bpl_flags(Flag)).


%% remove_bpl_flag(+Flag)
%
%     Removes a Flag from the current system flags. Does nothing if
%     Flag doesn't belong to the system flags.
%

remove_bpl_flag(Flag) :-
	bpl_flags(Flag),
	% Flag exists and must be removed
	!,
	retract(bpl_flags(Flag)).

remove_bpl_flag(_Flag).
	% Flag doesn't exist


%% backup_bpl_flags
%
%     Saves a copy of the current system flags that can be later
%     restored with restore_bpl_flags/0.
%
%     @see restore_bpl_flags/0
%

backup_bpl_flags :-
	% Removes previously saved flags
	retractall(saved_bpl_flags(_OldSavedFlags)),
	% Copies all "bpl_flags(X)" to "saved_bpl_flags(X)"
	findall(saved_bpl_flags(Flag), bpl_flags(Flag), FlagsToSave),
	maplist(assert, FlagsToSave).


%% restore_bpl_flags
%
%     Restores the system flags that were previously saved with
%     backup_bpl_flags/0.
%
%     @see backup_bpl_flags/0
%

restore_bpl_flags :-
	% Removes current system flags
	retractall(bpl_flags(_OldFlags)),
	% Copies all "saved_bpl_flags(X)" to "bpl_flags(X)"
	findall(bpl_flags(Flag), saved_bpl_flags(Flag), FlagsToRestore),
	maplist(assert, FlagsToRestore).


%% current_bpl_flags(-Flags)
%
%     Retrieves the current system flags.
%

current_bpl_flags(Flags) :-
	findall(Flag, bpl_flags(Flag), Flags).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Predicates for reseting system flags
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%% reset_bpl_flags
%
%     Sets the default values for all BPL flags.
%

reset_bpl_flags :-
	reset_program_prefix,
	reset_lambda_cut,
	reset_filtering,
	reset_weak_unification,
	reset_ext_block_equs,
	reset_fuzzy_logic,
	reset_continue,
	reset_relation_properties([sim, lEqThan, gEqThan, frel1, frel2, frel3]),
	reset_fuzzy_subsets.


%% reset_program_prefix
%
%     Sets a default program name as the prefix of the currently loaded
%     program.
%

reset_program_prefix :-
	default_program_prefix(Prefix),
	set_bpl_flag(program_prefix(Prefix)).


%% reset_lambda_cut(+Lambda)
%
%     Sets the default lambda-cut value in BPL flags.
%

reset_lambda_cut :-
	default_lambda(Lambda),
	set_bpl_flag(lambda_cut(Lambda)).


%% reset_filtering(+Boolean)
%
%     Sets the default filtering in BPL flags.
%

reset_filtering :-
	default_filtering(Boolean),
	set_bpl_flag(filtering(Boolean)).


%% reset_weak_unification
%
%     Sets the default weak unification algorithm in BPL flags.
%

reset_weak_unification :-
	default_weak_unification(Algorithm),
	set_bpl_flag(weak_unification(Algorithm)).


%% reset_ext_block_equs
%
%     Sets the default external block equations in BPL flags.
%

reset_ext_block_equs :-
	default_ext_block_equs(Boolean),
	set_bpl_flag(ext_block_equs(Boolean)).


%% reset_fuzzy_logic
%
%     Sets the fuzzy logic t-norm flag.
%

reset_fuzzy_logic :-
	default_fuzzy_logic(TNorm),
	set_bpl_flag(fuzzy_logic(TNorm)).


%% reset_continue
%
%     Set the main loop to continue.
%

reset_continue :-
	default_continue(Boolean),
	set_bpl_flag(continue(Boolean)).


%% reset_relation_properties(+Relation)
%
%     Sets the default closure properties and t-norm in BPL flags for
%     the specified Relation. If Relation is a list, this predicate
%     will be called once for each item.
%

reset_relation_properties([]) :-
	!.

reset_relation_properties([RelName|OtherRelNames]) :-
	% Calls this predicate recursively for each item
	reset_relation_properties(RelName),
	reset_relation_properties(OtherRelNames).

reset_relation_properties(RelName) :-
	% Gets default closure properties, converts them into a number
	% and stores it in BPL flags along with t-norm identifier
	atom(RelName),
	default_closure(RelName, ClosureProperties),
	remove_bpl_flag(relation_properties(RelName, _ClosureProperties)),
	add_bpl_flag(relation_properties(RelName, ClosureProperties)).


%% reset_fuzzy_sets
%
%     Removes all domains and fuzzy subsets from BPL flags.
%

reset_fuzzy_subsets :-
	retractall(bpl_flags(fuzzy_domain(_DomainName1, _Definition))),
	retractall(bpl_flags(fuzzy_subsets(_DomainName2, _Subsets))).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Default values for system flags
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%% default_program_prefix(?Prefix)
%
%     Unifies Prefix with the default prefix that will be appended to
%     user-defined predicates when no program is loaded.
%

default_program_prefix('none').


%% default_lambda(?Value)
%
%     Unifies Value with the default lambda-cut value, i.e., the lower
%     bound allowed for the approximation degree of weak unifications.
%     By default, this number is 0, so none of the computations are cut
%     regardless of their approximation degree.
%

default_lambda(0).


%% default_filtering(?Value)
%
%     Unifies Value with the default filtering value (this value can be
%     either 'true' for enabling filtering, or 'false' for disabling it).
%     By default, this value is 'true'.
%

default_filtering(true).


%% default_weak_unification(?Algorithm)
%
%     Unifies Algorithm with the default weak unification algoritm.
%     By default, this algorithm is 'a1', and other current possible
%     algorithms are 'a2' and 'a3'.
%

default_weak_unification('a3').


%% default_ext_block_equs(?Boolean)
%
%     Unifies Boolean with the default external block equations Boolean
%     flag. By default, this value is true, meaning that computing 
%     these equations is externally processed.
%

default_ext_block_equs('true').


%% default_fuzzy_logic(?TNorm)
%
%     Unifies TNorm with the default t-norm for resolution.
%     By default, this value is min, meaning that the used t-norm 
%     is goedel.
%

default_fuzzy_logic('min').


%% default_continue(?Value)
%
%     Unifies Value with the default. By default, this value is 'yes', 
%     meaning that the main loop continues. If set to 'no', the main 
%     loop stops.
%

default_continue('yes').


%% default_closure(?Relation, ?Properties)
%
%     Returns a list with the names of the default closure Properties
%     of a certain Relation. By default, a proximity relation is used
%     in weak unifications; partial orders are applied in 'more/less
%     general than' fuzzy relations; and custom fuzzy binary relations
%     are defined as similarity relations.
%

default_closure(SimRelation, [symmetric, reflexive]) :-
	member(SimRelation, [sim]).

default_closure(GeneralRelation, [reflexive, transitive(TNorm)])  :-
	member(GeneralRelation, [gEqThan, lEqThan]),
	default_t_norm(GeneralRelation, TNorm).

default_closure(CustomRelation, [symmetric, reflexive, transitive(TNorm)])  :-
	member(CustomRelation, [frel1, frel2, frel3]),
	default_t_norm(CustomRelation, TNorm).


%% default_t_norm(?Relation, ?TNorm)
%
%     Returns the name of the default TNorm to be used to compute the
%     transitive closure of a certain Relation. 'Minimum' is the
%     default t-norm in all the relations except in the one used in the
%     weak unification process, where no transitivity is applied.
%

default_t_norm(sim, no).
default_t_norm('~', min).
default_t_norm(gEqThan, min).
default_t_norm(lEqThan, min).
default_t_norm(frel1, min).
default_t_norm(frel2, min).
default_t_norm(frel3, min).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Dynamic predicates
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%% bpl_flags(?Flag)                                          is dynamic
%
%     Dynamic predicate which stores some flags that affect the
%     behavior of the Bousi-Prolog system. Currently, the following
%     flags are supported:
%
%      * lambda_cut(Lambda): defines the lambda-cut value, i.e., the
%        lower bound allowed for the approximation degree of weak
%        unifications.
%      * relation_properties(Name, Properties): contains the name of
%        the closure properties that should be applied when computing
%        the closure of each fuzzy relation. Valid properties are
%        "symmetric", "reflexive" and "transitive(TNorm)", where TNorm
%        can be "yes", "no", "min", "product" or "luka".
%      * fuzzy_domain(DomainName, Definition): defines the domains to
%        which belong the fuzzy subsets. Each domain definition must
%        be a list consisting of three items: [Min, Max, MeasureUnit].
%      * fuzzy_subsets(DomainName, Subsets): declares the lists of
%        fuzzy subsets that belong to each domain. Subset definitions
%        must follow the syntax of ext_translate_fuzzysets/5.
%      * program_prefix(Prefix): prefix that represents the program
%        currently loaded into memory. This prefix must be appended to
%        every call to a user-defined predicate.
%

:- dynamic bpl_flags/1.


%% saved_bpl_flags(?Flag)                                    is dynamic
%
%     Dynamic predicate used by backup_bpl_flags/0 and
%     restore_bpl_flags/0 to store a backup copy of the system flags.
%

:- dynamic saved_bpl_flags/1.

