%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Bousi-Prolog to Prolog (TPL) translator

:- module(translator, [
		translate_program/4,    % +InputProgram, +InputOntology,
		                        %  +OutputFile, +StateFile
		translate_query/4       % +String, -Query, -Bindings, -Degree
   ]).

:- use_module(parser).
:- use_module(evaluator).
:- use_module(flags).
:- use_module(utilities).

:- use_module(library(lists)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- set_prolog_flag(double_quotes, codes).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Translation of Bousi-Prolog files
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%% translate_program(+InputProgram, +InputOntology, +OutputFile, +StateFile)
%
%     Reads a Bousi-Prolog program together with a Bousi-Prolog
%     ontology, performs a lexical, syntactic and semantic analysis,
%     and generates an intermediate TPL file that can directly be
%     loaded into Prolog. InputOntology can be '' (an empty string) if
%     no ontology is needed. Current flags are saved in StateFile.
%
%     If the compilation of the program or the ontology causes an
%     error, this predicate will fail and won't generate anything.
%

translate_program(InputProgram, InputOntology, OutputFile, StateFile) :-
	% Parses the specified program and ontology files and gets all the
	% directives, rules and equations that are defined in them
	parser:parse_program(InputProgram, InputOntology, Directives, Rules,
	                     Equations, LingTerms, Messages),
	!,
	% Builds a list with the text of the messages returned by parser
	Template = [_File, _Line, _Column, Text, _Type],
	findall(Text, member(Template, Messages), TextMessages),
	% Checks if parser generated any error
	(member([_, _, _, _, error], Messages) ->
		% Shows errors and warnings and then stops translation
		forall(member(Message, TextMessages), (write(Message), nl)),
		fail
	;
		% Warnings aren't shown here because they'll be shown when
		% loading the TPL file
		true
	),
	% List of t-norms for the different relations
	build_t_norms(Directives,TNorms),
	% Adds the linguistic terms to the list of fuzzy subsets found in
	% source code
	add_linguistic_terms(LingTerms, AddedSubsets),
	% Computes the closure of each fuzzy relation 
	expand_equations([sim, gEqThan, lEqThan, frel1, frel2, frel3],
	                 Equations, ExpEquations1),
  % Converts the fuzzy sets into a list of equations of the binary fuzzy relation	sim
  translate_fuzzy_sets([], ExpEquations2),
	append(ExpEquations1, ExpEquations2, ExpEquations),
	build_block_equations(ExpEquations, ExpBlockEquations),
	% Expands the list of rules using the proximity/similarity relation
	utilities:simplify_filename(InputProgram, ProgramPrefix),
	expand_rules(Rules, ExpRules, ProgramPrefix, ExpEquations),
	% Writes TPL code to output file
	telling(CurrentOutput),
	tell(OutputFile),
	writeq((:- style_check([-singleton, -discontiguous]))), write('.'), nl,
	(Messages == [] ->
		true
	;
		% Writes warnings in TPL file to show them each time the file is loaded
		create_writes(TextMessages, Writes),
		writeq((:- initialization(Writes))), write('.'), nl
	),
	utilities:write_lines(Directives, '', '.'), nl,
	utilities:write_lines(TNorms, '', '.'), nl,
	(AddedSubsets == [] ->
		true
	;
		% Adds new fuzzy_set/2 directives to define the linguistic terms
		% built with the '#' operator found in the BPL code
		write_fuzzy_set_directives(AddedSubsets)
	),
%	(\+ flags:get_bpl_flag(weak_unification('a3')) ->
  	utilities:write_lines(ExpEquations, '', '.'), nl,
%  ;
	  utilities:write_lines(ExpBlockEquations, '', '.'),
%	),
	nl,
	utilities:write_lines(ExpRules, '', '.'), nl,
	told,
	tell(CurrentOutput),
	tell(StateFile),
	current_bpl_flags(Flags),
%	writef('%w.\n%w.\n', [':- dynamic tpl_flags/1', flags:tpl_flags(Flags)]),
%	writef('%w.\n', [tpl_flags(Flags)]),
	writef('%w.\n%w.\n', [':- multifile tpl_flags/1', tpl_flags(Flags)]),
	told.

	
%% build_t_norms(+Directives,-TNorms)
%
%     Returns the list of terms t_norm(Relation,TNorm)
%     representing the t-norm for each Relation.
%     If no directive is available for a given relation,
%     its default t-norm is used instead.
%     These terms are added to the tpl program, so that
%     they define the predicate t_norm/2.
%

build_t_norms(Directives,TNorms) :-
  findall(t_norm(Relation,Type),
            (member(fuzzy_rel(Relation,Properties),Directives),
             member(transitive(Type),Properties)
            ),
          FTNorms
          ),
  (member(t_norm('~',_Type),FTNorms) -> % '~' must be present in any BPL-program
     TNorms=FTNorms
   ;
     flags:default_t_norm('~',Type),
     TNorms=[t_norm('~',Type)|FTNorms]
  ).
  

%% create_writes(+Messages, -Writes)
%
%     Builds a conjunction of write/1 and nl/0 predicates that can be
%     later used to show the specified list of Messages.
%

create_writes([], true).

create_writes([Message|MoreMessages], Writes) :-
	SingleWrite = (write(Message), nl),
	create_writes(MoreMessages, MoreWrites),
	(MoreWrites == true ->
		Writes = SingleWrite
	;
		Writes = (SingleWrite, MoreWrites)
	).


%% write_fuzzy_set_directives(+Subsets)
%
%     Writes a directive ':- directive(fuzzy_set, Item).' in current
%     output stream for each Item in Subsets list.
%

write_fuzzy_set_directives([]).

write_fuzzy_set_directives([Subset|MoreSubsets]) :-
	writeq((:- directive(fuzzy_set, Subset))), write('.'), nl,
	write_fuzzy_set_directives(MoreSubsets).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Translation of Bousi-Prolog queries
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%% translate_query(+String, -Query, -Bindings, -Degree)
%
%     Reads the query contained in String, performs a lexical,
%     syntactic and semantic analysis, and returns the following:
%
%      * A term that can be used to execute the query.
%      * A list with the variable bindings (bindings of variables
%        starting with an underscore character are not returned).
%      * The variable in which the approximation degree of the query
%        will be stored after launching it.
%
%     If the translation of the query causes an error, this predicate
%     will fail.
%

translate_query(String, Query, Bindings, Degree) :-
	% Parses the specified query to get an executable term
	flags:get_bpl_flag(program_prefix(ProgramPrefix)),
	parser:parse_query(ProgramPrefix, String, ParsedQuery, LingTerms, Messages),
	% Builds a list with the text of the error messages returned by parser
	Template = [_File, _Line, _Column, Text, error],
	findall(Text, member(Template, Messages), TextMessages),
	% Checks if parser generated any error
	(TextMessages \== [] ->
		% Shows errors and then stops translation
		forall(member(Message, TextMessages), (write(Message), nl)),
		fail
	;
		true
	),
	% Converts the executable term into a string and then reads it
	% to get the list of variable bindings
	swritef(QueryString, '%w.', [ParsedQuery]),
	catch((
		atom_to_term(QueryString, QueryTerm, AllBindings)
	% (catcher)
	), _Exception, (
		fail
	)),
	% Removes bindings of unimportant variables (those starting with '_')
	findall((VarName = Var),
	        (member((VarName = Var), AllBindings), atom_chars(VarName, ['_'|_Chars])),
	        UnimportantBindings),
	subtract(AllBindings, UnimportantBindings, Bindings),
	% Adds the linguistic terms found in the query to the current
	% list of fuzzy subsets
	add_linguistic_terms(LingTerms, AddedSubsets),
	(AddedSubsets == [] ->
		% All of the subsets were already on the list, no further
		% translation is needed
		true
	;
		% New subsets were found, so new proximity/similarity equations
		% must be computed and asserted into the SWI-Prolog database
		translate_fuzzy_sets(AddedSubsets, Equations),
%		evaluator:add_sim_equations(Equations)
		evaluator:update_sim_equations(Equations, Updated),
		bplShell:reload_on_extra_equations(Equations, Updated)
	),
	% Adds the computation of the approximation degree to the query
	QueryTerm = [RealQuery, DegreeVars],
%	QueryAux = (RealQuery, min_degree(DegreeVars, Degree)),
	QueryAux = (RealQuery, degree_composition(DegreeVars, Degree)),
	% Translates assert/1 and retract/1 predicates
	evaluator:get_sim_equations(CurrentEquations),
	translate_asserts_retracts(QueryAux, Query, ProgramPrefix, CurrentEquations).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Expansion of rules
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%% expand_rules(+Rules, -ExpandedRules, +ProgramPrefix, +Equations)
%
%     Scans a list of Rules returned by Bousi-Prolog parser and, for
%     each symbol that is similar to each clause's head (attending to
%     Equations list), builds a translated clause and stores it in the
%     ExpandedRules list. Rules that aren't clauses are copied as is
%     in the output list.
%
%     For example, given ProgramPrefix = 'prog', the same rule as in
%     the example of the 'parser' module, [(prog_a(X, DG) :-
%     prog_b(X, D1), prog_c(D2), true), [D1, D2]], and the equations
%     [sim(a, p, 0.5), sim(X, X, 1)], this predicate will return the
%     following set of rules:
%
%      * prog_a(A1, DG) :- unify_arguments_a1[[A1, X, DA1]]),
%                          prog_b(X, D1), prog_c(D2), true,
%                          min_degree([D1, D2, DA1], DG).
%      * prog_p(A1, DG) :- over_lambdacut(0.5),
%                          unify_arguments_a1[[A1, X, DA1]]),
%                          prog_b(X, D1), prog_c(D2), true,
%                          min_degree([D1, D2, DA1, 0.5], DG).
%

%% expand_rule(+SimDegrees, +Clause, +RuleDegreeVars, +ProgramPrefix, -ExpandedClauses)
%
%     Internal predicate used to expand a single Clause. SimDegrees
%     must be a list consisting of lists with two items: a symbol and
%     an approximation degree.
%
%     @see expand_rules/4
%

expand_rules([], [], _ProgramPrefix, _Equations).

expand_rules([[Rule, HeadConstraintBlockVars, BodyConstraintBlockVars, DegreeVars]|MoreRules], ExpandedRules, ProgramPrefix, Equations) :-
	(Rule = (Head :- Body) ->
		% Removes the program prefix from the rule's head
		functor(Head, FunctorWithPrefix, _Arity),
		atom_chars(ProgramPrefix, PrefixChars),
		atom_chars(FunctorWithPrefix, FunctorWithPrefixChars),
		append(PrefixChars, ['_'], PrefixChars2),
		append(PrefixChars2, FunctorChars, FunctorWithPrefixChars),
		atom_chars(Functor, FunctorChars),
		% Translates the assert/1 and retract/1 predicates of the rule's body
		(Body \== true ->
			translate_asserts_retracts(Body, NewBody, ProgramPrefix, Equations)
		;
			NewBody = true
		),
		% Extracts all the symbols that are similar to this rule's head
		findall([Sim, Degree], member(sim(Functor, Sim, Degree), Equations), List),
		% Expands this rule using the resulting list
		expand_rule(List, (Head :- NewBody), HeadConstraintBlockVars, BodyConstraintBlockVars, DegreeVars, ProgramPrefix, ExpandedRules1)
	;
		% Rules that aren't clauses don't need expansion
		% (note that facts must have 'true' as their body)
		ExpandedRules1 = [Rule]
	),
	% Expands the remaining rules
	expand_rules(MoreRules, ExpandedRules2, ProgramPrefix, Equations),
	append(ExpandedRules1, ExpandedRules2, ExpandedRules).
	

expand_rule([], _Clause, _HeadConstraintBlockVars, _BodyConstraintBlockVars, _RuleDegreeVars, _ProgramPrefix, []).

expand_rule([[Symbol, Degree]|MoreSimDegrees], Clause, HeadConstraintBlockVars, BodyConstraintBlockVars, RuleDegreeVars, ProgramPrefix, [ExpClause|MoreExpClauses]) :-
	Clause = (Head :- Body),
	% Creates the lists of variables and approximation degrees that
	% will be used in the weak unifications of each rule's argument
	Head =.. [_HeadFunctor|HeadArgsWithCtrsAndDegree],
	length(HeadArgsWithCtrsAndDegree, ExtendedRuleArity),
  actual_rule_arity(ExtendedRuleArity,RuleArity),
	length(HeadArgs, RuleArity),
	length(HeadVars, RuleArity),
	length(HeadDegrees, RuleArity),
  append(HeadArgs, CtrsAndDegreeVar, HeadArgsWithCtrsAndDegree),
  append(_HeadCtrs,[HeadDegreeVar],CtrsAndDegreeVar),
%  init_ctr_store(HeadFunctor,Cin),
	create_unification_problems(HeadVars, HeadArgs, HeadCin, HeadCout, HeadDegrees, UnificationProblems),
	% Links head and body block constraints variables through the 
	% unification problems
	link_rule_block_constraint_variables(HeadConstraintBlockVars, BodyConstraintBlockVars, HeadCin, HeadCout),
	% Builds the new rule's head with the symbol that is similar to the
	% original functor and the variables of the HeadVars list
	append(HeadVars, CtrsAndDegreeVar, NewHeadArgsWithCtrsAndDegree),
	concat_atom([ProgramPrefix, '_', Symbol], SymbolWithPrefix),
	NewHead =.. [SymbolWithPrefix|NewHeadArgsWithCtrsAndDegree],
	build_unify_arguments_goal(UnificationProblems,UnifyArgsGoal),
	(Degree < 1 ->
		% Builds the new rule's body with a check of the lambda-cut value 
		% (if needed),
		% the weak unification of the arguments, the original body and the
		% computation of the approximation degree
		append(RuleDegreeVars, HeadDegrees, DegreeVarsAux),
		append(DegreeVarsAux, [Degree], DegreeVars),
		build_pre_over_lambda_cut_goal(Degree, PreOverLCGoal),
    build_degree_composition_goal(DegreeVars, HeadDegreeVar, TNormGoal),
		build_post_over_lambda_cut_goal(HeadDegreeVar, PostOverLCGoal),
		utilities:append_goals_list([
		                   PreOverLCGoal,
		                   UnifyArgsGoal,
		                   Body,
		                   TNormGoal,
		                   PostOverLCGoal], 
		                  NewBody)
	;
		% Builds the new rule's body with the weak unification of the
		% arguments, the original body and the computation of the
		% approximation degree
		append(RuleDegreeVars, HeadDegrees, DegreeVars),
    build_degree_composition_goal(DegreeVars, HeadDegreeVar, TNormGoal),
		build_post_over_lambda_cut_goal(HeadDegreeVar, PostOverLCGoal),
		utilities:append_goals_list([
		                   UnifyArgsGoal,
		                   Body,
		                   TNormGoal,
		                   PostOverLCGoal], 
		                  NewBody)
	),
	(NewBody==true ->
	  ExpClause = (NewHead)
	 ;
	  ExpClause = (NewHead :- NewBody)
	),
	% Scans the remaining symbols
	expand_rule(MoreSimDegrees, Clause, HeadConstraintBlockVars, BodyConstraintBlockVars, RuleDegreeVars, ProgramPrefix,
	            MoreExpClauses).

	            
%% build_unify_arguments_goal(+UnificationProblems, -Goal)
%
%     Creates a goal for computing the unification of the
%     predicate arguments, expressed as a list of unification
%     problems.
%      
build_unify_arguments_goal([], true) :-
  !.
  
build_unify_arguments_goal(UnificationProblems, UnifyArgsGoal) :-
	flags:get_bpl_flag(weak_unification(Algorithm)),
  atom_concat('unify_arguments_', Algorithm, GoalName),
  UnifyArgsGoal =.. [GoalName, UnificationProblems].
%  unify_arguments_a1(UnificationProblems)


%% build_pre_over_lambda_cut_goal(+Degree, -Goal)
%
%     Creates a goal for the rule's lambda-cut threshold. 
%     If filtering is enabled and t-norm min is selected,
%     there is no need for this goal, as proximity equations
%     are filtered and rules below such threshold are not 
%     generated
%      

build_pre_over_lambda_cut_goal(_Degree, true) :-
	flags:get_bpl_flag(filtering(true)),
%	flags:get_bpl_flag(fuzzy_logic(min)),
	!.
    
build_pre_over_lambda_cut_goal(Degree, over_lambdacut(Degree)).


%% build_degree_composition_goal(+DegreeVars, +HeadDegreeVar, -Goal)
%
%     Creates a goal for computing the composition of goal degrees, 
%     and the head degree of the clause.
%      
% build_degree_composition_goal([], 1, true) :-
%   !. 
  
% build_degree_composition_goal([D], D, true) :-
%   !. 
  
build_degree_composition_goal(DegreeVars, HeadDegreeVar, degree_composition(DegreeVars, HeadDegreeVar)).


%% build_post_over_lambda_cut_goal(+Degree, -Goal)
%
%     Creates a goal for a lambda-cut threshold needed  
%     for each t-norm whose composition may deal a degree
%     less than the degrees in the composition.
%      

build_post_over_lambda_cut_goal(_Degree, true) :-
	flags:get_bpl_flag(fuzzy_logic(min)),
	!.
    
build_post_over_lambda_cut_goal(Degree, over_lambdacut(Degree)).


%% link_rule_block_constraint_variables(?HeadConstraintBlockVars, ?BodyConstraintBlockVars, ?HeadCin, ?HeadCout)
%
%     Links head and body block constraints variables through the 
%     unification problems. That is:
%      
%       - Cin of head with Cin of unification problems.
%       - Cout of unification problems with Cin of body.
%       - Cout of body with Cout of head.
%

link_rule_block_constraint_variables(HeadConstraintBlockVars, BodyConstraintBlockVars, HeadCin, HeadCout) :-
	HeadConstraintBlockVars = [HeadCin|_],     % Cin of head with Cin of unification problems
	BodyConstraintBlockVars = [HeadCout|_],    % Cout of unification problems with Cin of body
	!,
	append(_,[HCout],BodyConstraintBlockVars), % Cout of body with 
	append(_,[HCout],HeadConstraintBlockVars). % Cout of head
	
% Nothing to do if the body has no constraint block variables (e.g., calls to built-ins)
link_rule_block_constraint_variables(_HeadConstraintBlockVars, _BodyConstraintBlockVars, _HeadCin, _HeadCout).


%% create_unification_problems(+Vars, +Args, +Degrees, -Problems, ?Cin, ?Cout)
%
%     Returns a list of unification Problems suitable for the
%     unify_arguments_ai/1 predicate. If the selected weak unification algorithm 
%     is 'a1', then each of the returned problems will be
%     a list with an item of each of the three input lists and the input and 
%     output block constraints variables: Vars, Args, and Degrees.
%     Else, the returned problems will be of the form: Vars, Args, Cin, Cout,
%     and Degrees, where the Cout of a unification problem is the Cin of the
%     next one.
%
%     For example, given Vars = [X, Y], Args = [a, b] and
%     Degrees = [D1, D2], 
%     for 'a1' this predicate will return:
%     Problems = [[X, a, D1], [Y, b, D2]], 
%     while for 'a2' and 'a3':
%     Problems = [[X, a, Cin, C1, D1], [Y, b, C1, Cout, D2]].
%     where Cin and Cout are unbound variables.
%

create_unification_problems(Vars, Args, _Cin, _Cout, Degrees, Goals) :-
	flags:get_bpl_flag(weak_unification('a1')),
  !,
  create_unification_a1_problems(Vars, Args, Degrees, Goals).
create_unification_problems(Vars, Args, Cin, Cout, Degrees, Goals) :-
  create_unification_a2_a3_problems(Vars, Args, Cin, Cout, Degrees, Goals).

create_unification_a1_problems([], [], [], []).
create_unification_a1_problems([Var|MoreVars], [Arg|MoreArgs], [Degree|MoreDegrees],
                                [[Var, Arg, Degree]|MoreGoals]) :-
  create_unification_a1_problems(MoreVars, MoreArgs, MoreDegrees, MoreGoals).

create_unification_a2_a3_problems([], [], Cin, Cin, [], []).
create_unification_a2_a3_problems([Var|MoreVars], [Arg|MoreArgs], Cin, Cout, [Degree|MoreDegrees],
                               [[Var, Arg, Cin, Cin1, Degree]|MoreGoals]) :-
  create_unification_a2_a3_problems(MoreVars, MoreArgs, Cin1, Cout, MoreDegrees, MoreGoals).



% %% link_block_constraints_variables(?LeftConstraintBlockVars, ?RightConstraintBlockVars)
% %
% %     Links two lists of block constraints: the last variable of the left
% %     list with the first variable of the right list.
% %

% link_block_constraints_variables(LeftConstraintBlockVars, RightConstraintBlockVars) :-
% 	flags:get_bpl_flag(weak_unification('a3')),
% 	!,
% 	append(_,[Cout],LeftConstraintBlockVars),
% 	RightConstraintBlockVars=[Cout|_].

% link_block_constraints_variables(_LeftConstraintBlockVars, _RightConstraintBlockVars).


%% actual_rule_arity(+ExpandedRuleArity, -RuleArity)
%
%     The arity of the expanded rule (with the degree variable and, 
%     possibly, with the block constraints variables) corresponds to 
%     a predicate without those extended arguments. For the weak 
%     unification algorithm 'a1', expanded rules are added only with
%     the resulting degree variable, whereas for 'a2' and 'a3', expanded 
%     rules contain also both the input and output block constraint 
%     variables. Thus, this predicate computes for a1 RuleArity as 
%     ExpandedRuleArity-1 and, for 'a3', ExpandedRuleArity-3.
%

actual_rule_arity(RuleArity, ActualRuleArity) :-
	flags:get_bpl_flag(weak_unification('a1')),
  !,
  ActualRuleArity is RuleArity - 1. % The last argument is the approximation degree
  
actual_rule_arity(RuleArity, ActualRuleArity) :-
  ActualRuleArity is RuleArity - 3. % The last three arguments are: the input and output block constraint stores, and the approximation degree

%% expanded_rule_arity(+RuleArity, -ExpandedRuleArity)
%
%  The converse predicate to actual_rule_arity/2. In this case,
%  it returns the arity of the translated (expanded) predicate.
%

expanded_rule_arity(RuleArity, ExpandedRuleArity) :-
	flags:get_bpl_flag(weak_unification('a1')),
  !,
  ExpandedRuleArity is RuleArity + 1. % The last argument is the approximation degree
  
expanded_rule_arity(RuleArity, ExpandedRuleArity) :-
  ExpandedRuleArity is RuleArity + 3. % The last three arguments are: the input and output block constraint stores, and the approximation degree

  
  
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Expansion of equations
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%% expand_equations(+EqPrefix, +Equations, -ExpandedEquations)
%
%     Extracts all the equations from the Equations list that match
%     template "EqPrefix(_, _, _)", computes their closure (using the
%     closure properties and t-norm specified in system flags) and
%     returns the resulting equations in the ExpandedEquations list. If
%     EqPrefix is a list, this predicate will be called once for each
%     item and the union of all resulting equations will be returned.
%

expand_equations([], _Equations, []) :-
	!.

expand_equations([EqPrefix|MorePrefixes], Equations, ExpandedEquations) :-
	% Calls this predicate recursively for each prefix
	expand_equations(EqPrefix, Equations, ExpandedEquations1),
	expand_equations(MorePrefixes, Equations, ExpandedEquations2),
	append(ExpandedEquations1, ExpandedEquations2, ExpandedEquations).

expand_equations(EqPrefix, Equations, ExpandedEquations) :-
	% Extract "EqPrefix(_, _, _)" equations from list
	atom(EqPrefix),
	utilities:extract_terms(EqPrefix, 3, Equations, SubEquations),
	% Computes the reflexive, transitive, and/or symmetric closure
	flags:get_bpl_flag(relation_properties(EqPrefix, ClosureProperties)),
	utilities:closure_properties(ClosureProperties, Closure, TNorm),
	flags:get_bpl_flag(lambda_cut(LambdaCut)),
	foreign:ext_closure(SubEquations, Closure, TNorm, EqPrefix, LambdaCut,
	                    ExpandedEquations).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Translation of assertions and retractions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%% translate_asserts_retracts(+Clause, -TranslatedClause, +ProgramPrefix, +Equations)
%
%     Makes a copy of Clause and returns it in TranslatedClause,
%     after expanding all its assert/1 and retract/1 terms. These terms
%     are expanded using ProgramPrefix and the list of Equations in the
%     same way facts and rules are expanded.
%
%     For example, considering that "a ~ b = 0.5", a term like
%     "assert(prog_a(g, D))" would be translated into:
%     "(assert((prog_a(A1, D) :- unify_arguments_a1[[A1, g, D1]]),
%                                true,
%                                min_degree([D1], D)),
%       assert((prog_b(A1, D) :- over_lambdacut(0.5),
%                                unify_arguments_a1[[A1, g, D1]]),
%                                true,
%                                min_degree([D1], D)))"

translate_asserts_retracts(Clause, TranslatedClause, ProgramPrefix, Equations) :-
	% Program prefix and list of equations are saved in dynamic
	% predicates because they're needed by the scanners called
	% by process_term/6
	assert(program_prefix(ProgramPrefix)),
	assert(equations(Equations)),
	utilities:process_term(Clause, TranslatedClause,
	                       [translator:assert_translator_scanner,
	                        translator:retract_translator_scanner],
	                       [], _, _),
	retract(program_prefix(ProgramPrefix)),
	retract(equations(Equations)).


%% assert_translator_scanner(+Term, -Result, +InData, -OutData)
%
%     Scanner that can be used with process_term/6 to "expand" the
%     assert/1 terms in the same way facts and rules are expanded.
%     See translate_asserts_retracts/2 for an example. This scanner
%     ignores InData and OutData.
%
%     @see process_term/6
%     @see translate_asserts_retracts/2
%

assert_translator_scanner(Term, Asserts, _, _) :-
	nonvar(Term),
	Term = assert(Fact),
	program_prefix(ProgramPrefix),
	equations(Equations),
	% Expands the asserted fact and creates a conjunction of
	% assert/1 predicates with each of the returned rules
	expand_rules([[Fact :- true, [_,_], [], []]], RulesToAssert, ProgramPrefix, Equations),
	create_conjunction(RulesToAssert, assert, Asserts).

assert_translator_scanner(Term, Term, _, _).
	% If term isn't assert/1, it's copied as is


%% retract_translator_scanner(+Term, -Result, +InData, -OutData)
%
%     Scanner that can be used with process_term/6 to "expand" the
%     retract/1 terms in the same way facts and rules are expanded.
%     See translate_asserts_retracts/2 for an example. This scanner
%     ignores InData and OutData.
%
%     @see process_term/6
%     @see translate_asserts_retracts/2
%

retract_translator_scanner(Term, Retracts, _, _) :-
	nonvar(Term),
	Term = retract(Fact),
	program_prefix(ProgramPrefix),
	equations(Equations),
	% Expands the retracted fact and creates a conjunction of
	% retract/1 predicates with each of the returned rules
	expand_rules([[Fact :- true, [_,_], [], []]], RulesToRetract, ProgramPrefix, Equations),
	create_conjunction(RulesToRetract, retract, Retracts).

retract_translator_scanner(Term, Term, _, _).
	% If term isn't retract/1, it's copied as is


%% create_conjunction(+Terms, +Atom, -Conjunction)
%
%     Builds a Conjunction of unary predicates using Atom as functor
%     and each of the terms in Terms as arguments.
%
%     For example, given Terms = [a, b, c] and Atom = 'write', this
%     predicate will return Conjunction = [write(a), write(b),
%     write(c)].
%

create_conjunction([], _Atom, true).

create_conjunction([Item|MoreItems], Atom, Terms) :-
	SingleTerm =.. [Atom, Item],
	create_conjunction(MoreItems, Atom, MoreTerms),
	(MoreTerms == true ->
		Terms = SingleTerm
	;
		Terms = (SingleTerm, MoreTerms)
	).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Predicates for handling linguistic terms
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%% translate_fuzzy_sets(+Sets, -Equations)
%
%     Converts a list of fuzzy sets into a binary fuzzy relation
%     defined with proximity/similarity equations, i.e., 'sim/3' terms.
%     Each fuzzy set must be a list consisting of two terms: a domain
%     name and a list of subsets. If Sets is an empty list, all the
%     current fuzzy sets stored in the BPL flags will be translated.
%

%% translate_fuzzy_sets_aux(+Sets, -Equations)
%
%     Internal predicate used by translate_fuzzy_sets/2.
%
%     @see translate_fuzzy_sets/2
%

translate_fuzzy_sets([], Equations) :-
	!,
	findall([DomainName, Subsets],
	        flags:get_bpl_flag(fuzzy_subsets(DomainName, Subsets)), AllFuzzySets),
	translate_fuzzy_sets_aux(AllFuzzySets, Equations).

translate_fuzzy_sets(FuzzySets, Equations) :-
	translate_fuzzy_sets_aux(FuzzySets, Equations).

translate_fuzzy_sets_aux([], []).

translate_fuzzy_sets_aux([[Domain, SubsetList]|MoreFuzzySets], Equations) :-
	% Gets the domain definition and its current subsets
	flags:get_bpl_flag(fuzzy_domain(Domain, [Min, Max, Unit])),
	flags:get_bpl_flag(fuzzy_subsets(Domain, FullSubsetList)),
	% Translates the subsets into a binary fuzzy relation
	foreign:ext_translate_fuzzysets([Domain, Min, Max, Unit], FullSubsetList,
	                                SubsetList, sim, DomainEquations),
	% Scans the remaining fuzzy subsets
	translate_fuzzy_sets_aux(MoreFuzzySets, MoreEquations),
	append(DomainEquations, MoreEquations, Equations).


%% add_linguistic_terms(+LinguisticTerms, -AddedSubsets)
%
%     Translates a list of linguistic terms returned by the parser
%     into a list of fuzzy subsets, and then adds them to the current
%     list of subsets stored in the system flags. AddedSubsets will be
%     unified with the list of new fuzzy subsets that were really added
%     to the flags.
%
%     Linguistic terms built with the '#' operator can be defined in two
%     ways:
%      * 1: [domain, DomainName, SubsetDefinition], if the linguistic
%        term belongs to the domain called DomainName.
%      * 2: [subset, SubsetName, SubsetDefinition], if the linguistic
%        term belongs to the domain in which a subset called SubsetName
%        is declared.
%

add_linguistic_terms(LingTerms, AddedSubsets) :-
	linguistic_terms_to_fuzzy_subsets(LingTerms, Subsets),
	add_fuzzy_subsets(Subsets, AddedSubsets).


%% linguistic_terms_to_fuzzy_subsets(+LingTerms, -Subsets)
%
%     Translates a list of linguistic terms returned by the parser into
%     a list of fuzzy subsets that can be processed by
%     ext_translate_fuzzysets/5. See add_linguistic_terms/2 for the
%     expected syntax of the LingTerms list.
%
%     @see add_linguistic_terms/2
%

linguistic_terms_to_fuzzy_subsets([], []).

linguistic_terms_to_fuzzy_subsets([[domain, DomainName, SubsetDefinition]|MoreLingTerms], Subsets) :-
	% Ensures that the specified domain exists
	flags:get_bpl_flag(fuzzy_domain(DomainName, [_Min, _Max, _Unit])), !,
	% Converts the remaining linguistic terms
	linguistic_terms_to_fuzzy_subsets(MoreLingTerms, MoreSubsets),
	append([[DomainName, [SubsetDefinition]]], MoreSubsets, Subsets).

linguistic_terms_to_fuzzy_subsets([[subset, SubsetName, SubsetDefinition]|MoreLingTerms], Subsets) :-
	% Looks for the domain in which a subset called SubsetName is declared
	flags:get_bpl_flag(fuzzy_subsets(DomainName, DomainSubsets)),
	member(WantedSubset, DomainSubsets),
	WantedSubset =.. [SubsetName|_Value],
	% Calls the previous rule replacing subset/SubsetName with domain/DomainName
	linguistic_terms_to_fuzzy_subsets([[domain, DomainName, SubsetDefinition]], NewSubset),
	% Converts the remaining linguistic terms
	linguistic_terms_to_fuzzy_subsets(MoreLingTerms, MoreSubsets),
	append(NewSubset, MoreSubsets, Subsets).

linguistic_terms_to_fuzzy_subsets([_BadLingTerm|MoreLingTerms], Subsets) :-
	% This rule is only executed if a linguistic term has an unknown
	% syntax or doesn't belong to any existing domain or fuzzy set
	linguistic_terms_to_fuzzy_subsets(MoreLingTerms, Subsets).


%% add_fuzzy_subsets(+Subsets, -AddedSubsets)
%
%     Adds a list of Subsets to the current list of subsets stored in
%     the BPL flags, and returns a list with the subsets that were not
%     already in them. Each subset must be defined with a list
%     following this syntax: [Domain, [SubsetDefinition]] (note that this
%     is the same syntax used by the fuzzy_set/2 BPL directive).
%

add_fuzzy_subsets([], []).

add_fuzzy_subsets([[Domain, [SubsetDefinition]]|MoreSubsets], AddedSubsets) :-
	% Checks if this subset is already defined
	flags:get_bpl_flag(fuzzy_subsets(Domain, DomainSubsets)),
	(member(SubsetDefinition, DomainSubsets) ->
		% Subset is alredy defined
		true,
		Added = []
	;
		% Adds the new subset to the lists of subsets of the domain
		append(DomainSubsets, [SubsetDefinition], NewDomainSubsets),
		Added = [[Domain, [SubsetDefinition]]],
		% Updates the fuzzy subsets of the domain
		flags:remove_bpl_flag(fuzzy_subsets(Domain, DomainSubsets)),
		flags:add_bpl_flag(fuzzy_subsets(Domain, NewDomainSubsets))
	),
	% Adds the remaining fuzzy subsets
	add_fuzzy_subsets(MoreSubsets, MoreAdded),
	append(Added, MoreAdded, AddedSubsets).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Auxiliary dynamic predicates
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%% program_prefix(?Prefix)                                   is dynamic
%
%     Auxiliary dynamic predicate used by assert_translator_scanner/1
%     and retract_translator_scanner/1 that store the prefix that must
%     be appended to the predicate names.
%

:- dynamic program_prefix/1.


%% equations(?Equations)                                     is dynamic
%
%     Auxiliary dynamic predicate used by assert_translator_scanner/1
%     and retract_translator_scanner/1 that store the list of equations
%     of the main proximity/similarity relation.
%

:- dynamic equations/1.



