%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% WN_CONNECT v1.3 : wn_gen_prox_equations
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
/*
AUTHORS: Pascual Julia´n-Iranzo (Universidad de Castilla-La Mancha, Spain)
Fernando Sa´enz-Pe´rez  (Universidad Complutense de Madrid, Spain)

WN_CONNECT is licensed for research and educational purposes only and it is
distributed with NO WARRANTY OF ANY KIND. You are freely allowed to use, copy
and distribute WN_CONNECT provided that you make no modifications to any of its
files and give credit to its original authors.
*******************************************************************************/
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Generation of proximity equations based on WordNet

:- module(wn_gen_prox_equations, [
  wn_gen_ontology_file/3,       % +ListOfListOfWords, +File, +Measure
	wn_gen_prox_equations_list/3, % +ListOfListOfWords, +Measure, -Equations
	wn_auto_gen_prox_equations/4  % +Directives, +Rules, -InEquations, -OutEquations
	]
	).

:- use_module(wn_sim_measures).
:- use_module(wn_utilities).
%%:- use_module(utilities).
%:- use_module(library(ordsets)).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% wn_gen_ontology_file(+ListOfListOfWords, +File, +Measure)
%%% Given a list of list of words, ListOfListOfWords, the name of a file, File,
%%% and the acronym of a measure, Measure (by now [path, wup, lch, res, lin, jcn,
%%% yarm]), it generates a set of proximity equations and stored them into the file File.
%%%
wn_gen_ontology_file(ListOfListOfWords, File, Measure) :-
    file_name_extension(_, Extension, File),
    ((Extension = ont) -> File_ont = File;
     (Extension = '')  -> file_name_extension(File, ont, File_ont)
    ),
    (exists_file(File_ont) ->
        write('The file '), write(File), write(' or '), write(File_ont), write(' does exists.'), nl
        ;
        (member(Measure, [path, wup, lch, res, lin, jcn, hso, lesk, vector, yar]) ->
            wn_gen_prox_equations_list(ListOfListOfWords, Measure, Equations),
            open(File_ont, write, OutputStream),
            write(OutputStream,'%% PROXIMITY EQUATIONS'), nl(OutputStream),
            write_equations(Equations, OutputStream),
            close(OutputStream)
            ;
            write(Measure), write(' is not a similarity or relatedness measure.'), nl
        )
    ).

%%% write_equations(+Equations, +OutputStream)
%%%
write_equations([], _).
write_equations([sim(W1,W2,D)|Equations], OutputStream) :-
            concat_atom([W1,'~', W2, '=', D, '.'], ProxEqu),
            write(OutputStream,ProxEqu), nl(OutputStream),
            write_equations(Equations, OutputStream).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% wn_auto_gen_prox_equations(+Directives, +Rules, -InEquations, -OutEquations)
%%%
%%% If Directives is [:- directive(wn_gen_prox_equations, [Measure, Auto])], then 
%%% return in OutEquations the equations in InEquations plus all the proximity 
%%% equations derived from the following:
%%% - Extract three sets from Rules: constant, functor and predicate identifiers, 
%%% - For each word W1 in a set compare it to any other word W2 in the same set to 
%%%   determine their relatedness degree D, and generate a proximity equation 
%%%   sim(W1, W2, D) in OutEquations.
%%% Otherwise, just return InEquations in OutEquations
%%%

wn_auto_gen_prox_equations([:- directive(wn_gen_prox_equations, [Measure, Auto])], Rules, InEquations, OutEquations) :-
  \+ is_list(Auto),
  atoms_functors_in_term(Rules, AllAtoms, AllFunctors),
  exception_words(ExceptionWords),
  ordsets:ord_subtract(AllAtoms, ExceptionWords, Atoms),
  ordsets:ord_subtract(AllFunctors, ExceptionWords, AllValidFunctors),
  bpl_predicates(AllValidFunctors, Functors, Predicates),
  !,
  wn_gen_prox_equations_list([Atoms, Functors, Predicates], Measure, NewEquations),
  lists:append(InEquations, NewEquations, OutEquations).
  
wn_auto_gen_prox_equations(_Directives, _Rules, Equations, Equations).


% List of words that are not to be related with WordNet words. 
% This must be an ordered set
exception_words([
  ':-',
  true   % true is the body of a fact, it is dismissed
  ]). 



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% wn_gen_prox_equations_list(+ListOfListOfWords, +Measure, -Equations)
%%% Given a ListOfListOfWords computes all proximity equations that can be formed paring
%%% the words of each list between them and then computing their proximity degree using
%%% the measure Measure.
%%%
%%% NOTES: Each list of ListOfListOfWords must be compounded by words of the same part of
%%%        speech (either nouns, verbs or adjectives)
%%%
%%%        "sim(Word1, Word2, Degree)" is the internal Bousi~Prolog representation of a
%%%        proximity equation "Word1 ~ Word2 = Degree" (i.e., Word1 is close to Word2 with
%%%        approximation degree Degree).
%%%

wn_gen_prox_equations_list(ListOfListOfWords, Measure, Equations) :-
    wn_gen_prox_equations_list(ListOfListOfWords, Measure, Equations, []).

%%%
%%% wn_gen_prox_equations_list/4 is implemented by a set of Definite Clause Grammar
%%% rules. Grammar rules are expanded automatically into Prolog clauses with two extra
%%% arguments added as the two arguments of the predicate in order to represent the
%%% input (or output) tokens as a difference list.
%%%
%%%

wn_gen_prox_equations_list([], _Measure) -->
  [].
  
wn_gen_prox_equations_list([ListOfWords|ListOfListOfWords], Measure) -->
  wn_gen_prox_equations(ListOfWords, Measure), 
  wn_gen_prox_equations_list(ListOfListOfWords, Measure).

  
wn_gen_prox_equations([], _Measure) -->
  [].
  
wn_gen_prox_equations([Word|ListOfWords], Measure) -->
  wn_gen_prox_equations(ListOfWords, Word, Measure), 
  wn_gen_prox_equations(ListOfWords, Measure).

  
wn_gen_prox_equations([], _Word1, _Measure) -->
  [].
  
wn_gen_prox_equations([Word2|ListOfWords], Word1, Measure) -->
  {gen_prox_equation(Measure, Word1, Word2, Equation),
   !},
  [Equation],
  wn_gen_prox_equations(ListOfWords, Word1, Measure).
  
wn_gen_prox_equations([_Word2|ListOfWords], Word1, Measure) --> % Word1 and Word2 are not related
  wn_gen_prox_equations(ListOfWords, Word1, Measure).

 
%%% gen_prox_equation(+Measure, +Pattern1, +Pattern2, -Equation)
%%%
%%%   Return the Equation for the given Measure and words. 
%%%   Words come expressed as the term Word:Type:SenseNumber

gen_prox_equation(Measure, Word1:Type1:Sense1, Word2:Type2:Sense2, Equation) :-
  atom(Word1),
  atom(Word2),
  valid_word_type(Type1),
  valid_word_type(Type2),
  number(Sense1),
  number(Sense2),
  !,
  gen_prox_equation_aux(Measure, Word1:Type1:Sense1, Word2:Type2:Sense2, Equation).
  
gen_prox_equation(Measure, Word1, Word2, Equation) :-
  atom(Word1),
  atom(Word2),
  !,
  gen_prox_equation_aux(Measure, Word1:Type:1, Word2:Type:1, Equation).
  
gen_prox_equation(_Measure, Word1, Word2, _Equation) :-
  format('ERROR: Incorrect pattern for ~p and/or ~p. Expected either plain words or patterns Word:Type:Sense, where Type is in {n,v} and Sense is a number.', [Word1, Word2]),
  fail.  
  
 
gen_prox_equation_aux(Measure, Pattern1, Pattern2, sim(Word1, Word2, NormalizedDegree)) :-
   wn_measure_module_goal(Measure, Module, MeasureGoalName),
   MeasureGoal =.. [MeasureGoalName, Pattern1, Pattern2, Degree],
   Module:MeasureGoal,
   measure_max_value(Measure, Max),
   NormalizedDegree is Degree/Max,
   Pattern1 = Word1:_:_,
   Pattern2 = Word2:_:_.
   

%%% valid_word_type(+Type).
%%%   Valid word types. Currently, only nouns (n) and verbs (v)
   
valid_word_type(n).

valid_word_type(v).


%%% wn_measure_module_goal(?Measure, ?Module, ?MeasureGoalName)
%%% This predicate stores a list of parameters:
%%% Measure: it is the name of the measure used to compute the similarity or
%%% relatedness of two ListOfWords.
%%% Module: it is the module where is implemented the corresponding measure.
%%% MeasureGoalName: it is the name of the predicate that implements the measure.
%%%

wn_measure_module_goal(path, wn_sim_measures, wn_path).
wn_measure_module_goal(wup,  wn_sim_measures, wn_wup).
wn_measure_module_goal(lch,  wn_sim_measures, wn_lch).
wn_measure_module_goal(res,  wn_ic_measures,  wn_res).
wn_measure_module_goal(jcn,  wn_ic_measures,  wn_jcn).
wn_measure_module_goal(lin,  wn_ic_measures,  wn_lin).
wn_measure_module_goal(yarm, wn_rel_measures, wn_yarm).


%%% measure_max_value(?Measure, ?Value)
%%% Measure: Name of the measure (path, lch, ...).
%%% Value: Maximum value the measure can take.
%%%

measure_max_value(path, 1).
measure_max_value(wup, 1).
measure_max_value(lch, 3.6888794541139363).
measure_max_value(res, 1) :- % WARNING: Check this value!
  nl, write('WARNING: Normalization is not checked.'), nl.
measure_max_value(jcn, 1) :- % WARNING: Check this value!
  nl, write('WARNING: Normalization is not checked.'), nl.
measure_max_value(lin, 1).
%measure_max_value(lesk,  ???).
measure_max_value(hso, 16).
measure_max_value(yarm, 1).

