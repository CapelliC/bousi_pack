%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Tests for predicates which check the type of a term

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%
% List of test suites
%

test_suites([test_var, test_atom, test_integer, test_floattype,
             test_atomic, test_compound, test_nonvar, test_number]).


%
% var/1 test
% - ISO -
%

test_var_1 :- var(foo).
test_var_2(Foo) :- var(Foo).
test_var_3(Foo) :- foo = Foo, var(Foo).
test_var_4 :- var(_).


%
% atom/1 test
% - ISO -
%

test_atom_1 :- atom(atom).
test_atom_2 :- atom('string').
test_atom_3 :- atom(a(b)).
test_atom_4(Var) :- atom(Var).
test_atom_5 :- atom([]).
test_atom_6 :- atom(6).
test_atom_7 :- atom(3.3).


%
% integer/1 test
% - ISO -
%

test_integer_1 :- integer(3).
test_integer_2 :- integer(-3).
test_integer_3 :- integer(3.3).
test_integer_4(X) :- integer(X).
test_integer_5 :- integer(atom).


%
% float/1 test (called real/1 in 2nd draft of ISO Prolog Standard)
% - ISO -
%

test_floattype_1 :- float(3.3).
test_floattype_2 :- float(-3.3).
test_floattype_3 :- float(3).
test_floattype_4 :- float(atom).
test_floattype_5(X) :- float(X).


%
% atomic/1 test
% - ISO -
%

test_atomic_1 :- atomic(atom).
test_atomic_2 :- atomic(a(b)).
test_atomic_3(Var) :- atomic(Var).
test_atomic_4 :- atomic(6).
test_atomic_5 :- atomic(3.3).


%
% compound/1 test
% - ISO -
%

test_compound_1 :- compound(33.3).
test_compound_2 :- compound(-33.3).
test_compound_3 :- compound(-a).
test_compound_4 :- compound(_).
test_compound_5 :- compound(a).
test_compound_6 :- compound(a(b)).
test_compound_7 :- compound([a]).


%
% nonvar/1 test
% - ISO -
%

test_nonvar_1 :- nonvar(33.3).
test_nonvar_2 :- nonvar(foo).
test_nonvar_3(Foo) :- nonvar(Foo).
test_nonvar_4(Foo) :- foo = Foo, nonvar(Foo).
test_nonvar_5 :- nonvar(_).
test_nonvar_6 :- nonvar(a(b)).


%
% number/1 test
% - ISO -
%

test_number_1 :- number(3).
test_number_2 :- number(3.3).
test_number_3 :- number(-3).
test_number_4 :- number(a).
test_number_5(X) :- number(X).

