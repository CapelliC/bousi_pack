%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Tests for predicates which find all solutions to a goal

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%
% List of test suites
%

test_suites([test_findall, test_bagof, test_setof]).


%
% findall/3 test
% - ISO -
%

test_findall_1(X, S) :- findall(X, (X = 1 ; X = 2), S).
test_findall_2(X, Y, S) :- findall(X + Y, (X = 1), S).
test_findall_3(X, L) :- findall(X, fail, L).
test_findall_4(X, S) :- findall(X, (X = 1 ; X = 1), S).
test_findall_5(X) :- findall(X, (X = 2 ; X = 1), [1, 2]).
test_findall_6(X, Goal, S) :- findall(X, Goal, S).
test_findall_7(X, S) :- findall(X, 4, S).

throws_exception(test_findall_6).
throws_exception(test_findall_7).


%
% bagof/3 test
% - ISO -
%

db_bagof_a(1, f(_)).
db_bagof_a(2, f(_)).

db_bagof_b(1, 1).
db_bagof_b(1, 1).
db_bagof_b(1, 2).
db_bagof_b(2, 1).
db_bagof_b(2, 2).
db_bagof_b(2, 2).

test_bagof_1(X, S) :- bagof(X, (X = 1 ; X = 2), S).
test_bagof_2(X) :- bagof(X, (X = 1 ; X = 2), X).
test_bagof_3(X, S) :- bagof(X, fail, S).
test_bagof_4(Y, L) :- bagof(1, (Y = 1 ; Y = 2), L).
test_bagof_5(X, Y, L) :- bagof(f(X, Y), (X = a ; Y = b), L).
test_bagof_6(X, Y, S) :- bagof(X, Y ^ ((X = 1, Y = 1) ; (X = 2, Y = 2)), S).
test_bagof_7(X, Y, S) :- bagof(X, Y ^ ((X = 1 ; Y = 1) ; (X = 2 ; Y = 2)), S).
test_bagof_8(X, Y, Z, S) :- bagof(X, (X = Y ; X = Z ; Y = 1), S).
test_bagof_9(X, Y, Z, S) :- bagof(X, (X = Y ; X = Z), S).
test_bagof_10(X, Y, L) :- bagof(X, db_bagof_a(X, Y), L).
test_bagof_11(X, Y, L) :- bagof(X, db_bagof_b(X, Y), L).
test_bagof_12(X, Y, Z, L) :- bagof(X, Y ^ Z, L).
test_bagof_13(X, L) :- bagof(X, 1, L).

throws_exception(test_bagof_12).
throws_exception(test_bagof_13).

% The following test isn't included because it may succeed or throw an
% exception depending on the 'unknown' (SWI-Prolog) or 'undefined_predicate'
% (ISO) system flag
%
%test_bagof_X1(X, Y, S) :- bagof(X, (Y ^ (X = 1 ; Y = 2) ; X = 3), S).


%
% setof/3 test
% - ISO -
%

db_setof_a(1, f(_)).
db_setof_a(2, f(_)).

db_setof_b(1, 1).
db_setof_b(1, 1).
db_setof_b(1, 2).
db_setof_b(2, 1).
db_setof_b(2, 2).
db_setof_b(2, 2).

db_setof_d(1, 1).
db_setof_d(1, 2).
db_setof_d(1, 1).
db_setof_d(2, 2).
db_setof_d(2, 1).
db_setof_d(2, 2).

test_setof_1(X, S) :- setof(X, (X = 1 ; X = 2), S).
test_setof_2(X) :- setof(X, (X = 1 ; X = 2), X).
test_setof_3(X, S) :- setof(X, (X = 2 ; X = 1), S).
test_setof_4(X, S) :- setof(X, (X = 2 ; X = 2), S).
test_setof_5(X, Y, Z, S) :- setof(X, (X = Y ; X = Z), S).
test_setof_6(X, S) :- setof(X, fail, S).
test_setof_7(Y, L) :- setof(1, (Y = 2 ; Y = 1), L).
test_setof_8(X, Y, L) :- setof(f(X, Y), (X = a ; Y = b), L).
test_setof_9(X, Y, S) :- setof(X, Y ^ ((X = 1, Y = 1) ; (X = 2, Y = 2)), S).
test_setof_10(X, Y, S) :- setof(X, Y ^ ((X = 1 ; Y = 1) ; (X = 2 ; Y = 2)), S).
test_setof_11(X, Z, S) :- setof(X, (X = Y ; X = Z; Y = 1), S).
test_setof_12(X, Y, L) :- setof(X, db_setof_a(X, Y), L).
test_setof_13(X, U, V, L) :- setof(X, member(X, [f(U, b), f(V, c)]), L).
test_setof_14(X, U, V) :- setof(X, member(X, [f(U, b), f(V, c)]),
                                [f(a, c), f(a, b)]). % Implementation dependent
test_setof_15(X, U, V) :- setof(X, member(X, [f(b, U), f(c, V)]), [f(b, a), f(c, a)]).
test_setof_16(X, V, U, L) :- setof(X, member(X, [V, U, f(U), f(V)]), L).
test_setof_17(X, V, U) :- setof(X, member(X, [V, U, f(U), f(V)]),
                                [a, b, f(a), f(b)]). % Implementation dependent
test_setof_18(X, V, U) :- setof(X, member(X, [V, U, f(U), f(V)]), [a, b, f(b), f(a)]).
test_setof_19(X, U, V) :- setof(X, (exists(U, V) ^ member(X, [V, U, f(U), f(V)])),
                                [a, b, f(b), f(a)]).
test_setof_20(X, Y, L) :- setof(X, db_setof_b(X, Y), L).
test_setof_21(X, Xs, Y, L) :- setof(X-Xs, Y ^ setof(Y, db_setof_b(X, Y), Xs), L).
test_setof_22(X, Xs, Y, L) :- setof(X-Xs, setof(Y, db_setof_b(X, Y), Xs), L).
test_setof_23(X, Xs, Y, L) :- setof(X-Xs, bagof(Y, db_setof_d(X, Y), Xs), L).

% The following test isn't included because it may succeed or throw an
% exception depending on the 'unknown' (SWI-Prolog) or 'undefined_predicate'
% (ISO) system flag
%
%test_setof_X1(X, Y, S) :- setof(X, (Y ^ (X = 1 ; Y = 2) ; X = 3), S).

