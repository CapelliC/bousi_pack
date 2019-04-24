%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Tests for other higher-order predicates

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%
% List of test suites
%

test_suites([test_call, test_failif, test_not, test_once, test_ignore,
             test_timepred, test_maplist, test_apply, test_forall]).


%
% call/1 test
% - ISO -
%

db_call_b(X) :-
	Y = (write(X), call(X)),  % BPL needs 'call' in 'call(X)'
	call(Y).

db_call_a(1).
db_call_a(2).

test_call_1 :- call(!).
test_call_2 :- call(fail).
test_call_3(X) :- call((fail, call(X))). % BPL needs 'call' in 'call(X)'
test_call_4 :- call((fail, call(1))).
test_call_5 :- db_call_b(3).
test_call_6(Z, X) :- (Z = !, call((Z = !, db_call_a(X), Z))).
test_call_7(Z ,X) :- call((Z = !, db_call_a(X), Z)).
test_call_8(X) :- call((write(3), X)).
test_call_9 :- call((write(3), call(1))).
test_call_10(X) :- call(X).
test_call_11 :- call(1).
test_call_12 :- call((fail, 1)).
test_call_13 :- call((write(3), 1)).
test_call_14 :- call((1 ; true)).

throws_exception(test_call_5).
throws_exception(test_call_8).
throws_exception(test_call_9).
throws_exception(test_call_10).
throws_exception(test_call_11).
throws_exception(test_call_12).
throws_exception(test_call_13).
throws_exception(test_call_14).

% This test works perfectly in Bousi-Prolog but it can't be checked because a
% different variable name is written when it's run on Prolog and Bousi-Prolog
%test_call_X1 :- db_call_b(_).


%
% \+/1 test (called fail_if/1 in 2nd draft of ISO Prolog Standard)
% - ISO -
%

test_failif_1 :- \+(true).
test_failif_2 :- \+(!).
test_failif_3 :- \+((!, fail)).
test_failif_4(X) :- (X = 1 ; X = 2), \+((!, fail)).
test_failif_5 :- \+(4 = 5).
%test_failif_6 :- \+(3).
test_failif_7(X) :- \+(X).
test_failif_8(X) :- \+(X = f(X)). % Undefined behavior

throws_exception(test_failif_6).
throws_exception(test_failif_7).


%
% not/1 test (equivalent to \+/1)
% - non-ISO -
%

test_not_1 :- not(true).
test_not_2 :- not(!).
test_not_3 :- not((!, fail)).
test_not_4(X) :- (X = 1 ; X = 2), not((!, fail)).
test_not_5 :- not(4 = 5).
test_not_6 :- not(3).
test_not_7(X) :- not(X).
test_not_8(X) :- not(X = f(X)). % Undefined behavior

throws_exception(test_not_6).
throws_exception(test_not_7).


%
% once/1 test
% - ISO -
%

test_once_1 :- once(!).
test_once_2(X) :- once(!), (X = 1; X = 2).
test_once_3 :- once(repeat).
test_once_4 :- once(fail).
test_once_5(X) :- once(X = f(X)). % Undefined behavior


%
% ignore/1 test
% - non-ISO -
%
 
test_ignore_1 :- once(!).
test_ignore_2(X) :- once(!), (X = 1; X = 2).
test_ignore_3 :- once(repeat).
test_ignore_4 :- once(fail).
test_ignore_5(X) :- once(X = f(X)).


%
% time/1 test
% - non-ISO -
%

test_timepred_1 :- time(!). 
test_timepred_2 :- time(write(foo)).
test_timepred_3 :- time(time((write(foo), write(foo)))).


%
% maplist/2 test
% - non-ISO -
%

db_maplist_sample(a).
db_maplist_sample(b).
db_maplist_sample(c).

test_maplist_1 :- maplist(write, [a, b, c]).
test_maplist_2 :- maplist(write, [a, b, c, d, e, f]).
test_maplist_3 :- maplist(db_maplist_sample, [a, b, c]).
test_maplist_4 :- maplist(db_maplist_sample, [a, b, c, d, e, f]).
test_maplist_5 :- maplist(_, [a, b]).

throws_exception(test_maplist_5).


%
% apply/2 test
% - non-ISO -
%

db_apply_numbers(1, 5).
db_apply_numbers(5, 1).
db_apply_numbers(0, 0).
db_apply_true.

test_apply_1 :- apply(write, [foo]).
test_apply_2 :- apply(<, [5, 9]).
test_apply_3 :- apply(db_apply_true, []).
test_apply_4(X, Y) :- apply(db_apply_numbers, [X, Y]), X =:= Y.
test_apply_5(X) :- apply(X, [foo]).
test_apply_6 :- apply(db_apply_true, _).

throws_exception(test_apply_5).
throws_exception(test_apply_6).


%
% forall/2 test
% - non-ISO -
%

db_forall_object(table).
db_forall_object(chair).
db_forall_object(lamp).

test_forall_1 :- forall(true, write(foo)).
test_forall_2 :- forall(fail, write(foo)).
test_forall_3 :- forall(X is 5 + 9, write(X)).
test_forall_4 :- forall(member(X, [a, b, c]), write(X)).
test_forall_5 :- forall(member(X, [a, b, c]), (X == a ; write(X))).
test_forall_6 :- forall(member(X, [5, 10, -6]), X > 2).
test_forall_7(L) :- setof(X, db_forall_object(X), L),
                    forall(member(Y, L), call(db_forall_object(Y))).
test_forall_8 :- forall(_, true).
test_forall_9 :- forall(true, _).

throws_exception(test_forall_8).
throws_exception(test_forall_9).

