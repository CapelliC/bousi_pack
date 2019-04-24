%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Tests for control predicates

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%
% List of test suites
%

test_suites([test_true, test_fail, test_cut, test_conj, test_disj,
             test_ifthen, test_ifthenelse]).


%
% true/0 test
% - ISO -
%

test_true_1 :- true.


%
% fail/0 test
% - ISO -
%

test_fail_1 :- fail.


%
% !/0 test
% - ISO -
%

db_cut_twice(!) :- write('C ').
db_cut_twice(true) :- write('Moss ').

test_cut_1 :- !.
test_cut_2 :- (!, fail ; true).
test_cut_3 :- (call(!), fail ; true).
test_cut_4 :- db_cut_twice(_), !, write('Forwards '), fail.
test_cut_5 :- (! ; write('No ')), write('Cut disjunction '), fail.
test_cut_6 :- db_cut_twice(_), (write('No ') ; !), write('Cut '), fail.
test_cut_7 :- db_cut_twice(_), (!, fail ; write('No ')).
test_cut_8(X) :- db_cut_twice(X), X, write('Forwards '), fail.
test_cut_9 :- db_cut_twice(_), not(not(!)), write('Forwards '), fail.
test_cut_10 :- db_cut_twice(_), once(!), write('Forwards '), fail.
test_cut_11 :- db_cut_twice(_), call(!), write('Forwards '), fail.

% The following test doesn't work in Bousi-Prolog because call(X) calls
% can only have a single term in X
%
%db_cut_goal((db_cut_twice(_), !)).
%db_cut_goal(write('Three ')).
%
%test_cut_X1(X) :- db_cut_goal(X), call(X), write('Forwards '), fail.


%
% ,/2 test
% - ISO -
%

test_conj_1(X) :- ','(X = 1, var(X)).
test_conj_2(X) :- ','(var(X), X = 1).
test_conj_3(X) :- ','(X = true, call(X)).

test_conj_1b(X) :- X = 1, var(X).
test_conj_2b(X) :- var(X), X = 1.
test_conj_3b(X) :- X = true, call(X).


%
% ;/2 test
% - ISO -
%

test_disj_1 :- ';'(true, fail).
test_disj_2 :- ';'((!, fail), true).
test_disj_3 :- ';'(!, call(3)).
test_disj_4(X) :- ';'((X = 1, !), X = 2).

test_disj_1b :- true ; fail.
test_disj_2b :- (!, fail) ; true.
test_disj_3b :- ! ; call(3).
test_disj_4b(X) :- (X = 1, !) ; X = 2.


%
% ->/2 test
% - ISO -
%

test_ifthen_1 :- '->'(true, true).
test_ifthen_2 :- '->'(true, fail).
test_ifthen_3 :- '->'(fail, true).
test_ifthen_4(X) :- '->'(true, X = 1).
test_ifthen_5(X) :- '->'(';'(X = 1, X = 2), true).
test_ifthen_6(X) :- '->'(true, ';'(X = 1, X = 2)).

test_ifthen_1b :- true -> true.
test_ifthen_2b :- true -> fail.
test_ifthen_3b :- fail -> true.
test_ifthen_4b(X) :- true -> X = 1.
test_ifthen_5b(X) :- (X = 1 ; X = 2) -> true.
test_ifthen_6b(X) :- true -> (X = 1 ; X = 2).


%
% ->/2 and ;/2 test
% - ISO -
%

test_ifthenelse_1 :- ';'('->'(true, true), fail).
test_ifthenelse_2 :- ';'('->'(fail, true), true).
test_ifthenelse_3 :- ';'('->'(true, fail), fail).
test_ifthenelse_4 :- ';'('->'(fail, true), fail).
test_ifthenelse_5(X) :- ';'('->'(true, X = 1), X = 2).
test_ifthenelse_6(X) :- ';'('->'(fail, X = 1), X = 2).
test_ifthenelse_7(X) :- ';'('->'(';'(X = 1, X = 2), true), true).

test_ifthenelse_1b :- true -> true ; fail.
test_ifthenelse_2b :- fail -> true ; true.
test_ifthenelse_3b :- true -> fail ; fail.
test_ifthenelse_4b :- fail -> true ; fail.
test_ifthenelse_5b(X) :- true -> X = 1 ; X = 2.
test_ifthenelse_6b(X) :- fail -> X = 1 ; X = 2.
test_ifthenelse_7b(X) :- (X = 1 ; X = 2) -> true ; true.

