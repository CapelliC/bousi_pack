%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Tests for predicates which manipulate the clause database

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%
% List of test suites
%

test_suites([test_assert, test_retract]).


%
% assert/1 test (equivalent to assertz/1)
% - ISO -
%

% Note that these tests have additional code in order to keep the
% original state of the clause database after executing them

:- dynamic db_assert_legs/2.

db_assert_legs(A, 6) :- db_assert_insect(A).

:- dynamic db_assert_insect/1.

db_assert_insect(ant).
db_assert_insect(bee).

:- dynamic db_assert_foo/1.

db_assert_foo(X) :- call(X), call(X).

test_assert_1 :- assert(db_assert_legs(spider, 8)), db_assert_legs(spider, 8),
                 retract(db_assert_legs(spider, 8)).
test_assert_2 :- assert(_).
test_assert_3 :- assert(4).

throws_exception(test_assert_2).
throws_exception(test_assert_3).

% The following tests don't work in Bousi-Prolog because assert/1
% can't be used to assert new rules into the clause database
%
%test_assert_X1(B) :- assert((db_assert_legs(B, 2) :- db_assert_bird(B))).
%test_assert_X2(X) :- assert((db_assert_foo(X) :- X -> call(X))).
%test_assert_X3 :- assert((db_assert_foo :- 4)).
%test_assert_X4 :- assert((atom(_) :- true)).
%
%throws_exception(test_assert_X3).
%throws_exception(test_assert_X4).


%
% retract/1 test
% - ISO -
%

% Note that these tests have additional code in order to keep the
% original state of the clause database after executing them

:- dynamic db_retract_legs/2.

db_retract_legs(A, 4) :- db_retract_animal(A).
db_retract_legs(octopus, 8).
db_retract_legs(A, 6) :- db_retract_insect(A).
db_retract_legs(spider, 8).
db_retract_legs(B, 2) :- db_retract_bird(B).

:- dynamic db_retract_insect/1.

db_retract_insect(ant).
db_retract_insect(bee).

:- dynamic db_retract_foo/1.

db_retract_foo(X) :- call(X), call(X).
db_retract_foo(X) :- call(X) -> call(X).

test_retract_1 :- retract(db_retract_legs(octopus, 8)),
                  not(db_retract_legs(octopus, 8)),
                  assert(db_retract_legs(octopus, 8)).
test_retract_2 :- retract(db_retract_legs(spider, 6)),
                  not(db_retract_legs(spider, 6)),
                  assert(db_retract_legs(spider, 6)).
test_retract_3 :- subtest_retract_3a ; subtest_retract_3b.
subtest_retract_3a :- retract(db_retract_insect(I)), write(I),
                      retract(db_retract_insect(bee)), fail.
subtest_retract_3b :- assert(db_retract_insect(ant)),
                      assert(db_retract_insect(bee)).

% The following tests don't work in Bousi-Prolog because retract/1
% can't be used to retract rules from the clause database
%
%test_retract_X1(X, T) :- retract((db_retract_legs(X, 2) :- T)).
%test_retract_X2(X, Y) :- retract((db_retract_legs(X, Y) :- Z)).
%test_retract_X3(A) :- retract((db_retract_foo(A) :- A, call(A))).
%test_retract_X4(A, B) :- retract((db_retract_foo(A) :- A -> B)).
%test_retract_X5(X, Y) :- retract((X :- in_eec(Y))).
%test_retract_X6(X) :- retract((4 :- X)).
%test_retract_X7(X) :- retract((atom(X) :- X == '[]')).
%
%throws_exception(test_retract_X5).
%throws_exception(test_retract_X6).
%throws_exception(test_retract_X7).

