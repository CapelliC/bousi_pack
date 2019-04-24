%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Tests for exception handling predicates

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%
% List of test suites
%

test_suites([test_throw]).


%
% throw/1 and catch/3 tests
% - ISO -
%

db_throw_foo(X) :- Y is X * 2, throw(test(Y)).
db_throw_bar(X) :- X = Y, throw(Y).
db_throw_coo(X) :- throw(X).
db_throw_car(X) :- X = 1, throw(X).

test_throw_1(Y) :- catch(db_throw_foo(5), test(Y), true).
test_throw_2(Z) :- catch(db_throw_bar(3), Z, true).
test_throw_3(C) :- catch(true, C, write(demoen)), throw(bla).
test_throw_4(X, Y) :- catch(db_throw_coo(X), Y, true).
test_throw_5(X, Y) :- catch(db_throw_car(X), Y, true).

throws_exception(test_throw_3).

