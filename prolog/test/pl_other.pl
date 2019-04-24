%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Tests for other predicates

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%
% List of test suites
%

test_suites([test_throw_catch, test_new_op, test_current_op]).


%
% throw/1 and catch/3 tests
% - ISO -
%

db_throw_foo(X) :- Y is X * 2, throw(test(Y)).
db_throw_bar(X) :- X = Y, throw(Y).
db_throw_coo(X) :- throw(X).
db_throw_car(X) :- X = 1, throw(X).

test_throw_catch_1(Y) :- catch(db_throw_foo(5), test(Y), true).
test_throw_catch_2(Z) :- catch(db_throw_bar(3), Z, true).
test_throw_catch_3(C) :- catch(true, C, write(demoen)), throw(bla).
test_throw_catch_4(X, Y) :- catch(db_throw_coo(X), Y, true).
test_throw_catch_5(X, Y) :- catch(db_throw_car(X), Y, true).

throws_exception(test_throw_catch_3).


%
% op/3 test
% - ISO -
%

% This test suite includes an extra test that uses a custom operator
% declared with an op/3 directive

:- op(400, xfy, +*+).

test_new_op_1 :- op(30, xfy, ++).
test_new_op_2 :- op(0, yfx, ++).
test_new_op_3 :- op(max, xfy, ++).
test_new_op_4 :- op(-30, xfy, ++).
test_new_op_5 :- op(1201, xfy, ++).
test_new_op_6(XFY) :- op(30, XFY, ++).
test_new_op_7 :- op(30, xfy, 0).
test_new_op_8 :- op(30, xfy, ++), op(40, xfx, ++).
test_new_op_9 :- op(30, xfy, ++), op(50, yf, ++).
test_new_op_10(Y) :- X = 5 +*+ 7,
                     Y = +*+(3, X, 2).

throws_exception(test_new_op_3).
throws_exception(test_new_op_4).
throws_exception(test_new_op_5).
throws_exception(test_new_op_6).
throws_exception(test_new_op_7).
%throws_exception(test_new_op_9). % Should throw an exception according to
                                  % 2nd draft of ISO Prolog Standard

% The following test isn't included because some SWI-Prolog versions support
% the 'yfy' specifier, while other versions consider it as an invalid one.
%
%test_new_op_X1 :- op(30, yfy, ++).


%
% current_op/3 test
% - ISO -
%

test_current_op_1(P, OP) :- current_op(P, xfy, OP).

