%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Tests for predicates which create and decompose terms

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%
% List of test suites
%

test_suites([test_functor, test_arg, test_univ]).


%
% functor/3 test
% - ISO -
%

test_functor_1 :- functor(foo(a, b, c), foo, 3).
test_functor_2(X, Y) :- functor(foo(a, b, c), X, Y).
test_functor_3(X) :- functor(X, foo, 3).
test_functor_4(X) :- functor(X, foo, 0).
test_functor_5 :- functor(foo(a), foo, 2).
test_functor_6 :- functor(foo(a), fo, 1).
test_functor_7(X, Y) :- functor(1, X, Y).
test_functor_8(X) :- functor(X, 1.1, 0).
test_functor_9(F) :- functor(F, foo(a), 1).
test_functor_10 :- functor([_|_], '.', 2).
test_functor_11 :- functor([], [], 0).
test_functor_12(X, Y) :- functor(X, Y, 3).
test_functor_13(X, N) :- functor(X, foo, N).
test_functor_14(X) :- functor(X, foo, a).
test_functor_15(A, T, X) :- current_prolog_flag(max_arity, A), X is A + 1,
                            functor(T, foo, X).
test_functor_16(Minus_1, F) :- Minus_1 is 0 - 1, functor(F, foo, Minus_1).

throws_exception(test_functor_9). % Shouldn't throw an exception according
                                  % to 2nd draft of ISO Prolog Standard
throws_exception(test_functor_12).
throws_exception(test_functor_13).
throws_exception(test_functor_14).
throws_exception(test_functor_15).
throws_exception(test_functor_16).


%
% arg/3 test
% - ISO -
%

test_arg_1 :- arg(1, foo(a, b), a).
test_arg_2(X) :- arg(1, foo(a, b), X).
test_arg_3(X) :- arg(1, foo(X, b), a).
test_arg_4(X, Y) :- arg(1, foo(X, b), Y).
test_arg_5 :- arg(1, foo(a, b), b).
test_arg_6 :- arg(0, foo(a, b), foo).
test_arg_7(N) :- arg(3, foo(3, 4), N).
test_arg_8(X) :- arg(X, foo(a, b), a).
test_arg_9(X) :- arg(1, X, a).
test_arg_10(A) :- arg(0, atom, A).
test_arg_11(A) :- arg(0, 3, A).
test_arg_12(X) :- arg(1, foo(X), u(X)). % Undefined behavior

%throws_exception(test_arg_8). % Should throw an exception according
                               % to 2nd draft of ISO Prolog Standard
throws_exception(test_arg_9).
throws_exception(test_arg_10). 
throws_exception(test_arg_11).

%
% =../2 test
% - ISO -
%

test_univ_1 :- '=..'(foo(a, b), [foo, a, b]).
test_univ_2(X) :- '=..'(X, [foo, a, b]).
test_univ_3(L) :- '=..'(foo(a, b), L).
test_univ_4(X, Y) :- '=..'(foo(X, b), [foo, a, Y]).
test_univ_5 :- '=..'(1, [1]).
test_univ_6 :- '=..'(foo(a, b), [foo, b, a]).
test_univ_7(X, Y) :- '=..'(X, Y).
test_univ_8(X, Y) :- '=..'(X, [foo, a|Y]).
test_univ_9(X) :- '=..'(X, [foo|bar]).
test_univ_10(X, Foo) :- '=..'(X, [Foo, bar]).
test_univ_11(X) :- '=..'(X, [3, 1]).
test_univ_12(X) :- '=..'(X, [1.1, foo]).
test_univ_13(X) :- '=..'(X, [a(b), 1]).
test_univ_14(X) :- '=..'(X, 4).
test_univ_15(X) :- '=..'(f(X), [f, u(X)]). % Undefined behavior

throws_exception(test_univ_7).
throws_exception(test_univ_8).
throws_exception(test_univ_9).
throws_exception(test_univ_10).
throws_exception(test_univ_11).
throws_exception(test_univ_12).
throws_exception(test_univ_13).
throws_exception(test_univ_14).

test_univ_1b :- foo(a, b) =.. [foo, a, b].
test_univ_2b(X) :- X =.. [foo, a, b].
test_univ_3b(L) :- foo(a, b) =.. L.
test_univ_4b(X, Y) :- foo(X, b) =.. [foo, a, Y].
test_univ_5b :- 1 =.. [1].
test_univ_6b :- foo(a, b) =.. [foo, b, a].
test_univ_7b(X, Y) :- X =.. Y.
test_univ_8b(X, Y) :- X =.. [foo, a|Y].
test_univ_9b(X) :- X =.. [foo|bar].
test_univ_10b(X, Foo) :- X =.. [Foo, bar].
test_univ_11b(X) :- X =.. [3, 1].
test_univ_12b(X) :- X =.. [1.1, foo].
test_univ_13b(X) :- X =.. [a(b), 1].
test_univ_14b(X) :- X =.. 4.
test_univ_15b(X) :- f(X) =.. [f, u(X)]. % Undefined behavior

throws_exception(test_univ_7b).
throws_exception(test_univ_8b).
throws_exception(test_univ_9b).
throws_exception(test_univ_10b).
throws_exception(test_univ_11b).
throws_exception(test_univ_12b).
throws_exception(test_univ_13b).
throws_exception(test_univ_14b).

