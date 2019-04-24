%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Tests for arithmetic predicates and operators

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%
% List of test suites
%

test_suites([test_is, test_arith, test_plus, test_minus, test_times,
             test_division, test_mod, test_floor, test_round, test_ceiling,
             test_truncate, test_floatarith, test_abs, test_sqrt,
             test_power]).


%
% is/2 test
% - ISO -
%

test_is_1(Result) :- 'is'(Result, 3 + 11.0).
test_is_2(X, Y) :- X = 1 + 2, Y is X * 3.
test_is_3 :- X = foo, 'is'(X, 77).
test_is_4(N) :- 'is'(77, N).

throws_exception(test_is_4).


%
% =:=/2, =\=/2, >/2, </2, >=/2 and =</2 tests
% - ISO -
%

test_arith_1 :- '=:='(0, 1).
test_arith_2 :- '=\\='(0, 1).
test_arith_3 :- '<'(0, 1).
test_arith_4 :- '>'(0, 1).
test_arith_5 :- '>='(0, 1).
test_arith_6 :- '=<'(0, 1).
test_arith_7 :- '=:='(1.0, 1).
test_arith_8 :- '=\\='(1.0, 1).
test_arith_9 :- '<'(1.0, 1).
test_arith_10 :- '>'(1.0, 1).
test_arith_11 :- '>='(1.0, 1).
test_arith_12 :- '=<'(1.0, 1).
test_arith_13 :- '=:='(3 * 2, 7 - 1).
test_arith_14 :- '=\\='(3 * 2, 7 - 1).
test_arith_15 :- '<'(3 * 2, 7 - 1).
test_arith_16 :- '>'(3 * 2, 7 - 1).
test_arith_17 :- '>='(3 * 2, 7 - 1).
test_arith_18 :- '=<'(3 * 2, 7 - 1).
test_arith_19(X) :- '=:='(X, 5).
test_arith_20(X) :- '=\\='(X, 5).
test_arith_21(X) :- '<'(X, 5).
test_arith_22(X) :- '>'(X, 5).
test_arith_23(X) :- '>='(X, 5).
test_arith_24(X) :- '=<'(X, 5).

throws_exception(test_arith_19).
throws_exception(test_arith_20).
throws_exception(test_arith_21).
throws_exception(test_arith_22).
throws_exception(test_arith_23).
throws_exception(test_arith_24).

test_arith_1b :- 0 =:= 1.
test_arith_2b :- 0 =\= 1.
test_arith_3b :- 0 < 1.
test_arith_4b :- 0 > 1.
test_arith_5b :- 0 >= 1.
test_arith_6b :- 0 =< 1.
test_arith_7b :- 1.0 =:= 1.
test_arith_8b :- 1.0 =\= 1.
test_arith_9b :- 1.0 < 1.
test_arith_10b :- 1.0 > 1.
test_arith_11b :- 1.0 >= 1.
test_arith_12b :- 1.0 =< 1.
test_arith_13b :- 3 * 2 =:= 7 - 1.
test_arith_14b :- 3 * 2 =\= 7 - 1.
test_arith_15b :- 3 * 2 < 7 - 1.
test_arith_16b :- 3 * 2 > 7 - 1.
test_arith_17b :- 3 * 2 >= 7 - 1.
test_arith_18b :- 3 * 2 =< 7 - 1.
test_arith_19b(X) :- X =:= 5.
test_arith_20b(X) :- X =\= 5.
test_arith_21b(X) :- X < 5.
test_arith_22b(X) :- X > 5.
test_arith_23b(X) :- X >= 5.
test_arith_24b(X) :- X =< 5.

throws_exception(test_arith_19b).
throws_exception(test_arith_20b).
throws_exception(test_arith_21b).
throws_exception(test_arith_22b).
throws_exception(test_arith_23b).
throws_exception(test_arith_24b).


%
% +/2 test
% - ISO -
%

test_plus_1(X) :- X is '+'(7, 35).
test_plus_2(X) :- X is '+'(0, 3 + 11).
test_plus_3(X) :- X is '+'(0, 3.2 + 11).
test_plus_4(X, N) :- X is '+'(77, N).
test_plus_5(X) :- Y = foo, X is '+'(Y, 77).

throws_exception(test_plus_4).
throws_exception(test_plus_5).

test_plus_1b(X) :- X is 7 + 35.
test_plus_2b(X) :- X is 0 + (3 + 11).
test_plus_3b(X) :- X is 0 + (3.2 + 11).
test_plus_4b(X, N) :- X is 77 + N.
test_plus_5b(X) :- Y = foo, X is Y + 77.

throws_exception(test_plus_4b).
throws_exception(test_plus_5b).

% The following tests aren't included because the 'max_integer' flag isn't
% available in all versions of SWI-Prolog, so the tests may fail or throw
% an exception depending on the installed version of SWI-Prolog
%
%test_plus_X1(MI, X) :- current_prolog_flag(max_integer, MI), X is '+'(MI, 1).
%test_plus_X1b(MI, X) :- current_prolog_flag(max_integer, MI), X is MI + 1.


%
% -/2 test
% - ISO -
%

test_minus_1(X) :- X is '-'(7).
test_minus_2(X) :- X is '-'(3 - 11).
test_minus_3(X) :- X is '-'(3.2 - 11).
test_minus_4(X, N) :- X is '-'(N).
test_minus_5(X) :- Y = foo, X is '-'(Y).
test_minus_6(X) :- X is '-'(7, 35).
test_minus_7(X) :- X is '-'(20, 3 + 11).
test_minus_8(X) :- X is '-'(0, 3.2 + 11).
test_minus_9(X, N) :- X is '-'(77, N).
test_minus_10(X) :- Y = foo, X is '-'(Y, 77).

throws_exception(test_minus_4).
throws_exception(test_minus_5).
throws_exception(test_minus_9).
throws_exception(test_minus_10).

test_minus_1b(X) :- X is -7.
test_minus_2b(X) :- X is -(3 - 11).
test_minus_3b(X) :- X is -(3.2 - 11).
test_minus_4b(X, N) :- X is -N.
test_minus_5b(X) :- Y = foo, X is -Y.
test_minus_6b(X) :- X is 7 - 35.
test_minus_7b(X) :- X is 20 - (3 + 11).
test_minus_8b(X) :- X is 0 - (3.2 + 11).
test_minus_9b(X, N) :- X is 77 - N.
test_minus_10b(X) :- Y = foo, X is Y - 77.

throws_exception(test_minus_4b).
throws_exception(test_minus_5b).
throws_exception(test_minus_9b).
throws_exception(test_minus_10b).

% The following tests aren't included because the 'max_integer' flag isn't
% available in all versions of SWI-Prolog, so the tests may fail or throw
% an exception depending on the installed version of SWI-Prolog
%
%test_minus_X1(MI, X) :- current_prolog_flag(max_integer, MI), X is '-'(-1, MI).
%test_minus_X1b(MI, X) :- current_prolog_flag(max_integer, MI), X is -1 - MI.


%
% */2 test
% - ISO -
%

test_times_1(X) :- X is '*'(7, 35).
test_times_2(X) :- X is '*'(0, 3 + 11).
test_times_3(X) :- X is '*'(1.5, 3.2 + 11).
test_times_4(X, N) :- X is '*'(77, N).
test_times_5(X) :- Y = foo, X is '*'(Y, 77).

throws_exception(test_times_4).
throws_exception(test_times_5).

test_times_1b(X) :- X is 7 * 35.
test_times_2b(X) :- X is 0 * (3 + 11).
test_times_3b(X) :- X is 1.5 * (3.2 + 11).
test_times_4b(X, N) :- X is 77 * N.
test_times_5b(X) :- Y = foo, X is Y * 77.

throws_exception(test_times_4b).
throws_exception(test_times_5b).

% The following tests aren't included because the 'max_integer' flag isn't
% available in all versions of SWI-Prolog, so the tests may fail or throw
% an exception depending on the installed version of SWI-Prolog
%
%test_times_X1(MI, X) :- current_prolog_flag(max_integer, MI), X is '*'(MI, 2).
%test_times_6b(MI, X) :- current_prolog_flag(max_integer, MI), X is MI * 2.


%
% '/'/2 test
% - ISO -
%

test_division_1(X) :- X is '/'(7, 35).
test_division_2(X) :- X is '/'(7.0, 35).
test_division_3(X) :- X is '/'(140, 3 + 11).
test_division_4(X) :- X is '/'(20.1644, 3.2 + 11).
test_division_5(X) :- X is '/'(7, -3). % Implementation dependent
test_division_6(X) :- X is '/'(-7, 3). % Implementation dependent
test_division_7(X, N) :- X is '/'(77, N).
test_division_8(X) :- Y = foo, X is '/'(Y, 77).
test_division_9(X) :- X is '/'(3, 0).

throws_exception(test_division_7).
throws_exception(test_division_8).
throws_exception(test_division_9).

test_division_1b(X) :- X is 7 / 35.
test_division_2b(X) :- X is 7.0 / 35.
test_division_3b(X) :- X is 140 / (3 + 11).
test_division_4b(X) :- X is 20.1644 / (3.2 + 11).
test_division_5b(X) :- X is 7 / -3. % Implementation dependent
test_division_6b(X) :- X is -7 / 3. % Implementation dependent
test_division_7b(X, N) :- X is 77 / N.
test_division_8b(X) :- Y = foo, X is Y / 77.
test_division_9b(X) :- X is 3 / 0.

throws_exception(test_division_7b).
throws_exception(test_division_8b).
throws_exception(test_division_9b).


%
% mod/2 test
% - ISO -
%

test_mod_1(X) :- X is mod(7, 3).
test_mod_2(X) :- X is mod(0, 3 + 11).
test_mod_3(X, N) :- X is mod(77, N).
test_mod_4(X) :- Y = foo, X is mod(Y, 77).
test_mod_5(X) :- X is mod(7.5, 2).
test_mod_6(X) :- X is mod(7, 0).
test_mod_7(X) :- X is mod(7, -2).

throws_exception(test_mod_3).
throws_exception(test_mod_4).
throws_exception(test_mod_5).
throws_exception(test_mod_6).
%throws_exception(test_mod_7). % Should throw an exception according to
                               % 2nd draft of ISO Prolog Standard


%
% floor/1 test
% - ISO -
%

test_floor_1(X) :- X is floor(7.4).
test_floor_2(X) :- X is floor(-0.4).


%
% round/1 test
% - ISO -
%

test_round_1(X) :- X is round(7.5).
test_round_2(X) :- X is round(7.6).
test_round_3(X) :- X is round(-0.6).
test_round_4(X, N) :- X is round(N).

throws_exception(test_round_4).


%
% ceiling/1 test
% - ISO -
%

test_ceiling_1(X) :- X is ceiling(-0.5).


%
% truncate/1 test
% - ISO -
%

test_truncate_1(X) :- X is truncate(-0.5).
test_truncate_2(X) :- Y = foo, X is truncate(Y).

throws_exception(test_truncate_2).


%
% float/1 test
% - ISO -
%

test_floatarith_1(X) :- X is float(7).
test_floatarith_2(X) :- X is float(7.3).
test_floatarith_3(X) :- X is float(5 / 3).
test_floatarith_4(X, N) :- X is float(N).
test_floatarith_5(X) :- Y = foo, X is float(Y).

throws_exception(test_floatarith_4).
throws_exception(test_floatarith_5).

% The following test isn't included because the 'max_integer' flag isn't
% available in all versions of SWI-Prolog, so the test may fail or throw
% an exception depending on the installed version of SWI-Prolog
%
%test_floatarith_X1(MI, R, X) :- current_prolog_flag(max_integer, MI),
%                                R is float(MI) * 2, X is floor(R).


%
% abs/1 test
% - ISO -
%

test_abs_1(X) :- X is abs(7).
test_abs_2(X) :- X is abs(3 - 11).
test_abs_3(X) :- X is abs(3.2 - 11.0).
test_abs_4(X, N) :- X is abs(N).
test_abs_5(X) :- Y = foo, X is abs(Y).

throws_exception(test_abs_4).
throws_exception(test_abs_5).


%
% sqrt/1 test
% - ISO -
%

test_sqrt_1(X) :- X is sqrt(0.0).
test_sqrt_2(X) :- X is sqrt(4.0).
test_sqrt_3(X) :- X is sqrt(0).
test_sqrt_4(X) :- X is sqrt(1.0).
test_sqrt_5(X, N) :- X is sqrt(N).
test_sqrt_6(X) :- Y = foo, X is sqrt(Y).
test_sqrt_7(X) :- X is sqrt(-1.0).

throws_exception(test_sqrt_5).
throws_exception(test_sqrt_6).
throws_exception(test_sqrt_7).


%
% **/2 test
% - ISO -
%

test_power_1(X) :- X is '**'(5, 3).
test_power_2(X) :- X is '**'(-5.0, 3).
test_power_3(X) :- X is '**'(5, -1).
test_power_4(X, N) :- X is '**'(77, N).
test_power_5(X) :- Y = foo, X is '**'(Y, 2).
test_power_6(X) :- X is '**'(5, 3.0).
test_power_7(X) :- X is '**'(0.0, 0).

throws_exception(test_power_4).
throws_exception(test_power_5).

test_power_1b(X) :- X is 5 ** 3.
test_power_2b(X) :- X is -5.0 ** 3.
test_power_3b(X) :- X is 5 ** -1.
test_power_4b(X, N) :- X is 77 ** N.
test_power_5b(X) :- Y = foo, X is Y ** 2.
test_power_6b(X) :- X is 5 ** 3.0.
test_power_7b(X) :- X is 0.0 ** 0.

throws_exception(test_power_4b).
throws_exception(test_power_5b).

