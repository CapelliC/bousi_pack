%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Tests for list-related predicates (non-ISO)

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%
% List of test suites
%

test_suites([test_member, test_append, test_length, test_reverse]).


%
% member/2 test
% - non-ISO -
%

test_member_1 :- member(c, [a, b, c]).
test_member_2 :- member(z, [a, b, c]).
test_member_3(X) :- member(X, [a, b, c]).
test_member_4 :- member(_, [a, b, c]).
test_member_5(X) :- member(X, [_, _, _]).
test_member_6 :- member([a, c], [[a, x], [a, c], [a, z]]).
test_member_7(L) :- member(1, L).

throws_exception(test_member_7). % Infinite solutions


%
% append/3 test
% - non-ISO -
%

test_append_1 :- append([a, b], [c, d], [a, b, c, d]).
test_append_2(X) :- append(X, [c, d], [a, b, c, d]).
test_append_3(X) :- append([a, b], X, [a, b, c, d]).
test_append_4(X, Y) :- append(X, Y, [a, b, c, d]).
test_append_5(Z) :- append([a, b], [c, d], Z).
test_append_6(X) :- append(X, [c, d], [_, _, c, d]).
test_append_7(Y) :- append([a, b], Y, [a, b, _, _]).
test_append_8(X) :- append([x, y], X, [a, b, c, d]).
test_append_9(X) :- append(X, [x, y], [a, b, c, d]).
test_append_10(X, Y) :- append(X, Y, [_, _, _, _]).
test_append_11(X, Y) :- append(X, Y, [_, _|[_, _]]).
test_append_12 :- append([], [], []).
test_append_13(X) :- append([1, 2], X, [1, 2]).
test_append_14(L) :- append([], [], L).
test_append_15(I1, I2) :- append([a, I1], [c, d], [a, b, I2, d]).
test_append_16(X, Y, Z) :- append(X, Y, Z).
test_append_17(X, Z) :- append(X, [1, 2], Z).

throws_exception(test_append_16). % Infinite solutions
throws_exception(test_append_17). % Infinite solutions


%
% length/2 test
% - non-ISO -
%

test_length_1 :- length([a, b, c, d], 4).
test_length_2(X) :- length([a, b, c, d], X).
test_length_3(L) :- length(L, 4).
test_length_4(T) :- length([a, b|T], 4).
test_length_5(L, X) :- length(L, X).
test_length_6 :- length([a, b], foo).
test_length_7 :- length([a, b], 2.5).
test_length_8 :- length(foo, 2).

throws_exception(test_length_5). % Infinite solutions
throws_exception(test_length_6).
throws_exception(test_length_7).
throws_exception(test_length_8).


%
% reverse/2 test
% - non-ISO -
%

test_reverse_1 :- reverse([a, b, c], [c, b, a]).
test_reverse_2(X) :- reverse(X, [c, b, a]).
test_reverse_3(X) :- reverse([a, b, c], X).
test_reverse_4(T) :- reverse([a, b|T], [c, b, a]).
test_reverse_5 :- reverse([], []).
test_reverse_6(X) :- reverse([], X).
test_reverse_7(I1, I2) :- reverse([I1, b, c], [a, b, I2]).
test_reverse_8(X) :- reverse(X, X).
test_reverse_9(X, Y) :- reverse(X, Y).

throws_exception(test_reverse_8). % Infinite solutions
throws_exception(test_reverse_9). % Infinite solutions

