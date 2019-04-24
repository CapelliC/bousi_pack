%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Tests for evaluating relations with different t-norms

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%
% List of tests
%

test_suite([test_tnorms_1, test_tnorms_2, test_tnorms_3]).


%
% Tests with a product-transitive relation
%

:- transitivity(product).
a ~ b = 0.4.
b ~ c = 0.8.

test_tnorms_1 :- a ~ b =:= 0.4,
                 b ~ c =:= 0.8,
                 a ~ c =< 0.8,
                 a ~ c = D, R is 0.4 * 0.8, approx_equal(D, R),
                 c ~ a =< 0.8,
                 c ~ a = E, S is 0.4 * 0.8, approx_equal(E, S).


%
% Tests with a luka-transitive relation
%

:- fuzzy_rel(~>, [reflexive, transitive(luka)]).
a ~> b = 0.4.
b ~> c = 0.8.

test_tnorms_2 :- a ~> b =:= 0.4,
                 b ~> c =:= 0.8,
                 a ~> c =< 0.8,
                 a ~> c = D, M is 0.4 + 0.8 - 1, R is max(0, M), approx_equal(D, R),
                 not(c ~> a).


%
% Tests with a min-transitive relation
%

:- fuzzy_rel(~1~, [transitive(min)]).
a ~1~ b = 0.4.
b ~1~ c = 0.8.

test_tnorms_3 :- a ~1~ b =:= 0.4,
                 b ~1~ c =:= 0.8,
                 a ~1~ c =< 0.8,
                 a ~1~ c = D, R is min(0.4, 0.8), approx_equal(D, R),
                 not(c ~1~ a).


%
% Helper predicates
%

approx_equal(Res, Val) :- Res >= Val - 0.00001, Res =< Val + 0.00001.

