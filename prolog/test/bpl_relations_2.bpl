%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Tests for evaluating reflexive, symmetric and/or transitive
% relations (II)

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%
% List of tests
%

test_suite([test_relations2_1, test_relations2_2, test_relations2_3,
            test_relations2_4, test_relations2_5, test_relations2_6,
            test_relations2_7, test_relations2_8]).


%
% Tests with a relation with no properties
%

:- fuzzy_rel(~1~, []).
a ~1~ b = 0.3.
b ~1~ c = 0.7.

test_relations2_1 :- not(W ~1~ W),
                     a ~1~ b =:= 0.3,
                     b ~1~ c =:= 0.7,
                     not(b ~1~ a),
                     not(c ~1~ b),
                     not(a ~1~ c),
                     not(c ~1~ a),
                     not(_X ~1~ _Y),
                     not(_Z ~1~ c).

test_relations2_2 :- not(c(c) ~1~ c(c)),
                     a(a) ~1~ b(b) =:= 0.3,
                     b(b) ~1~ c(c) =:= 0.7,
                     a(b) ~1~ b(c) =:= 0.3,
                     not(a(a) ~1~ c(c)),
                     not(a(a, W) ~1~ b(b, W)),
                     not(a(_X) ~1~ b(_Y)),
                     not(b(_Z) ~1~ b(c)).


%
% Tests with a reflexive and symmetric relation
%

:- fuzzy_rel(~2~, [reflexive, symmetric]).
a ~2~ b = 0.3.
b ~2~ c = 0.7.

test_relations2_3 :- (W ~2~ W =:= 1, var(W)),
                     a ~2~ b =:= 0.3,
                     b ~2~ c =:= 0.7,
                     b ~2~ a =:= 0.3,
                     c ~2~ b =:= 0.7,
                     not(a ~2~ c),
                     not(c ~2~ a),
                     not(_X ~2~ _Y),
                     not(_Z ~2~ c).

test_relations2_4 :- c(c) ~2~ c(c) =:= 1,
                     a(a) ~2~ b(b) =:= 0.3,
                     b(b) ~2~ c(c) =:= 0.7,
                     a(b) ~2~ b(c) =:= 0.3,
                     not(a(a) ~2~ c(c)),
                     a(a, W) ~2~ b(b, W) =:= 0.3,
                     not(a(_X) ~2~ b(_Y)),
                     not(b(_Z) ~2~ b(c)).


%
% Tests with a symmetric and transitive relation
%

:- fuzzy_rel(~3~, [symmetric, transitive]).
a ~3~ b = 0.3.
b ~3~ c = 0.7.

test_relations2_5 :- not(W ~3~ W),
                     a ~3~ b =:= 0.3,
                     b ~3~ c =:= 0.7,
                     b ~3~ a =:= 0.3,
                     c ~3~ b =:= 0.7,
                     a ~3~ c =:= 0.3,
                     c ~3~ a =:= 0.3,
                     not(_X ~3~ _Y),
                     not(_Z ~3~ c).

test_relations2_6 :- c(c) ~3~ c(c) =:= 0.7,
                     a(a) ~3~ b(b) =:= 0.3,
                     b(b) ~3~ c(c) =:= 0.7,
                     a(b) ~3~ b(c) =:= 0.3,
                     a(a) ~3~ c(c) =:= 0.3,
                     not(a(a, W) ~3~ b(b, W)),
                     not(a(_X) ~3~ b(_Y)),
                     not(b(_Z) ~3~ b(c)).


%
% Tests with another reflexive and symmetric relation
% (these tests use weak unifications, not term comparisons)
%

:- transitivity(no).
a ~ b = 0.3.
b ~ c = 0.7.

test_relations2_7 :- (W ~ W =:= 1, var(W)),
                     a ~ b =:= 0.3,
                     b ~ c =:= 0.7,
                     b ~ a =:= 0.3,
                     c ~ b =:= 0.7,
                     not(a ~ c),
                     not(c ~ a),
                     (a ~ X =:= 1, X == a),
                     (Y ~ c =:= 1, Y == c).

test_relations2_8 :- c(c) ~ c(c) =:= 1,
                     a(a) ~ b(b) =:= 0.3,
                     b(b) ~ c(c) =:= 0.7,
                     not(a(a) ~ c(c)),
                     (a(W) ~ a(c) =:= 1, W == c),
                     (a(X) ~ b(c) =:= 0.3, X == c),
                     (Y ~ b(c) =:= 1, Y == b(c)),
                     (c(a) ~ Z =:= 1, Z == c(a)).

