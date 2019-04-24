%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Tests for evaluating reflexive, symmetric and/or transitive
% relations (III)

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%
% List of tests
%

test_suite([test_relations3_1, test_relations3_2, test_relations3_3,
            test_relations3_4]).


%
% Tests with a reflexive, symmetric and transitive relation
%

:- fuzzy_rel(~1~, [reflexive, symmetric, transitive]).
a ~1~ b = 0.3.
b ~1~ c = 0.7.

test_relations3_1 :- (W ~1~ W =:= 1, var(W)),
                     a ~1~ b =:= 0.3,
                     b ~1~ c =:= 0.7,
                     b ~1~ a =:= 0.3,
                     c ~1~ b =:= 0.7,
                     a ~1~ c =:= 0.3,
                     c ~1~ a =:= 0.3,
                     not(_X ~1~ _Y),
                     not(_Z ~1~ c).

test_relations3_2 :- c(c) ~1~ c(c) =:= 1,
                     a(a) ~1~ b(b) =:= 0.3,
                     b(b) ~1~ c(c) =:= 0.7,
                     a(b) ~1~ b(c) =:= 0.3,
                     a(a) ~1~ c(c) =:= 0.3,
                     a(a, W) ~1~ b(b, W) =:= 0.3,
                     not(a(_X) ~1~ b(_Y)),
                     not(b(_Z) ~1~ b(c)).


%
% Tests with another reflexive, symmetric and transitive relation
% (these tests use weak unifications, not term comparisons)
%

:- transitivity(yes).
a ~ b = 0.3.
b ~ c = 0.7.

test_relations3_3 :- (W ~ W =:= 1, var(W)),
                     a ~ b =:= 0.3,
                     b ~ c =:= 0.7,
                     b ~ a =:= 0.3,
                     c ~ b =:= 0.7,
                     a ~ c =:= 0.3,
                     c ~ a =:= 0.3,
                     (a ~ X =:= 1, X == a),
                     (Y ~ c =:= 1, Y == c).

test_relations3_4 :- c(c) ~ c(c) =:= 1,
                     a(a) ~ b(b) =:= 0.3,
                     b(b) ~ c(c) =:= 0.7,
                     a(b) ~ b(c) =:= 0.3,
                     a(a) ~ c(c) =:= 0.3,
                     (a(W) ~ a(c) =:= 1, W == c),
                     (a(X) ~ b(c) =:= 0.3, X == c),
                     (Y ~ b(c) =:= 1, Y == b(c)),
                     (c(a) ~ Z =:= 1, Z == c(a)).

