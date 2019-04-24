%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Tests for evaluating reflexive, symmetric and/or transitive
% relations (I)

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%
% List of tests
%

test_suite([test_relations1_1, test_relations1_2, test_relations1_3,
            test_relations1_4, test_relations1_5, test_relations1_6,
            test_relations1_7, test_relations1_8, test_relations1_9,
            test_relations1_10]).


%
% Tests with a reflexive relation
%

:- fuzzy_rel(~1~, [reflexive]).
a ~1~ b = 0.3.
b ~1~ c = 0.7.

test_relations1_1 :- (W ~1~ W =:= 1, var(W)),
                     a ~1~ b =:= 0.3,
                     b ~1~ c =:= 0.7,
                     not(b ~1~ a),
                     not(c ~1~ b),
                     not(a ~1~ c),
                     not(c ~1~ a),
                     not(_X ~1~ _Y),
                     not(_Z ~1~ c).

test_relations1_2 :- c(c) ~1~ c(c) =:= 1,
                     a(a) ~1~ b(b) =:= 0.3,
                     b(b) ~1~ c(c) =:= 0.7,
                     a(b) ~1~ b(c) =:= 0.3,
                     not(a(a) ~1~ c(c)),
                     a(a, W) ~1~ b(b, W) =:= 0.3,
                     not(a(_X) ~1~ b(_Y)),
                     not(b(_Z) ~1~ b(c)).


%
% Tests with a symmetric relation
%

:- fuzzy_rel(~2~, [symmetric]).
a ~2~ b = 0.3.
b ~2~ c = 0.7.

test_relations1_3 :- not(W ~2~ W),
                     a ~2~ b =:= 0.3,
                     b ~2~ c =:= 0.7,
                     b ~2~ a =:= 0.3,
                     c ~2~ b =:= 0.7,
                     not(a ~2~ c),
                     not(c ~2~ a),
                     not(_X ~2~ _Y),
                     not(_Z ~2~ c).

test_relations1_4 :- not(c(c) ~2~ c(c)),
                     a(a) ~2~ b(b) =:= 0.3,
                     b(b) ~2~ c(c) =:= 0.7,
                     a(b) ~2~ b(c) =:= 0.3,
                     not(a(a) ~2~ c(c)),
                     not(a(a, W) ~2~ b(b, W)),
                     not(a(_X) ~2~ b(_Y)),
                     not(b(_Z) ~2~ b(c)).


%
% Tests with a transitive relation
%

:- fuzzy_rel(~3~, [transitive]).
a ~3~ b = 0.3.
b ~3~ c = 0.7.

test_relations1_5 :- not(W ~3~ W),
                     a ~3~ b =:= 0.3,
                     b ~3~ c =:= 0.7,
                     not(b ~3~ a),
                     not(c ~3~ b),
                     a ~3~ c =:= 0.3,
                     not(c ~3~ a),
                     not(_X ~1~ _Y),
                     not(_Z ~1~ c).

test_relations1_6 :- not(c(c) ~3~ c(c)),
                     a(a) ~3~ b(b) =:= 0.3,
                     b(b) ~3~ c(c) =:= 0.7,
                     a(b) ~3~ b(c) =:= 0.3,
                     a(a) ~3~ c(c) =:= 0.3,
                     not(a(a, W) ~3~ b(b, W)),
                     not(a(_X) ~3~ b(_Y)),
                     not(b(_Z) ~3~ b(c)).


%
% Tests with a reflexive and transitive relation
%

:- fuzzy_rel(~>, [reflexive, transitive]).
a ~> b = 0.3.
b ~> c = 0.7.

test_relations1_7 :- (W ~> W =:= 1, var(W)),
                     a ~> b =:= 0.3,
                     b ~> c =:= 0.7,
                     not(b ~> a),
                     not(c ~> b),
                     a ~> c =:= 0.3,
                     not(c ~> a),
                     not(_X ~> _Y),
                     not(_Z ~> c).

test_relations1_8 :- c(c) ~> c(c) =:= 1,
                     a(a) ~> b(b) =:= 0.3,
                     b(b) ~> c(c) =:= 0.7,
                     a(b) ~> b(c) =:= 0.3,
                     a(a) ~> c(c) =:= 0.3,
                     a(a, W) ~> b(b, W) =:= 0.3,
                     not(a(_X) ~> b(_Y)),
                     not(b(_Z) ~> b(c)).


%
% Tests with a reflexive and transitive relation
%

:- fuzzy_rel(<~, [transitive, reflexive]).
a <~ b = 0.3.
b <~ c = 0.7.

test_relations1_9 :- (W <~ W =:= 1, var(W)),
                     a <~ b =:= 0.3,
                     b <~ c =:= 0.7,
                     not(b <~ a),
                     not(c <~ b),
                     a <~ c =:= 0.3,
                     not(c <~ a),
                     not(_X <~ _Y),
                     not(_Z <~ c).

test_relations1_10 :- c(c) <~ c(c) =:= 1,
                      a(a) <~ b(b) =:= 0.3,
                      b(b) <~ c(c) =:= 0.7,
                      a(b) <~ b(c) =:= 0.3,
                      a(a) <~ c(c) =:= 0.3,
                      a(a, W) <~ b(b, W) =:= 0.3,
                      not(a(_X) <~ b(_Y)),
                      not(b(_Z) <~ b(c)).

