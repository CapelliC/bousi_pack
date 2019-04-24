%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Tests for the weak unify and WSLD resolution algorithms

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%
% List of tests
%

test_suite([test_wsld_1, test_wsld_2, test_wsld_3, test_wsld_4,
            test_wsld_5, test_wsld_6, test_wsld_7, test_wsld_8,
            test_wsld_9, test_wsld_10, test_wsld_11, test_wsld_12,
            test_wsld_13, test_wsld_14, test_wsld_15, test_wsld_16,
            test_wsld_17]).


%
% Definition of a proximity relation
%

:- transitivity(no).
person ~ baby = 1.
person ~ young = 1.
person ~ middle = 1.
person ~ old = 1.
baby ~ young = 0.7.
young ~ middle = 0.5.
young ~ old = 0.1.
middle ~ old = 0.5.


%
% Clause database
%

:- dynamic baby/1.
:- dynamic young/1.
:- dynamic middle/1.
:- dynamic old/1.
:- dynamic person/1.

baby(lucy).
young(george).
young(charles).
old(mary).

can_vote(X) :- person(X), (middle(X) ; old(X)).

can_vote_2(person(X)) :- middle(X) ; old(X).


%
% Basic tests
%

test_wsld_1 :- (baby(X), X == lucy),
               (young(Y), Y == charles),
               (old(Z), Z == mary).

test_wsld_2 :- (young(X), baby(X), X == lucy). % lucy is young with 0.7

test_wsld_3 :- (young(X), baby(X), X == charles). % charles is baby with 0.7

test_wsld_4 :- (old(Y), middle(Y), young(Y), Y == charles). % charles is old with 0.1

test_wsld_5 :- (middle(Z), Z == george). % george is middle with 0.5

test_wsld_6 :- call((middle(Z), Z == george)). % call/1 ignores approx. degrees

approximation_degree(test_wsld_2, 0.7).
approximation_degree(test_wsld_3, 0.7).
approximation_degree(test_wsld_4, 0.1).
approximation_degree(test_wsld_5, 0.5).


%
% Tests with negations
%

test_wsld_7 :- not((baby(X), old(X))). % george & charles are baby with 0.7
                                       %  and old with 0.1; not is 1 - 0.1

test_wsld_8 :- \+((baby(X), old(X))). % george & charles are baby with 0.7
                                      %  and old with 0.1; \+ is 1

test_wsld_9 :- not(middle(george)). % george is middle with 0.5; not is 1 - 0.5

test_wsld_10 :- \+(middle(george)). % george is middle with 0.5; \+ is 1

test_wsld_11 :- not(not(baby(lucy))),
                \+(\+(baby(lucy))).

approximation_degree(test_wsld_7, 0.9).
approximation_degree(test_wsld_9, 0.5).


%
% Tests with findall/3
%

test_wsld_12 :- findall(X, person(X), L), sort(L, SL),
                SL = [charles, george, lucy, mary].

test_wsld_13 :- findall(X, can_vote(X), L), sort(L, SL),
                SL = [charles, george, mary].

test_wsld_14 :- findall(X, can_vote_2(X), L), sort(L, SL),
                SL = [person(charles), person(george), person(mary)].


%
% Tests with assertions and retractions
%

test_wsld_15 :- assert(middle(adrian)),
                young(adrian), % here adrian is young with 0.5
                retract(middle(adrian)).

test_wsld_16 :- assert(baby(mike)), assert(old(steve)),
                findall(X, can_vote(X), L), sort(L, SL),
                SL = [charles, george, mary, steve],
                retract(baby(mike)), retract(old(steve)).

test_wsld_17 :- retract(old(mary)),
                findall(X, person(X), L), sort(L, SL),
                SL = [charles, george, lucy],
                assert(old(mary)).

approximation_degree(test_wsld_15, 0.5).

