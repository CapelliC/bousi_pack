%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Tests for evaluating weak unification algorithm a1
% bpl_unif_algorithm_a1.bpl

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- weak_unification(a1).
:- transitivity(no).

%
% List of tests
%

test_suite([test_a1_1,test_a1_2,test_a1_3,test_a1_4,test_a1_5]).

%
% Tests 
%

a~b=0.8.
b~c=0.75.

t(X):-a~b=X.

p(a,c).

p(X):-q(X),r(X).
q(c).
r(a).

/* 
 Incomplete Weak Unification Algorithm:
 p(X,X) fails
 p(b,b) succeeds
 p(X) fails
 p(b) succeeds
*/

test_a1_1 :-
  not(p(X,X)).

test_a1_2 :-
  p(b,b).
  
test_a1_3 :-
  not(p(_X)).
  
test_a1_4 :-
  p(b).

test_a1_5 :-
  t(X),
  X==0.8.
  
approximation_degree(test_a1_2, 0.75). 
approximation_degree(test_a1_4, 0.75). 
