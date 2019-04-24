%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Tests for evaluating weak unification algorithm a2
% bpl_unif_algorithm_a2.bpl

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- weak_unification(a3).
:- transitivity(no).

%
% List of tests
%

test_suite([test_a3_1,test_a3_2,test_a3_3,test_a3_4,test_a3_5]).

%
% Tests 
%

a~b=0.8.
b~c=0.75.

n :- not(p(b,c)).

t(X):-a~b=X.

p(a,c).

p(X):-q(X),r(X).
q(c).
r(a).

/* 
 Complete Weak Unification Algorithm:
 p(X,X) fails
 p(b,b) fails
 p(X) fails
 p(b) fails
*/

test_a3_1 :-
  not(p(X,X)).

test_a3_2 :-
  not(p(b,b)).
  
test_a3_3 :-
  not(p(_X)).
  
test_a3_4 :-
  not(p(b)).

test_a3_5 :-
  t(X),
  X==0.8.
  
% approximation_degree(test_a3_2, 0.75). 
% approximation_degree(test_a3_4, 0.75). 
