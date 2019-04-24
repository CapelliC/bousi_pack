% DIRECTIVE
:- transitivity(yes).

% PROXIMITY EQUATIONS
wheat~bean=0.315.  bean~crop=0.315.
wheat~corn=0.315.  bean~child=0.33.
wheat~grass=0.315. bean~grass=0.315.
wheat~horse=0.315. bean~flower=0.315.
wheat~human=0.205. bean~horse=0.335.
bean~animal=0.35.  bean~potato=0.5.
bean~corn=0.48.    bean~table=0.35.

% FACTS and RULES
%% searchTerm(T,L1,L2), true (with approxima-
%% tion degree 1) if T is a (constant) term,
%% L1 is a list of (constant) terms (represen-
%% ting a text) and L2 is a list of triples
%% t(X,N,D); where X is a term similar to T
%% with degree D, which occurs N times in the
%% text L1
searchTerm(_T,[],[]).
searchTerm(T,[X|R],L):-
  T~X=AD, 
  !, 
  searchTerm(T,R,L1),
  insert(t(X,1,AD),L1,L).
searchTerm(T,[_X|R],L):-
  searchTerm(T,R,L).
  
insert(t(T,N,D),[],[t(T,N,D)]).
insert(t(T1,N1,D),[t(T2,N2,_)|R],[t(T1,N,D)|R]) :-
  T1 == T2, N is N1+N2.
insert(t(T1,N1,D),[t(T2,N2,D2)|R2],[t(T2,N2,D2)|R]) :-
  T1 \== T2, insert(t(T1,N1,D), R2, R).
  
% GOAL
g(T,L):-
  searchTerm(T,
    [agriculture,department,report,farm,
    own,reserve,national,average,price,
    loan,release,price,reserves,matured,
    bean,grain,enter,corn,sorghum,rates,
    bean,potato], L).
    
    