%:-ext_block_equs(false).

:-domain(age,0,100,years).
:-fuzzy_set(age,[young(0,0,30,50),
    middle(20,40,60,80), old(50,80,100,100)]).
    
:-domain(speed,0,40,'km/h').
:-fuzzy_set(speed,[slow(0,0,15,20),
    normal(15,20,25,40), fast(25,30,40,40)]).

speed(X, fast) :- age(X, young).

age(robert, middle).

%% GOAL EXAMPLES
% speed(robert,somewhat#fast)
