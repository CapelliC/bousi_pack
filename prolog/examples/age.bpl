%% fuzzy_sets
:-domain(age,0,100,years).
:-fuzzy_set(age, [young(0,0,30,50),middle(20,40,60,80),old(50,80,100,100)]).
%% facts
age(john, young ).
age(mary, age#35 ).
age(paul, about#age#40 ).
age(warren, very#young ).
