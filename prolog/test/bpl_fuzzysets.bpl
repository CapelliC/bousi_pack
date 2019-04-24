%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Tests for evaluating fuzzy binary relations from fuzzy subsets

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%
% List of tests
%

test_suite([test_fuzzysets_1, test_fuzzysets_2, test_fuzzysets_3,
            test_fuzzysets_4, test_fuzzysets_5, test_fuzzysets_6,
            test_fuzzysets_7, test_fuzzysets_8, test_fuzzysets_9,
            test_fuzzysets_10, test_fuzzysets_11, test_fuzzysets_12,
            test_fuzzysets_13, test_fuzzysets_14, test_fuzzysets_15]).


%
% Additional queries
% (needed for adding new linguistic terms at runtime)
%

additional_queries(['person(paris, about#age#53)',
                    'person(robert, somewhat#middle)']).


%
% Definition of some linguistic variables
%

:- domain(age, 0, 100, years).
:- domain(temperature, -100, 100, celsius).

:- fuzzy_set(age, [young(0,0,10,30), middle(20,40,60), old(50,80,100,100)]).
:- fuzzy_set(temperature, [cold(-100,-100,-20,20), warm(10,20,30), hot(20,30,100,100)]).

age#60#75 ~ veteran = 0.5.
age#75#90 ~ veteran = 0.9.


%
% Clause database
%

person(martin, age#4).
person(john, age#21).
person(alice, age#55).
person(peter, age#90).

person(robert, old).
person(paris, middle).

current_temperature(sensor1, temperature#-50).
current_temperature(sensor2, temperature#15).

temperature#15(sensor3).
about#temperature#0#30(sensor4).
cold(sensor5).


%
% Tests
%

test_fuzzysets_1 :- old ~ age#80#90 =:= 1,       % in all these unifications,
                    old ~ about#age#89#91 =:= 1, % the second subset is
                    middle ~ age#40 =:= 1,       % completely included in the
                    young ~ about#age#5 =:= 1,   % first one
                    young ~ very#young =:= 1,
                    about#temperature#40 ~ temperature#40 =:= 1,
                    about#temperature#-30#50 ~ temperature#-30#50 =:= 1.

test_fuzzysets_2 :- age#80#90 ~ old < 1,       % in all these unifications,
                    about#age#89#91 ~ old < 1, % the second subset is NOT
                    age#40 ~ middle < 1,       % completely included in the
                    about#age#5 ~ young < 1,   % first one
                    very#young ~ young < 1,
                    temperature#40 ~ about#temperature#40 < 1,
                    temperature#0#50 ~ about#temperature#0#50 < 1,
                    temperature#-30#50 ~ about#temperature#-30#50 < 1.

test_fuzzysets_3 :- person(peter, old), not(person(peter, middle)).
test_fuzzysets_4 :- person(alice, middle).      % alice is 55 years old, not exactly middle
test_fuzzysets_5 :- person(john, middle).       % john is 21 years old, not exactly middle
test_fuzzysets_6 :- person(john, young).        % john is 21 years old, not exactly young
test_fuzzysets_7 :- not(person(john, age#22)).  % john is 21 years old
test_fuzzysets_8 :- person(alice, about#age#55). % john is 21 years old
test_fuzzysets_9 :- person(paris, middle).
test_fuzzysets_10 :- person(paris, age#40). % paris is middle, but her exact age is unknwon

test_fuzzysets_11 :- not(young ~ bad#50),
                     not(very#bad ~ young).

test_fuzzysets_12 :- current_temperature(X, cold), X == sensor1,
                     current_temperature(Y, warm), Y == sensor2,
                     \+(current_temperature(Y, hot)).

test_fuzzysets_13 :- temperature#15(X), X == sensor3.
test_fuzzysets_14 :- temperature#15(X), X == sensor4.
test_fuzzysets_15 :- warm(sensor3), warm(sensor4), warm(sensor5).

approximation_degree(test_fuzzysets_4, _). % less than 1
approximation_degree(test_fuzzysets_5, _). % less than 1
approximation_degree(test_fuzzysets_6, _). % less than 1
approximation_degree(test_fuzzysets_10, _). % less than 1
approximation_degree(test_fuzzysets_12, _). % less than 1
approximation_degree(test_fuzzysets_15, _). % less than 1

