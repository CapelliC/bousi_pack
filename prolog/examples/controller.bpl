% CONTROLLER

:-domain(temperature,0,500,celsius).
:-fuzzy_set(temperature,
    [cold(0,0,110,165), 
     cool(110,165,220),
     normal(165,220,275), 
     warm(220,275,330),
     hot(275,330,500,500)]).

:-domain(pressure,0,300,kpa).
:-fuzzy_set(pressure,
    [weak(0,0,10,70), 
     low(10,70,130),
     ok(70,130,190), 
     strong(130,190,250),
     high(190,250,300,300)]).

:-domain(throttle,-60,60,rpm).
:-fuzzy_set(throttle,
    [neg_large(-60,-60,-45,-30),
     neg_medium(-45,-30,-15),
     neg_small(-30,-15,0), 
     zero(-15,0,15),
     pos_small(0,15,30), 
     pos_medium(15,30,45),
     pos_large(30,45,60,60)]).
     
%% Cold
throttle(positive_medium):-
  temperature(cold), pressure(low).
throttle(positive_small):-
  temperature(cold), pressure(ok).
throttle(negative_small):-
  temperature(cold), pressure(strong).
throttle(negative_medium):-
  temperature(cold), pressure(high).

%% Cool
throttle(positive_large):-
  temperature(cool), pressure(weak).
throttle(positive_medium):-
  temperature(cool), pressure(low).
throttle(zero):-
  temperature(cool), pressure(ok).
throttle(negative_medium):-
  temperature(cool), pressure(strong).
throttle(negative_medium):-
  temperature(cool), pressure(high).
     
%% Facts
temperature(temperature#300).
pressure(pressure#150).

%% GOAL EXAMPLES
% defuzzify(throttle(_),Y).
