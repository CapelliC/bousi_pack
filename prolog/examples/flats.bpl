%% DIRECTIVES
%% Linguistic variable: rental
:-domain(rental,0,600,euros).
:-fuzzy_set(rental, [cheap(100,100,250,500),
                     normal(100,300,400,600),
                     expensive(300,450,600,600)]).
                     
%% Linguistic variable: distance
:-domain(distance,0,50,minutes).
:-fuzzy_set(distance,
   [close(0,0,15,40), 
    medial(15,25,30,35), 
    far(20,35,50,50)]).

%% Linguistic variable: flat conditions
:-domain(condition,0,10,conditions).
:-fuzzy_set(condition, 
   [unfair(0,0,1,3), fair(1,3,6), 
    good(4,6,8), excellent(7,9,10,10)]).

%% FACTS
%% Flats table
%% flat(Code, Street, Rental, Condition).
flat(f1, libertad_street, rental#300, more_or_less#good).
flat(f2, ciruela_street, rental#450, somewhat#good).
flat(f3, granja_street, rental#200, unfair).

%% Streets table
%% street(Name, District)
street(libertad_street,ronda_la_mata).
street(ciruela_street,downtwon).
street(granja_street,ronda_santa_maria).

%% Distance table
%% distance(District, District, Distance)
%% to university campus
distance(ronda_la_mata,campus,somewhat#close).
distance(downtwon,campus,medial).
distance(ronda_santa_maria,campus,far).

%% RULES
%% close_to(Flat, District)
close_to(Flat, District):-
  flat(Flat, Street, _, _),
  street(Street, Flat_Dist),
  distance(Flat_Dist, District, close).

select_flat(Flat, Street):-
  flat(Flat, Street, cheap, good),
  close_to(Flat,campus).
  
  
%% GOAL EXAMPLES
% select_flat(Flat, Street).
