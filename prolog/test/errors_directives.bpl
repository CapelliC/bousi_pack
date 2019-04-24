%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Tests for showing errors related to BPL directives

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%
% lambda_cut/1 BPL directive test
%

:- lambda_cut(0).
:- lambda_cut(1).
:- lambda_cut(0.6).
:- lambda_cut(-0.1).    % Error, lambda < 0
:- lambda_cut(2.5).     % Error, lambda > 1
:- lambda_cut(foo).     % Error, lambda isn't a number
:- lambda_cut(X).       % Error, lambda isn't a number
:- lambda_cut([0.5]).   % Error, lambda isn't a number
:- lambda_cut(1, 2, 3). % Error, incorrect arity


%
% transitivity/1 BPL directive test
%

:- transitivity(no).
:- transitivity(yes).
:- transitivity(min).
:- transitivity(product).
:- transitivity(luka).
:- transitivity(bad).     % Error, invalid t-norm
:- transitivity(X).       % Error, invalid t-norm
:- transitivity([luka]).  % Error, invalid t-norm
:- transitivity(yes, no). % Error, incorrect arity


%
% fuzzy_rel/2 BPL directive test
%

:- fuzzy_rel(~>, [reflexive, transitive(min)]).
:- fuzzy_rel(~1~, [symmetric, transitive(yes), reflexive]).
:- fuzzy_rel(~2~, [reflexive, transitive(no)]).
:- fuzzy_rel(~3~, []).
:- fuzzy_rel(~, [reflexive]).              % Error, ~ properties can't be changed
:- fuzzy_rel(~>, [symmetric]).             % Error, invalid properties for ~>
:- fuzzy_rel(~1~, [reflexive, reflexive]). % Error, property repeated twice
:- fuzzy_rel(~1~, [reflexive, invalid]).   % Error, invalid property
:- fuzzy_rel(~1~, reflexive).              % Error, invalid property list
:- fuzzy_rel(~1~, X).                      % Error, invalid property list
:- fuzzy_rel(foo, [transitive]).           % Error, invalid relation operator
:- fuzzy_rel(X, [reflexive]).              % Error, invalid relation operator
:- fuzzy_rel(~2~, reflexive, symmetric).   % Error, incorrect arity


%
% domain/4 BPL directive test
%

:- domain(age, 0, 100, years).
:- domain(age_2, 0, 150, years).
:- domain(age, 0, 100, years).        % Error, domain already exists
:- domain(distance, 0, 1.5, km).      % Error, min isn't an integer number
:- domain(distance, 1.5, 4, km).      % Error, max isn't an integer number
:- domain(speed, 300, 0, kmh).        % Error, min > max
:- domain(speed, 100, 100, kmh).      % Error, min =:= max
:- domain(temperature, low, high, c). % Error, range isn't numeric
:- domain(temperature, 0, high, c).   % Error, range isn't numeric
:- domain(temperature, low, 150, c).  % Error, range isn't numeric
:- domain(speed(ok), 0, 200, kmh).    % Error, invalid domain name
:- domain(X, 0, 200, metres).         % Error, invalid domain name
:- domain(100, 0, 200, metres).       % Error, invalid domain name
:- domain(height, 0, 200, X).         % Error, invalid measure unit
:- domain(height, 0, 200, unit(m)).   % Error, invalid measure unit
:- domain(height, 0, 200, 100).       % Error, invalid measure unit
:- domain(height, [0, 200, metres]).  % Error, incorrect arity


%
% fuzzy_set/2 BPL directive test
%

:- fuzzy_set(age, [young(0,0,10,30), middle(20,45,60), old(50,70,100,100)]).
:- fuzzy_set(age_2, [young_2(0,0,10,30), middle_2(20,45,60)]).
:- fuzzy_set(age_2, [old_2(50,70,100,100)]).
:- fuzzy_set(age_2, []).
:- fuzzy_set(speed, [low(0,30,60), high(40,90,100)]). % Error, undefined domain
:- fuzzy_set(age, [baby(0,0.5,1)]).            % Error, not all points are integers
:- fuzzy_set(age, [baby(0,0.1,0.2,0.3)]).      % Error, not all points are integers
:- fuzzy_set(age, [bad1(60,30,0)]).            % Error, values aren't in ascending order
:- fuzzy_set(age, [bad2(10,20,a,40)]).         % Error, one point isn't numeric
:- fuzzy_set(age, [bad3(10,20,X,40)]) .        % Error, one point isn't numeric
:- fuzzy_set(age, [bad4(10,20)]).              % Error, unknown subset type
:- fuzzy_set(age, [bad4(10,20,30,40,50)]).     % Error, unknown subset type
:- fuzzy_set(age, [young(0,0,10,30)]).         % Error, subset already exists
:- fuzzy_set(age_2, [young(0,0,10,30)]).       % Error, subset already exists
:- fuzzy_set(X, [young(0,0,10,30)]).           % Error, invalid domain name
:- fuzzy_set(domain(age), [young(0,0,10,30)]). % Error, invalid domain name
:- fuzzy_set(100, [young(0,0,10,30)]).         % Error, invalid domain name
:- fuzzy_set(age, [baby]).                     % Error, invalid subset
:- fuzzy_set(age, [10]).                       % Error, invalid subset
:- fuzzy_set(age, [X]).                        % Error, invalid subset
:- fuzzy_set(age, foo).                        % Error, invalid subset ist
:- fuzzy_set(age, baby, 0, 0, 10).             % Error, incorrect arity

