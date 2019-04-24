%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Tests for the lambda_cut/1 BPL directive

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%
% List of tests
%

test_suite([test_lambda_1, test_lambda_2]).


%
% Definition of the lambda cut
%

:- lambda_cut(0.5).


%
% Definition of some fuzzy relations
%

:- domain(speed, 0, 200, kmh).
:- fuzzy_set(speed, [stopped(0,0,10,20), slow(0,0,30,60),
                     middle(40,50,60,80), fast(70,90,200,200)]).

:- transitivity(product).
white ~ yellow = 0.8.
yellow ~ light_green = 0.6.
light_green ~ blue = 0.4.

:- fuzzy_rel(~1~, [reflexive, symmetric, transitive(yes)]).
a ~1~ b = 0.7.
b ~1~ c = 0.5.
c ~1~ d = 0.3.

:- fuzzy_rel(~2~, []).
a ~2~ b = 0.8.
b ~2~ a = 0.3.


%
% Clause database
%

colour(sun, yellow).
light_green(light).
bicycle(slow).


%
% Tests
%

test_lambda_1 :- colour(sun, white),              % yellow ~ white = 0.8 >= 0.5
                 yellow(light),                   % light_green ~ yellow = 0.6 >= 0.5
                 bicycle(stopped),                % slow ~ stopped >= 0.5
                 sample(a, b) ~1~ sample(b, c),   % b ~1~ c = 0.5 >= 0.5
                 a ~2~ b.                         % a ~2~ b = 0.8 >= 0.5

test_lambda_2 :- \+(colour(sun, blue)),             % yellow ~ blue = 0.6 * 0.4 = 0.24 < 0.5
                 \+(white(light)),                  % light_green ~ white = 0.8 * 0.6 = 0.48 < 0.5
                 \+(bicycle(middle)),               % slow ~ middle < 0.5
                 \+(sample(a, b) ~1~ sample(c, d)), % b ~2~ d = 0.3 < 0.5
                 \+(b ~2~ a).                       % a ~2~ b = 0.8 < 0.5

approximation_degree(test_lambda_1, _).
approximation_degree(test_lambda_2, 0).

