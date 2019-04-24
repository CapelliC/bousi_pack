%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Tests for showing warnings

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%
% Warnings related to singleton variables
%

no_singleton(_).
no_singleton(_, _).
no_singleton(X, X).
no_singleton(_A, _B, _C).
no_singleton(A, B, C) :- write(A), call(B), (true -> fail ; call(C)).
no_singleton(R) :- A is 2 + 3, B is 3 + 2,
                   C is 5 + 9, R is A + B + C.
singleton(X).                             % Singleton: X
singleton(_X, _X).                        % Singleton-marked more than once: _X
singleton(A, B, C).                       % Singleton: A, B, C
singleton(A, B) :- write(A), true, !.     % Singleton: B
singleton(_A, _B) :- read(_A), write(_B). % Singleton-marked more than once: _A, _B
singleton(_A, B) :- write(_A).            % Several singleton warnings
singleton :- A is 2 + 3, B is 3 + 2,      % Singleton: R
             C is 5 + 9, R is A + B + C.


%
% Warnings related to clauses and goals with free variables
%

no_clause_vars(X) :- once(X).
no_clause_vars :- X =.. [true], Y is 2 + 3, (Y > 0 -> true ; ignore(X)).
no_goal_vars(X, Y) :- call((call(X), call(Y))).
no_goal_vars(L) :- findall(X, X, L), catch(_Goal, _Error, _Catcher).
clause_vars(X) :- X.                                          % X is a subclause
clause_vars :- X =.. [true], Y is 2 + 3, (Y > 0 -> true ; X). % X is a subclause
goal_vars(X, Y) :- call((X, call(Y))).                        % Y is a subgoal
goal_vars(X) :- call((call(X), _Y)).                          % _Y is a subgoal
goal_vars(L) :- findall((X, Y), (X, Y), L).                   % X and Y are subgoals


%
% Lines of the warnings related to singleton variables
%

first_warning(singleton, 18).
number_warnings(singleton, 7).
warning_in_line(Line) :- get_warning_lines(singleton, Line).


%
% Lines of the warnings related to clauses and goals with free variables
%

first_warning(subclauses_subgoals, 36).
number_warnings(subclauses_subgoals, 5).
warning_in_line(Line) :- get_warning_lines(subclauses_subgoals, Line).


%
% Helper predicates
%

get_warning_lines(Type, Line) :- first_warning(Type, First),
                                 number_warnings(Type, Num),
                                 Last is First + Num - 1,
                                 between(First, Last, Line).

