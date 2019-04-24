%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Additional file for testing 'errors_directives.bpl'

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%
% Lines of the lambda_cut/1 BPL directive errors
%

first_error(lambda_cut, 14).
number_errors(lambda_cut, 6).
error_in_line(Line) :- get_error_lines(lambda_cut, Line).


%
% Lines of the transitivity/1 BPL directive errors
%

first_error(transitivity, 31).
number_errors(transitivity, 4).
error_in_line(Line) :- get_error_lines(transitivity, Line).


%
% Lines of the fuzzy_rel/2 BPL directive errors
%

first_error(fuzzy_rel, 45).
number_errors(fuzzy_rel, 9).
error_in_line(Line) :- get_error_lines(fuzzy_rel, Line).


%
% Lines of the domain/4 BPL directive errors
%

first_error(domain, 62).
number_errors(domain, 15).
error_in_line(Line) :- get_error_lines(domain, Line).


%
% Lines of the fuzzy_set/2 BPL directive errors
%

first_error(fuzzy_set, 87).
number_errors(fuzzy_set, 18).
error_in_line(Line) :- get_error_lines(fuzzy_set, Line).


%
% Helper predicates
%

get_error_lines(Type, Line) :- first_error(Type, First),
                               number_errors(Type, Num),
                               Last is First + Num - 1,
                               between(First, Last, Line).

