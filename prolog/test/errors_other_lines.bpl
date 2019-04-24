%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Additional file for testing 'errors_comments.bpl'

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%
% Lines of the miscellaneous errors
%

first_error(misc, 13).
number_errors(misc, 13).
error_in_line(Line) :- get_error_lines(misc, Line).


%
% Lines of the errors related to unterminated strings
%

error_in_line(38).
error_in_line(40).


%
% Lines of the errors related to operators and priority clashes
%

first_error(operator, 60).
number_errors(operator, 7).
error_in_line(Line) :- get_error_lines(operator, Line).


%
% Line of the syntax error in end-of-file
%

error_in_line(73).


%
% Helper predicates
%

get_error_lines(Type, Line) :- first_error(Type, First),
                               number_errors(Type, Num),
                               Last is First + Num - 1,
                               between(First, Last, Line).

