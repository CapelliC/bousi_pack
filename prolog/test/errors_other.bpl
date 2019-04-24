%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Tests for syntax errors

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%
% Miscellaneous errors
%

a([2, []]) :- b(-2, c :- d).
w(w(w(w(w(w(w(_))))))).
a(0) a(1).        % '.' missing
a([3).            % ']' missing
a(3]).            % '[' missing
a().              % empty argument list
ñ(2).             % unexpected character in input ('ñ')
a(5, ).           % incomplete fact
a(X) :- b(X.      % incomplete rule
:- b(X.           % incomplete directive
a(X) :- b :- c.   % malformed rule
a(X) :- X + * 3.  % operand expected (between '+' and '*')
a(X) :- X is 4 5. % operator expected (between '4' and '5')
write(foo).       % no permission to redefine a predefined predicate
not(foo).         % no permission to redefine a predefined predicate


%
% Errors related to unterminated strings
%

a('aaa "bbb" \'ccc\'').
a("aaa 'bbb' \"ccc\"").
a('aaa\
   bbb').
a("aaa\
   bbb").
a('end
.         % ^ ''' missing
a("end
.         % ^ '"' missing


%
% Errors related to operators and priority clashes
%

:- op(222, xfx, xfx).
:- op(222, xfy, xfy).
:- op(222, yfx, yfx).
:- op(222, fx, fx).
:- op(222, fy, fy).

ok :- 9 xfy 10 xfy fx 89.
ok :- fy fy fy 20.
ok :- 20 xfx 50 yfx 89.
ok :- 9 =:= 3 ; 8 =:= 4.
ok :- 16 is 2 ** (2 ** 2).
ok :- a , 'X' -> d ; - _ is 9 + w ; a =\= 2 ** 'X'.
bad :- 10 xfy xfy 20.      % Syntax error
bad :- true , , true.      % Syntax error
bad :- 10 fy 15 xfy 20.    % Syntax error
bad :- -5 xfx 20 xfx 80.   % Operator priority clash (xfx)
bad :- fx fx 10.           % Operator priority clash (fx)
bad :- 4 > 3 > 2.          % Operator priority clash (>)
bad :- (:- :- w).          % Operator priority clash (:-)


%
% Syntax error in end-of-file
%

bad_eof :-

