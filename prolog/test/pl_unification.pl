%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Tests for unification operators

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%
% List of test suites
%

test_suites([test_unif, test_notunif]).


%
% =/2 test
% - ISO -
%

test_unif_1 :- '='(1, 1).
test_unif_2(X) :- '='(X, 1).
test_unif_3(X, Y) :- '='(X, Y).
test_unif_4 :- '='(_, _).
test_unif_5(X, Y) :- '='(X, Y), '='(X, abc).
test_unif_6(X, Y) :- '='(f(X, def), f(def, Y)).
test_unif_7 :- '='(1, 2).
test_unif_8 :- '='(1, 1.0).
test_unif_9(X) :- '='(g(X), f(f(X))).
test_unif_10(X) :- '='(f(X, 1), f(a(X))).
test_unif_11(X, Y) :- '='(f(X, Y, X), f(a(X), a(Y), Y, 2)).
test_unif_12(X) :- '='(X, a(X)). % Undefined behavior
test_unif_13(X) :- '='(f(X, 1), f(a(X), 2)). % Undefined behavior
test_unif_14(X) :- '='(f(1, X, 1), f(2, a(X), 2)). % Undefined behavior
test_unif_15(X) :- '='(f(1, X), f(2, a(X))). % Undefined behavior
test_unif_16(X, Y) :- '='(f(X, Y, X, 1), f(a(X), a(Y), Y, 2)). % Undefined behavior

test_unif_1b :- 1 = 1.
test_unif_2b(X) :- X = 1.
test_unif_3b(X, Y) :- X = Y.
test_unif_4b :- _ = _.
test_unif_5b(X, Y) :- X = Y, X = abc.
test_unif_6b(X, Y) :- f(X, def) = f(def, Y).
test_unif_7b :- 1 = 2.
test_unif_8b :- 1 = 1.0.
test_unif_9b(X) :- g(X) = f(f(X)).
test_unif_10b(X) :- f(X, 1) = f(a(X)).
test_unif_11b(X, Y) :- f(X, Y, X) = f(a(X), a(Y), Y, 2).
test_unif_12b(X) :- X = a(X). % Undefined behavior
test_unif_13b(X) :- f(X, 1) = f(a(X), 2). % Undefined behavior
test_unif_14b(X) :- f(1, X, 1) = f(2, a(X), 2). % Undefined behavior
test_unif_15b(X) :- f(1, X) = f(2, a(X)). % Undefined behavior
test_unif_16b(X, Y) :- f(X, Y, X, 1) = f(a(X), a(Y), Y, 2). % Undefined behavior


%
% \=/2 test
% - ISO -
%

test_notunif_1 :- '\\='(1, 1).
test_notunif_2(X) :- '\\='(X, 1).
test_notunif_3(X, Y) :- '\\='(X, Y).
test_notunif_4 :- '\\='(_, _).
test_notunif_5(X, Y) :- '\\='(X, Y), '\\='(X, abc).
test_notunif_6(X, Y) :- '\\='(f(X, def), f(def, Y)).
test_notunif_7 :- '\\='(1, 2).
test_notunif_8 :- '\\='(1, 1.0).
test_notunif_9(X) :- '\\='(g(X), f(f(X))).
test_notunif_10(X) :- '\\='(f(X, 1), f(a(X))).
test_notunif_11(X, Y) :- '\\='(f(X, Y, X), f(a(X), a(Y), Y, 2)).
test_notunif_12(X) :- '\\='(X, a(X)). % Undefined behavior
test_notunif_13(X) :- '\\='(f(X, 1), f(a(X), 2)). % Undefined behavior
test_notunif_14(X) :- '\\='(f(1, X, 1), f(2, a(X), 2)). % Undefined behavior
test_notunif_15(X) :- '\\='(f(1, X), f(2, a(X))). % Undefined behavior
test_notunif_16(X, Y) :- '\\='(f(X, Y, X, 1), f(a(X), a(Y), Y, 2)). % Undefined behavior

test_notunif_1b :- 1 \= 1.
test_notunif_2b(X) :- X \= 1.
test_notunif_3b(X, Y) :- X \= Y.
test_notunif_4b :- _ \= _.
test_notunif_5b(X, Y) :- X \= Y, X \= abc.
test_notunif_6b(X, Y) :- f(X, def) \= f(def, Y).
test_notunif_7b :- 1 \= 2.
test_notunif_8b :- 1 \= 1.0.
test_notunif_9b(X) :- g(X) \= f(f(X)).
test_notunif_10b(X) :- f(X, 1) \= f(a(X)).
test_notunif_11b(X, Y) :- f(X, Y, X) \= f(a(X), a(Y), Y, 2).
test_notunif_12b(X) :- X \= a(X). % Undefined behavior
test_notunif_13b(X) :- f(X, 1) \= f(a(X), 2). % Undefined behavior
test_notunif_14b(X) :- f(1, X, 1) \= f(2, a(X), 2). % Undefined behavior
test_notunif_15b(X) :- f(1, X) \= f(2, a(X)). % Undefined behavior
test_notunif_16b(X, Y) :- f(X, Y, X, 1) \= f(a(X), a(Y), Y, 2). % Undefined behavior

