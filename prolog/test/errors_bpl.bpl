%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Tests for Bousi-Prolog specific errors (except directives)

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%
% Errors related to invalid Bousi-Prolog linguistic terms
%

:- domain(sample, 0, 100, none).

no(X) :- X ~ sample#40.5.
no(X) :- X ~ sample#10#20.5.

