/*  File:    bousi_pack.pl
    Author:  Carlo,,,
    Created: Apr 23 2019
    Purpose:
*/

:- module(bousi_pack, [bousi_pack/0]).

% :- initialization use_foreign_library(foreign(bousi_support)).
:- use_module('source/bousi.pl').

bousi_pack :-
	main.
