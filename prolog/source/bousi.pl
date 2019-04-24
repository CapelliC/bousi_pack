%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Bousi-Prolog interactive system launcher

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- module(bousi, [
		main/0,
		exit/0
   ]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% References to modules in folders

% WordNet:
% Looks if wn modules are in the current directory:
wn_directory(Directory) :- 
  File='./wn/wn.pl',
  exists_file(File),
  absolute_file_name(File, Path),
  file_directory_name(Path, Directory),
  !.
  
% If not, look in the paths in the environment variable PATH
wn_directory(Directory) :- 
  user:file_search_path(path, Path),
  atomic_concat(Path,'/wn',Directory),
  atomic_concat(Directory,'/wn.pl',File),
  exists_file(File),
  !.
  
% If not found, issue and error, but the system can still be used 
% with non-WordNet features
wn_directory(_) :- 
  write('ERROR: wn directory not found. Access to WordNet is not \c
                available.'),
  nl.
  
:- (wn_directory(Directory)
    ->
     asserta(user:file_search_path(wn, Directory))
    ;
     true).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Modules loaded at start-up

:- use_module(bplShell).
:- use_module(foreign).
:- use_module(test).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Set double quotes for delimiting list of character codes,
% instead of the new SWI-Prolog behaviour to delimit strings
:- set_prolog_flag(double_quotes, codes).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Main predicate for launching the Bousi-Prolog system
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%% main
%
%     Initial predicate of the Bousi-Prolog system. This predicate
%     loads the foreign library, shows a welcome message and
%     initializes and launches the Bousi-Prolog command-line shell.
%

main :-
  set_host_safe,
	foreign:load_foreign_extension,
	flags:set_bpl_flag(continue('yes')),
	welcome_message, 
	bplShell:start_bpl_shell.

	
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Predicate for setting a host safe execution mode
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%% set_host_safe
%
%     Sets host safe execution mode if indicated by the file host_safe
%
	
set_host_safe :-
  exists_file('./../../host_safe'),
	!,
	flags:set_bpl_flag(host_safe('yes')).
set_host_safe :-
	flags:set_bpl_flag(host_safe('no')).
	

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Predicate for continuing the execution of the Bousi-Prolog system
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%% exit
%
%     Continues with Bousi-Prolog execution after a bk command.
%
	
exit :-
	flags:set_bpl_flag(continue('yes')),
	bplShell:bpl_shell_loop.
	

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Miscellaneous predicates
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%% welcome_message
%
%     Shows Bousi-Prolog initial welcome message.
%

welcome_message :-
	nl,
	write('                                                    Universidad de'), nl,
	write('|O)               |D)                            Castilla - La Mancha'), nl,
	write('|O)(O)\\U(S)|I| ~~ || |R (O) |L (O) (G|.     (Version 3.2 ~~ April 2019)'), nl,
	write('----------------------------------------------------------------------------'), nl,
	write('Welcome to Bousi~Prolog, a fuzzy logic programming system created by'), nl,
	write('Juan Gallardo-Casero and Pascual Julian-Iranzo. Fernando Saenz-Perez'), nl,
	write('(UCM) contributed to this version. This software is for research and'), nl, 
	write('educational purposes only, and it is distributed with NO WARRANTY.'), nl,
	write('Please visit our website for the latest news on Bousi~Prolog:'), nl,
	write('https://dectau.uclm.es/bousi-prolog'), nl,
	write('----------------------------------------------------------------------------'), nl,
	nl,
	write('----------------------------------------------------------------------------'), nl,
	write('< bousi_pack by CapelliC >'), nl,
	write('----------------------------------------------------------------------------'), nl,
	nl,
	write('----------------------------------------------------------------------------'), nl,
	write('--            Enter \'hp\' to get help on the available commands            --'), nl,
	write('----------------------------------------------------------------------------'), nl,
	nl.

