%
% Graphical display of graphs 
%
% Use:
%   display_graph(Graph), where Graph=[arc(v1,v2),...,arc(vn-1,vn)]
%
% Examples:
%   ?- findall(arc(X,Y),(wn_hypernyms(man,List),append(_,[X,Y|_],List)),Graph), display_graph(Graph).
%   ?- setof(arc(X,Y),List^H^T^(wn_hypernyms(man,List),append(H,[X,Y|T],List)),Graph), display_graph(Graph).

% Requires:
% - PDF displayer (as indicated in pdf_displayer/1 fact and accesible in the path).
% - dot (part of Graphviz, accesible in the path)
% - dot2tex (for generating a LaTeX version of the graph). If LaTeX output is enabled (disabled by default)

:- module(wn_display_graph,
	  [ display_graph/1
	  ]).

%%% 'PDFViewer' is a user defined environment variable that must be exported to
%%% be accesible by the son process that executes SWI-Prolog. It stores the command
%%% to launch the specific PDF viewer of the operating system we are using.
pdf_displayer(PDFViewer) :-
    getenv('PDFViewer', PDFViewer), %% Set by the user
    !.

pdf_displayer('open -a Preview') :-
    current_prolog_flag(apple, true), % MacOS system
    !.

pdf_displayer('xpdf') :-
    current_prolog_flag(unix, true), % Linux system
    !.

pdf_displayer('acrobat.exe /A "view=Fit"') :-
    current_prolog_flag(windows, true), % Windows system
    !.

:- if((getenv('OSTYPE',OSystem), OSystem = darwin16)).
    %% The system is MacOS
    pdf_displayer('open -a Preview').
:- elif((getenv('OSTYPE',OSystem), OSystem = linux-gnu)).
    %% The system is Linux GNU
    pdf_displayer('xpdf').
:- else.
    %% The system is Windows
    pdf_displayer('acrobat.exe /A "view=Fit"').
:- endif.

% display_graph(+Graph)
%   Graph is a list of arc(From,To)
%   Displays a PDF containing the graphical representation of Graph
%   Creates the files:
%   - out.dot: A file with the graph in DOT format (graph description language) 
%   - out.pdf: The PDF document with the graph representation
%   - out.tex: The LaTeX document with the graph representation. Disabled for now (just uncomment it below for enabling)
display_graph(Graph) :-
	open('out.dot', write, Handle),
	write(Handle, 'digraph G { size="1,1";'), 
	nl(Handle),
  write_arcs(Handle, Graph),
	write(Handle,'}'),
	close(Handle),
  % shell('dot2tex out.dot > out.tex'),
	display_dot_in_pdf.

display_dot_in_pdf :-
	(write('Displaying graph...'),
	 nl,
	 shell('dot out.dot -Tpdf -o out.pdf'),
	 pdf_displayer(PDFViewer),
	 atom_concat(PDFViewer, ' out.pdf', PDFViewerCommand),
     %% (shell(PDFViewerCommand) -> true ; true),
     (shell(PDFViewerCommand) -> true
        ; write('ERROR: Cannot start PDF viewer. Check the environment variable PDFViewer')
     ),
	 !
	 ;
	 write('ERROR: Cannot generate PDF output file. Check that the dot program is accesible')
    ).

write_arcs(_Handle,[]).
write_arcs(Handle,[arc(A,B)|R]):-
	write(Handle,A), 
	write(Handle,' -> '), 
	write(Handle,B), 
	write(Handle,';'), 
	nl(Handle), 
	write_arcs(Handle,R).
