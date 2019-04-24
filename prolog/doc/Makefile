###################################################################
# Makefile for creating Bousi-Prolog documentation (Unix version) #
###################################################################

# Note that PROLOG variable must be passed to "make" in order to build
# the documentation

# Source directory for Bousi-Prolog system
BOUSIDIR = ../source

# Name of the documentation generator's main module
DOCMODULE = builddoc

# LaTeX to PDF converter
PDFLATEX = pdflatex

# Documentation filenames
DOCBASE = doc
TEXDOC = $(DOCBASE).tex
PDFDOC = $(DOCBASE).pdf


all: $(PDFDOC)

clean:
	rm -f $(TEXDOC) $(PDFDOC) $(DOCBASE).aux $(DOCBASE).idx \
	 $(DOCBASE).log $(DOCBASE).out

$(PDFDOC): $(TEXDOC)
	$(PDFLATEX) $(TEXDOC)
	$(PDFLATEX) $(TEXDOC)

$(TEXDOC): $(BOUSIDIR)/*.pl $(DOCMODULE).pl
	$(PROLOG) -f $(DOCMODULE).pl -g "create_documentation('$(TEXDOC)'), halt."

