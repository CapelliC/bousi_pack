# running Bousi~Prolog as a pack
#
# CapelliC 2019
# with special help by Jan Wielemaker
#

# SWIARCH,PACKSODIR,SOEXT expected to be set by pack_install
# defaults based on my Linux environment, where swipl was built by source

ifeq ($(SWIARCH),)
 SWIARCH = x86_64-linux
endif

ifeq ($(PACKSODIR),)
 PACKSODIR = $(HOME)/lib/swipl/lib/$(SWIARCH)
endif

ifeq ($(SOEXT),)
 SOEXT = so
endif

ifeq ($(LDSOFLAGS),)
 LDSOFLAGS = -shared -fPIC -L$(PACKSODIR) -lswipl
endif

# this is not mentioned by Jan
# https://swi-prolog.discourse.group/t/bpl-3-2-released/519/4
ifeq ($(CFLAGS),)
 CFLAGS = -c -fPIC -I$(HOME)/lib/swipl/include
endif

# note: was external.so
# also changed in foreign.pl and bousi_pack.pl
SOBJ = bousi_support.$(SOEXT)

all: $(SOBJ)

SRC = ./extern

OBJ = \
	$(SRC)/closure.o \
	$(SRC)/blocks.o \
	$(SRC)/fuzzysets.o \
	$(SRC)/array.o \
	$(SRC)/lexical.o \
	$(SRC)/tokenize.o \
	$(SRC)/shell.o \
	$(SRC)/install.o

$(SOBJ): $(OBJ)
	swipl-ld $(LDSOFLAGS) -o $@ $^

# not sure it's required
check:

clean:
	rm -f $(OBJ) $(SOBJ) $(PACKSODIR)/$(SOBJ)

install: $(SOBJ)
	mkdir -p $(PACKSODIR)
	cp $(SOBJ) $(PACKSODIR)

.PHONY: all clean check
