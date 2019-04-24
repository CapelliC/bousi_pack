# running Bousi~Prolog as a pack
#
# CapelliC 2019
#

# SWIARCH,PACKSODIR,SOEXT expected to be set by pack_install
ifeq ($(SWIARCH),)
 SWIARCH = x86_64-linux
endif

ifeq ($(PACKSODIR),)
 PACKSODIR = lib/$(SWIARCH)
endif

ifeq ($(SOEXT),)
 SOEXT = so
endif

LIBRARYNAME = external
LIBRARY = $(LIBRARYNAME).$(SOEXT)

ifeq ($(CFLAGS),)
#seems pack_install doesn't get PKG_CONFIG_PATH ?
#CFLAGS =  -c -fPIC `pkg-config --cflags swipl` 
 CXFLAGS =  -c -fPIC -I$(HOME)/lib/swipl/include
else
 CXFLAGS =  -c $(CFLAGS)
endif

SWIPL_BIN_LIB = $(HOME)/lib/swipl/$(PACKSODIR)

LDFLAGS = -shared -fPIC -L$(SWIPL_BIN_LIB) -lswipl

SRC = ./extern
OBJ = ./build

all: $(LIBRARY)

OBJS = \
	$(OBJ)/closure.o \
	$(OBJ)/blocks.o \
	$(OBJ)/fuzzysets.o \
	$(OBJ)/array.o \
	$(OBJ)/lexical.o \
	$(OBJ)/tokenize.o \
	$(OBJ)/shell.o \
	$(OBJ)/install.o

$(OBJ)/closure.o: $(SRC)/closure.c $(SRC)/closure.h $(SRC)/array.h $(SRC)/bool.h
	$(CC) $(CXFLAGS) -o$@ $<

$(OBJ)/blocks.o: $(SRC)/blocks.c $(SRC)/array.h $(SRC)/bool.h $(SRC)/closure.h $(SRC)/blocks.h
	$(CC) $(CXFLAGS) -o$@ $<

$(OBJ)/fuzzysets.o: $(SRC)/fuzzysets.c $(SRC)/fuzzysets.h $(SRC)/array.h $(SRC)/bool.h
	$(CC) $(CXFLAGS) -o$@ $<

$(OBJ)/array.o: $(SRC)/array.c $(SRC)/array.h
	$(CC) $(CXFLAGS) -o$@ $<

$(OBJ)/lexical.o: $(SRC)/lexical.c $(SRC)/lexical.h $(SRC)/bool.h
	$(CC) $(CXFLAGS) -o$@ $<

$(OBJ)/tokenize.o: $(SRC)/tokenize.c $(SRC)/tokenize.h $(SRC)/array.h $(SRC)/lexical.h
	$(CC) $(CXFLAGS) -o$@ $<

$(OBJ)/shell.o: $(SRC)/shell.c $(SRC)/shell.h $(SRC)/bool.h
	$(CC) $(CXFLAGS) -o$@ $<

$(OBJ)/install.o: $(SRC)/install.c
	$(CC) $(CXFLAGS) -o$@ $<

$(LIBRARY): $(OBJS)
	swipl-ld $(LDFLAGS) -o $@ $^

# not sure it's required
check:

clean:
	rm -f $(OBJS) $(LIBRARY) $(SWIPL_BIN_LIB)/$(LIBRARY)

install: $(LIBRARY)
	cp $(LIBRARY) $(SWIPL_BIN_LIB)

.PHONY: all clean check
