ROOT = ../..

CFLAGS  =
LDFLAGS =

all: basic.so basic.doc

%.so: %.o
	gcc -shared $(LDFLAGS) -o $@ $<

%.o: %.c
	gcc -ggdb3 -Wall -I$(ROOT)/src $(CFLAGS) -fPIC -c $<

%.doc: %.c
	$(ROOT)/lib-src/make-docfile $< > $@
