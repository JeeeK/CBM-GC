#
# Makefile Super Garbage Collection
# 1985-2020 J. E. Klasek j klasek.at
#
# ACME cross-assembler environment
#

VERSION=1.1-rel8-1985-12-27-Original-2016

# disable default suffixes and rules
.SUFFIXES:

F=jk-gc

all: jk-gc.o supergc-1985corr.prg

$(F).o: $(F).asm
	acme -v3 -l $(F).l -r $(F).r $(F).asm

supergc-1985corr.prg: $(F).o
	ln -sf $< $@

clean:
	rm -f *.r *.l *.out *.o

