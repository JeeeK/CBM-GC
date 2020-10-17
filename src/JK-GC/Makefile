#
# Makefile Super Garbage Collection
# 1985-2020 J. E. Klasek j klasek.at
#
# ACME cross-assembler environment
#

VERSION=2.1-rel2-2019-02-15-New

# disable default suffixes and rules
.SUFFIXES:

F=jk-gc

all: jk-gc.o

$(F).o: $(F).asm
	acme -v3 -l $(F).l -r $(F).r $(F).asm

clean:
	rm -f *.r *.l *.out *.o

