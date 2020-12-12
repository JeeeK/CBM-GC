#
# Makefile Super Garbage Collection
# 1985-2020 J. E. Klasek j klasek.at
#
# ACME cross-assembler environment
#

VERSION=2.2-rel3-2020-12-12-New

# Start address SuperGC
START = $$C500


# disable default suffixes and rules
.SUFFIXES:

F=jk-gc

all: jk-gc-basic.o jk-gc-irqa.o jk-gc-irqe.o jk-gc.o


# Default version is now the IRQ hook variant with buffer at $A000 ...

$(F).o: $(F)-irqa.o
	cp -p $(F)-irqa.o $(F).o
	cp -p $(F)-irqa.l $(F).l
	cp -p $(F)-irqa.r $(F).r

# create all variants ...

# BASIC ROM copy in RAM patch hook and buffer from $E000 to $FFFF

$(F)-basic.o: $(F).asm debug.asm
	acme -Dstart='$(START)' -Dbasic_patch=1 -v3 -f cbm -o $(F)-basic.o -l $(F)-basic.l -r $(F)-basic.r $(F).asm

# IRQ hook with buffer from $E000 to $FFFF

$(F)-irqe.o: $(F).asm debug.asm
	acme  -Dstart='$(START)' -Dno_basic_rom_buffer=1 -v3 -f cbm -o $(F)-irqe.o -l $(F)-irqe.l -r $(F)-irqe.r $(F).asm

# IRQ hook with buffer from $A000 to $BFFF

$(F)-irqa.o: $(F).asm debug.asm
	acme  -Dstart='$(START)' -Dbasic_rom_buffer=1 -v3 -f cbm -o $(F)-irqa.o -l $(F)-irqa.l -r $(F)-irqa.r $(F).asm


clean:
	rm -f jk-gc-*.r jk-gc-*.l *.out jk-gc-*.o


# Source code checks

# any code differences between english and german version?
check:
	sed -e 's/;.*//' jk-gc.asm > 1 ;\
	sed -e 's/;.*//' jk-gc.de.asm > 2 ;\
	diff -wB 1 2 ;\
	rm -f 1 2


